setup_batch_files <- function(
  batch_size = 4,
  type = c("train", "val", "test"),
  random = FALSE,
  min_events_per_batch = max(2, ceiling(batch_size / 2)),
  steps_per_epoch = NULL,
  folder = here::here("dev")
) {
  checkmate::qassert(batch_size, "X1(0,)")
  type = match.arg(type)



  # separate events and censored subjects
  nodes_name <- attr(
    targets::tar_read_raw(glue::glue("clinicKeras_{type}")),
    "dimnames"
  )[[1]]

  n <- length(nodes_name)
  batch_size <- min(batch_size, n)
  n_batches <- ceiling(n / batch_size)
  bfp_path <- glue::glue("size-{batch_size}_batches_{type}_paths.rds")

  if (random) {
    usethis::ui_todo("Setup random batches")
    usethis::ui_info("Batch size: {usethis::ui_value(batch_size)}")

    if (!is.null(steps_per_epoch)) {
      n_batches <- steps_per_epoch
    }
    usethis::ui_info("N batches: {usethis::ui_value(n_batches)}")

    bfp_path <- glue::glue("{n_batches}-random_{bfp_path}")
    usethis::ui_info("bfp_path: {usethis::ui_value(bfp_path)}")

    usethis::ui_todo("Defining events and censors for batches")
    are_event_nodes <- purrr::map_lgl(nodes_name, ~{
      targets::tar_read_raw(.x)[["output"]][["outcome"]]
    })
    event_nodes_names <- nodes_name[are_event_nodes]
    censored_nodes_names <- nodes_name[!are_event_nodes]
    usethis::ui_done("Events and censors for batches defined.")
    nodes_name <- list(
      event_nodes_names = event_nodes_names,
      censored_nodes_names = censored_nodes_names
    )
    min_events_per_batch <- min(
      min_events_per_batch, batch_size, length(event_nodes_names)
    )
    usethis::ui_info(
      "Min events/batch: {usethis::ui_value(min_events_per_batch)}"
    )
  } else {
    usethis::ui_todo("Setup regular batches")
    usethis::ui_info("Batch size: {usethis::ui_value(batch_size)}")
    usethis::ui_info("N batches: {usethis::ui_value(n_batches)}")
    usethis::ui_info("bfp_path: {usethis::ui_value(bfp_path)}")
  }




  fs::dir_create(folder)
  batches_file_paths <- file.path(folder, bfp_path) |>
    path.expand() |>
    normalizePath(mustWork = FALSE)

  if (fs::file_exists(batches_file_paths)) {
    usethis::ui_info("Batches files already exists. We'll use them.")
    return(readr::read_rds(batches_file_paths))
  } else {
    usethis::ui_info("Batches files does not exists yet. We'll create them.")
    gen <- batch_generator(
      nodes_name = nodes_name,
      batch_size = batch_size,
      type = type,
      random = random,
      min_events_per_batch = min_events_per_batch,
      steps_per_epoch = n_batches
    )

    purrr::map_chr(seq_len(n_batches), ~{
      tag_random <- if (random) "random_" else ""
      file <- fs::file_temp(
        pattern = glue::glue(
          "{tag_random}{type}_batch-{n_batches}-{.x}_"
        ),
        tmp_dir = here::here(folder, "batches") |> fs::dir_create(),
        ext = "qs"
      )
      qs::qsave(gen(), file, preset = "fast")
      usethis::ui_done(
        "({.x}/{n_batches}): {usethis::ui_value(file)} written on disk."
      )
      file
    }) |>
      readr::write_rds(batches_file_paths)
  }
}


create_batch_generator <- function(batches_paths) {
  # start iterator
  i <- 1

  # return an iterator function
  function() {
    # reset iterator if already seen all data
    if (i > length(batches_paths)) i <<- 1

    # return current batch
    res <- qs::qread(
      batches_paths[[i]],
      nthreads = parallelly::availableCores("multicore", omit = 2)
    )

    i <<- i + 1
    res
  }
}



batch_generator <- function(
    nodes_name,
    batch_size,
    type = c("train", "val", "test"),
    random = FALSE,
    min_events_per_batch = max(2, ceiling(batch_size / 2)),
    steps_per_epoch = NULL
) {

  last_id <- if (random) {
    n_batches <- if (is.null(steps_per_epoch)) {
      ceiling(length(unlist(nodes_name)) / batch_size)
    } else {
      steps_per_epoch
    }
    batch_size * n_batches
  } else {
    length(nodes_name)
  }

  # start iterator and clean up iterator container environment
  i <- 1
  gc()

  # return an iterator function
  function() {
    np <- reticulate::import("numpy", convert = FALSE)

    # reset iterator if already seen all data
    current_last_id <- i + batch_size - 1
    if (current_last_id > last_id) i <<- 1

    records <- if (random) {
      event_nodes_names <- nodes_name[["event_nodes_names"]]
      censored_nodes_names <- nodes_name[["censored_nodes_names"]]

      event_sampled <- sample(event_nodes_names, min_events_per_batch)
      other_sampled <- setdiff(event_nodes_names, event_sampled) |>
        c(censored_nodes_names) |>
        sample(batch_size - length(event_sampled))

      c(event_sampled, other_sampled) |>
        sample(batch_size)
    } else {
      # iterate current batch's rows
      rows <- i:min(current_last_id, last_id)
      records <- nodes_name[rows]
    }


    # update to next iteration
    i <<- current_last_id + 1

    # find the outcome
    outcomes <- purrr::map_df(records, ~{
      targets::tar_read_raw(.x)[["output"]]
    })
    # mark -1 as censored
    events <- (as.integer(outcomes[["outcome"]]) * 2) - 1
    time <- outcomes[["fup"]]
    y_true <- array(events * time, dim = c(length(events), 1L))
    rm(outcomes, events, time)

    # return the batch
    list(
      x = list(
        input_s_cine = subset_data("cine", "1", type, records),
        input_l2c_cine = subset_data("cine", "2", type, records),
        input_l3c_cine = subset_data("cine", "3", type, records),
        input_l4c_cine = subset_data("cine", "4", type, records),
        input_s_lge = subset_data("lge", "1", type, records),
        input_l2c_lge = subset_data("lge", "2", type, records),
        input_l3c_lge = subset_data("lge", "3", type, records),
        input_l4c_lge = subset_data("lge", "4", type, records),
        input_clinic = subset_data("clinic", "", type, records)
      ),
      y_true = y_true
    )
  }
}


subset_data <- function(
    cinelge = c("cine", "lge", "clinic"),
    ch = c("1", "2", "3", "4", ""),
    type = c("train", "val", "test"),
    records
) {
  cinelge <- match.arg(cinelge)
  stopifnot(length(ch) == 1, ch %in% c("1", "2", "3", "4", ""))
  type <- match.arg(type)


  db <- targets::tar_read_raw(glue::glue("{cinelge}{ch}Keras_{type}"))

  if (cinelge == "clinic") {
    db[records, , drop = FALSE]
  } else if (cinelge == "cine" && ch == 1) {
    db[records, , , , , drop = FALSE]
  } else if (cinelge == "lge" && ch != 1) {
    db[records, , , drop = FALSE]
  } else {
    db[records, , , , drop = FALSE]
  }
}
