setup_batch_files <- function(
  batch_size = 4,
  type = c("train", "val"),
  folder = here::here("dev"),
  bfp_path = glue::glue("{batch_size}-batch_{type}_paths.rds")
) {
  checkmate::qassert(batch_size, "X1(0,)")
  type = match.arg(type)

  lge4 <- targets::tar_read_raw(glue::glue("lge4Keras_{type}"))
  nodes_name <- attr(lge4, "dimnames")[[1]]
  n <- length(nodes_name)
  batch_size <- min(batch_size, n)
  n_batches <- ceiling(n / batch_size)

  fs::dir_create(folder)
  batches_file_paths <- file.path(folder, bfp_path) |>
    path.expand() |>
    normalizePath(mustWork = FALSE)

  if (fs::file_exists(batches_file_paths)) {
    usethis::ui_info("Batches files already exists. We'll use them.")
    return(readr::read_rds(batches_file_paths))
  } else {
    usethis::ui_info("Batches files does not exists yet. We'll create them.")
    gen <- batch_generator(batch_size, type = type)

    purrr::map_chr(seq_len(n_batches), ~{
      file <- fs::file_temp(
        pattern = glue::glue("{type}_batch-{n_batches}-{.x}_"),
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
    qs::qread(
      batches_paths[[i]],
      nthreads = parallelly::availableCores("multicore", omit = 2)
    )
  }
}



batch_generator <- function(
    batch_size,
    type = c("train", "val")
) {
  # start iterator
  i <- 1

  # return an iterator function
  function() {
    np <- reticulate::import("numpy", convert = FALSE)

    current_last_id <- i + batch_size - 1

    nodes_name <- attr(
      targets::tar_read_raw(glue::glue("lge4Keras_{type}")),
      "dimnames"
    )[[1]]
    last_id <- length(nodes_name)

    # reset iterator if already seen all data
    if (current_last_id > last_id) i <<- 1

    # iterate current batch's rows
    rows <- i:min(current_last_id, last_id)
    records <- nodes_name[rows]

    # update to next iteration
    i <<- current_last_id + 1

    # find the outcome
    outcomes <- purrr::map_df(records, ~{
      targets::tar_read_raw(.x)[["output"]]
    })
    events <- as.integer(outcomes[["outcome"]])
    time <- outcomes[["fup"]]
    y_true <- list(
      array(events, dim = c(length(events), 1L)
      ),
      make_riskset(time)
    )
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
        input_l4c_lge = subset_data("lge", "4", type, records)
      ),
      y_true = y_true
    )
  }
}


subset_data <- function(
    cinelge = c("cine", "lge"),
    ch = c("1", "2", "3", "4"),
    type = c("train", "val"),
    records
) {
  cinelge <- match.arg(cinelge)
  ch <- match.arg(ch)
  type <- match.arg(type)

  db <- targets::tar_read_raw(glue::glue("{cinelge}{ch}Keras_{type}"))

  if (cinelge == "cine" && ch == 1) {
    db[records, , , , , drop = FALSE]
  } else if (cinelge == "lge" && ch != 1) {
    db[records, , , drop = FALSE]
  } else {
    db[records, , , , drop = FALSE]
  }
}
