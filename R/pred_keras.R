pred_keras <- function(
    modelpath,
    type = c("train", "val", "test"),
    records = NULL,
    on_cpu = TRUE,
    n_at_time = 12
) {
  if (on_cpu) set_cpu_only()

  nodes_names <- attr(
    targets::tar_read_raw(glue::glue("clinicKeras_{type}")),
    "dimnames"
  )[[1]]

  records <- if (is.null(records)) {
    usethis::ui_info("All records used")
    nodes_names
  } else {
    setdiff(records, nodes_names)
  }
  n <- length(records)
  stopifnot(n > 0)
  usethis::ui_todo("{n} records to evaluate")

  model <- keras::load_model_hdf5(modelpath, compile = FALSE)

  risk_scores <- records |>
    split(ceiling(seq_along(records)/n_at_time)) |>
    purrr::map(~{
      db <- list(
        input_s_cine   = subset_data("cine",  "1", type, .x),
        input_l2c_cine = subset_data("cine",  "2", type, .x),
        input_l3c_cine = subset_data("cine",  "3", type, .x),
        input_l4c_cine = subset_data("cine",  "4", type, .x),
        input_s_lge    = subset_data("lge",   "1", type, .x),
        input_l2c_lge  = subset_data("lge",   "2", type, .x),
        input_l3c_lge  = subset_data("lge",   "3", type, .x),
        input_l4c_lge  = subset_data("lge",   "4", type, .x),
        input_clinic   = subset_data("clinic", "", type, .x)
      )
      pred <- predict(model, db)
      rm(db)
      gc(FALSE)
      usethis::ui_done("Risk score for record {.x} done.")
      pred
    }) |>
    purrr::flatten_dbl()
  usethis::ui_done("All risk scores evaluated.")

  outcomes <- records |>
    purrr::map_df(~targets::tar_read_raw(.x)[["output"]])


  tibble::tibble(
    record = records,
    event = outcomes[["outcome"]],
    time = outcomes[["fup"]],
    risk_score = risk_scores
  )
}


set_cpu_only <- function() {
  Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
  reticulate::py_run_string('
# import os
# os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
# ')
}
