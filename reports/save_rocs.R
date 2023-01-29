c(
  "valRoc1Covar",
  "valRoc2Covar",
  "valRoc3Covar",
  "valRoc5Covar",
  "valRoc8Covar",
  "testRoc1Covar",
  "testRoc2Covar",
  "testRoc3Covar",
  "testRoc5Covar",
  "testRoc8Covar"
) |>
  purrr::set_names() |>
  purrr::map(~tar_read_raw(.x)[[20]]) |>
  purrr::iwalk(
    ~ggplot2::ggsave(
      paste0("roc30_", .y, ".jpg"),
      .x,
      width = 16,
      height = 9
    )
  )

