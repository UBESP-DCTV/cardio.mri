get_keras_model_path <- function(model = c("covar", "no-covar")) {
  model <- match.arg(model)
  hdf5_name <- switch(model,
    covar = "run_20221213150533-32x6gte4events_model.hdf5",
    `no-covar` = "run_20221209151027-32x6gte4events_model.hdf5"
  )
  get_output_data_path(file.path("models", model)) |>
    list.files("^.*model.*\\.hdf5$", full.names = TRUE)
}
