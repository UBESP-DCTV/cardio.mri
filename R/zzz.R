# global reference to scipy (will be initialized in .onLoad)
np <- NULL
tf <- NULL

.onLoad <- function(libname, pkgname) {
  # use superassignment to update global reference to scipy
  np <<- reticulate::import("numpy", convert = FALSE, delay_load = TRUE)
  tf <<- reticulate::import(
    "tensorflow", convert = FALSE, delay_load = TRUE
  )
}
