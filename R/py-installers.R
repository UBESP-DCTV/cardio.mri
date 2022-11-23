install_numpy <- function(
    envname = NULL, method = "auto", conda = "auto"
) {
  reticulate::py_install(
    "numpy", envname = envname, method = method, conda = conda
  )
}

install_tensorflow <- function(
    envname = NULL, method = "auto", conda = "auto"
) {
  reticulate::py_install(
    "tensorflow", envname = envname, method = method, conda = conda
  )
}

install_keras <- function(
    envname = NULL, method = "auto", conda = "auto"
) {
  reticulate::py_install(
    "keras", envname = envname, method = method, conda = conda
  )
}
