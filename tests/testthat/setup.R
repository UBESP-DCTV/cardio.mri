# the following code is running at the beginning of every tests.
library(checkmate)

# helper function to skip tests if we don't have the 'foo' module
skip_if_not_py_pkg <- function(pkg) {
  msg <- paste0("Py module ", pkg, "is not available for testing")
  testthat::skip_if_not(reticulate::py_module_available(pkg), msg)
}
