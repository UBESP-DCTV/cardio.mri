test_that("extract_fct_names works", {
  # setup
  funs <- "
  a <- function() {}
  b <- 2
  c<- function() {}
  d <-function() {}
  `%||%` <- function() {}
  "
  withr::local_file("funs.R")
  fs::file_exists("funs.R")
  readr::write_lines(funs, "funs.R")
  readr::read_lines("funs.R")
  # execution
  res <- extract_fct_names("funs.R")

  # expectation
  expect_equal(res, c("a", "c", "d", "%||%"))
})

test_that("optimal_n_cores works", {
  # setup
  available <- 1:16

  # eval
  res <- purrr::map_dbl(available, optimal_n_cores)

  # expectation
  expect_equal(res, c(1, 1, 1, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8, 8, 8, 8))
})
