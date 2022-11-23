test_that("multiplication works", {
  skip_if_not_py_pkg("numpy")
  skip_if_not_py_pkg("tensorflow")

  # setup

  # evaluation

  # expectaions
  skip("just a template")
})


test_that("safe_normalize works", {
  skip_if_not_py_pkg("tensorflow")

  # setup
  ko_vars <- array(1:10, dim = c(1, 10))
  ko_sbj <- array(1:20, dim = c(10, 2))
  ok_sbj <- array(1:10, dim = c(10, 1))
  to_adjust <- array(-1:8, dim = c(10, 1))


  # evaluation
  ok_normalized <- safe_normalize(ok_sbj)
  ok_adjusted <- safe_normalize(to_adjust)

  # expectaions
  expect_error(safe_normalize(ko_vars), regexp = "single column array")
  expect_error(safe_normalize(ko_sbj), regexp = "single column array")
})
