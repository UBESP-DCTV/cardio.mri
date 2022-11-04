test_that("pad_array works on 1d array", {
  # setup
  array_1d <- array(1:3)

  # eval
  array_1d_nopad <- pad_array(array_1d)
  array_1d_long1 <- pad_array(array_1d, 4)
  expected_long1 <- abind::abind(array_1d, -1)
  dimnames(expected_long1) <- dimnames(array_1d)

  # expectations
  expect_equal(array_1d_nopad, array_1d)
  expect_equal(array_1d_long1, expected_long1)
})

test_that("pad_array works on 2d array", {
  # setup
  array_2d <- array(1:4, dim = c(2, 2))

  # eval
  array_2d_nopad <- pad_array(array_2d)
  array_2d_long1 <- pad_array(array_2d, c(2, 3))
  expected_long1 <- abind::abind(array_2d, c(-1, -1), along = 2)
  dimnames(expected_long1) <- dimnames(array_2d)

  # expectations
  expect_equal(array_2d_nopad, array_2d)
  array_2d_long1 |>
    expect_equal(expected_long1)
})

test_that("pad_array works on 3d array", {
  # setup
  array_3d <- array(1:24, dim = c(2, 3, 4))

  # eval
  array_3d_nopad <- pad_array(array_3d)
  array_3d_long1 <- pad_array(array_3d, c(3, 3, 5))
  expected_long1 <- array_3d |>
    abind::abind(array(rep(-1, 12), c(3, 4)), along = 1) |>
    abind::abind(array(rep(-1, 9), c(3, 3)), along = 3)
  dimnames(expected_long1) <- dimnames(array_3d)

  # expectations
  expect_equal(array_3d_nopad, array_3d)
  array_3d_long1 |>
    expect_equal(expected_long1)
})
