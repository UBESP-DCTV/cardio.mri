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
  dimnames(expected_long1) <- NULL

  # expectations
  expect_equal(array_3d_nopad, array_3d)
  array_3d_long1 |>
    expect_equal(expected_long1)
})


test_that("merge_cine_1 works", {
  # setup
  dim_a <- c(2, 3, 4, 5)
  a <- array(sample.int(prod(dim_a), replace = TRUE), dim = dim_a)

  dim_b <- c(2, 3, 3, 6)
  b <- array(sample.int(prod(dim_b), replace = TRUE), dim = dim_b)

  base_dim <- c(2, 3, 5, 7)

  mt <- targets::tar_read(matched, branches = c(2, 3)) |>
    purrr::transpose() |>
    purrr::chuck("cine-1")



  # eval
  pad_a <- pad_array(a, base_dim)
  res <- merge_cine_short(list(a = a, b = b), dims = base_dim)

  mt_first_pad <- pad_array(mt[[1]], out_dims = c(640, 480, 30, 25))
  res_mt <- merge_cine_short(mt)


  # expected
  res |>
    expect_array(
      "integer",
      any.missing = FALSE,
      d = 5
    )
  expect_equal(dim(res), c(2, base_dim))
  expect_equal(res[1, , , , ], pad_a)

  expect_equal(res_mt[1, , , , ], mt_first_pad, ignore_attr = TRUE)
})
