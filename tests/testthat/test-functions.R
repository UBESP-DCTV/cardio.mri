test_that("get_info_from_filename works", {
  # setup
  pinco_pallino <- list(
    ch1_cine = "pinco_pallino-ch1-cine-t25-s14.avi",
    ch2_cine = "pinco_pallino-ch2-cine.avi",
    ch3_cine = "pinco_pallino-ch3-cine.avi",
    ch4_cine = "pinco_pallino-ch4-cine.avi",
    ch1_lge = "pinco_pallino-ch1-lge.avi",
    ch2_lge = "pinco_pallino-ch2-lge.png",
    ch3_lge = "pinco_pallino-ch3-lge.png",
    ch4_lge = "pinco_pallino-ch4-lge.png"
  )

  # execution
  info_ch1_cine <- get_info_from_filename(pinco_pallino[["ch1_cine"]])
  info_ch1_lge <- get_info_from_filename(pinco_pallino[["ch1_lge"]])
  info_ch2_lge <- get_info_from_filename(pinco_pallino[["ch2_lge"]])

  # expectations
  expect_list(info_ch1_cine)
  expect_equal(info_ch1_cine[["name"]], "pinco_pallino")
  expect_equal(info_ch1_cine[["ch"]], 1L)
  expect_equal(info_ch1_cine[["type"]], "cine")
  expect_equal(info_ch1_cine[["t_max"]], 25L)
  expect_equal(info_ch1_cine[["s_tot"]], 14L)
})


test_that("read_mri works on 2d (ch 2-3-4 / lge) images", {
  # setup
  correct_file <- file.path(
    Sys.getenv("PRJ_SHARED_PATH"),
    "Parziale con codici", "Parziale con codici",
    "Bonato Renata", "bonato_renata-ch4-lge.png"
  )

  # execution
  imported_image <- read_mri(correct_file)

  # expectation
  expect_array(imported_image, d = 2)
})


test_that("gray3draw_to_gray2dint works", {
  # setup
  original_matrix <- matrix(1:16, nrow = 4)
  raw_matrix <- original_matrix |>
    as.raw() |>
    matrix(nrow = 4)

  # execution
  back_to_int <- gray3draw_to_gray2dint(raw_matrix)

  # expectation
  expect_equal(back_to_int, original_matrix)

})
