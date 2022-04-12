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
  expect_equal(info_ch1_cine[["name"]], "pinco_pallino")
  expect_equal(info_ch1_cine[["ch"]], 1)
  expect_equal(info_ch1_cine[["type"]], "cine")
  expect_equal(info_ch1_cine[["t_max"]], 25)
  expect_equal(info_ch1_cine[["s_tot"]], 14)
})
