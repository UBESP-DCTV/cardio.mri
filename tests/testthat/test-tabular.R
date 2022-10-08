test_that("get_info_from_filename works", {
  # setup
  data_path <- targets::tar_read(tabularPath)

  # execution
  db <- read_tabular(data_path)

  # expectations
  expect_tibble(db)
  names(db) |>
    expect_names(
      must.include = c(
        "generalita_cognome", "generalita_nome",
        "generalita_data_di_nascita"
      )
    )
})


test_that("correct data type imported", {
  # setup
  data_path <- targets::tar_read(tabularPath)

  # execution
  db <- read_tabular(data_path)

  # expectations  expect_tibble(db)
  expect_character(db[["generalita_cognome"]])
  expect_character(db[["generalita_nome"]])
  expect_date(db[["generalita_data_di_nascita"]])
})
