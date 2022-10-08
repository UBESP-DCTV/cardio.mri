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
  expect_numeric(db[["generalita_peso_kg"]])
  expect_scalar_na(db[["generalita_peso_kg"]][[3]])
  expect_factor(db[["generalita_sesso"]], c("maschio", "femmina"))
  expect_logical(db[["generalita_dislipidemia"]])
})


test_that("compose outcome works", {
  # setup
  db <- targets::tar_read(tabular)

  # evaluate
  out <- compose_outcome(db)

  # expectations
  expect_tibble(out, ncols = 3)
})

test_that("compose outcome works", {
  # setup
  db <- targets::tar_read(mris_b381672b)
  out <- targets::tar_read(outcome) |>
    dplyr::mutate(
      name = paste(cognome, nome, sep = "_")
    )

  # evaluate
  matched <- match_mri_out(db, out)

  # expectations
  expect_list(matched)
  expect_tibble(matched[["out"]])

  matched[["out"]][["name"]] |>
    expect_equal(attributes(db[[1]])[["mri_info"]][["name"]])
})
