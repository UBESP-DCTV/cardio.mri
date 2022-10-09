## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------

library(targets)
library(here)

# load all your custom functions
list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


# Code here below -------------------------------------------------
# use `tar_read(target_name)` to load a target anywhere (note that
# `target_name` is NOT quoted!)

# library(keras)
#
# dim(train_images) # NxLxH
# dim(train_labels) # N

ex1 <- tar_read(patientsMrisPaths)[[1]]
str(read_mri(ex1), 1L)

a <- tar_read(mris_b381672b)

str(a, 1)
attributes(a[[1]])


attributes(a[[1]])


out <- tar_read(outcome)
View(out)

present <- out[[1]] |>
  stringr::str_replace_all("_", " ") |>
  stringr::str_to_title() |>
  sort()


missed <- warns[[2]][-nrow(warns)] |>
  stringr::str_remove("No match for name ") |>
  stringr::str_replace_all("_", " ") |>
  stringr::str_to_title() |>
  sort()


purrr::map_lgl(missed |> purrr::set_names(), ~{
  any(
    stringr::str_detect(present, .x) |
    stringr::str_detect(.x, present)
  )
})

b <- match_mri_out(a, out)

a |> str(1)


matched_ccfdde5d
matched_fe9007f4
matched_b2f6cd69
matched_2b911744
matched_ae05b3e0
matched_0d4fd41a
matched_713aefa1
matched_971deff0
matched_3bbb77c6
matched_89cf6d47
