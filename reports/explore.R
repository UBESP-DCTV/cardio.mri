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

b <- match_mri_out(a, out)

a |> str(1)
