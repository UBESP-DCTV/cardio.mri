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

a <- tar_read(mris_b381672b)

mt <- tar_read(matched)


mt |> str(1)


to_retain <- purrr::map_lgl(mt, ~length(.x) == 10)
mt_used <- mt[to_retain]

mt_fields <- purrr::transpose(mt_used)
str(mt_fields[[1]], 1)

mt_fields[[1]] |>
  purrr::map(~{
    dim(.x)
  })


max_dim <- mt_fields[[1]] |>
   purrr::map(dim) |>
   unlist() |>
   matrix(nrow = 4) |>
   apply(1, max)

zeros <- zeros <- array(0L, dim = max_dim)

mt_padded <- mt_fields[[1]] |>
  purrr::map(~{
    dims <- dim(.x)
    idx <- purrr::map(dims, seq_len)
    zeros[idx[[1]], idx[[2]], idx[[3]], idx[[4]]] <- .x
    zeros
  })

res <- abind::abind(mt_padded, along = 0)


r <- mt_fields[[1]] |>
  abind::abind(along = 0)


library(tensorflow)
library(keras)

