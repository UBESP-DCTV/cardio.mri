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

transposed <- tar_read(forKeras)


a <- tar_read(mris_b381672b)

mt <- tar_read(matched, branches = 1)


mt |> str(1)


to_retain <- purrr::map_lgl(mt, ~length(.x) == 10)
mt_used <- mt[to_retain]

mt_fields <- purrr::transpose(mt_used)
str(mt_fields[["cine-1"]], 1)

qs::qsave(mt_fields, "mt_field.qs")

purrr::map(mt_fields[["cine-1"]], dim)


max_dim <- purrr::map(mt_fields[["cine-1"]], dim) |>
   unlist() |>
   matrix(nrow = 4) |>
   apply(1, max)

zeros <- zeros <- array(0L, dim = max_dim)

mt_padded <- mt_fields[["cine-1"]] |>
  purrr::map(~{
    dims <- dim(.x)
    idx <- purrr::map(dims, seq_len)
    zeros[idx[[1]], idx[[2]], idx[[3]], idx[[4]]] <- .x
    zeros
  })

res <- abind::abind(mt_padded, along = 0)


r <- mt_fields[["cine-1"]] |>
  abind::abind(along = 0)


library(reticulate)
library(tensorflow)
library(keras)


cine_1 <- qs::qread("cine-1.qs")
mt_field <- qs::qread("mt_field.qs")
out <- dplyr::bind_rows(mt_field[["output"]])[["outcome"]]
rm(mt_field)
gc()


model <- keras_model_sequential(
    input_shape = c(640, 480, 18, 25) # 640 480 18 25
  ) |>
  layer_flatten() |>
  layer_dense(2, activation = "relu") |>
  layer_dense(1)

model %>% compile(
  optimizer = "adam",
  loss = loss_sparse_categorical_crossentropy(from_logits = TRUE),
  metrics = "accuracy"
)

summary(model)

model %>% fit(cine_1, out, epochs = 5)
