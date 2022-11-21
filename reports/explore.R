## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------

library(targets)
library(here)
library(depigner)


library(reticulate)
use_condaenv("tf", required = TRUE)
 system2("export", "LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_PREFIX/lib/")

library(tensorflow)
library(keras)
np <- import("numpy", convert = FALSE)

#
#
# tf_config()
# tf_gpu_configured()
#
#
# Sys.sleep(5)




# load all your custom functions
list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Code here below -------------------------------------------------
# use `tar_read(target_name)` to load a target anywhere (note that
# `target_name` is NOT quoted!)

# define batch sizes
batch_size_train <- 4
batch_size_val <- 4

# get train/val sample sizes and corresponding number of batches
lg4train <- targets::tar_read(lge4Keras_train)
nodes_name_train <- attr(lg4train, "dimnames")[[1]]
n_train <- length(nodes_name_train)
n_batches_train <- ceiling(n_train / batch_size_train)

lg4val <- targets::tar_read(lge4Keras_val)
nodes_name_val <- attr(lg4val, "dimnames")[[1]]
n_val <- length(nodes_name_val)
n_batches_val <- ceiling(n_val / batch_size_val)

rm(lg4train, lg4val, nodes_name_train, nodes_name_val)

# get path of or save batcehs
train_bfp_path <- glue::glue(
    "dev/{batch_size_train}-batch_train_file_paths.rds"
  ) |>
  here::here()

train_batches_file_paths <- if (fs::file_exists(train_bfp_path)) {
  readr::read_rds(train_bfp_path)
} else {
  train_generator <- function(batch_size) {
    # start iterator
    i <- 1

    # return an iterator function
    function() {
      current_start_id <- i + batch_size - 1

      l4_lge <- targets::tar_read(lge4Keras_train)
      nodes_name <- attr(l4_lge, "dimnames")[[1]]
      last_id <- length(nodes_name)

      # reset iterator if already seen all data
      if (current_start_id > last_id) i <<- 1

      # iterate current batch's rows
      rows <- i:min(current_start_id, last_id)
      records <- nodes_name[rows]

      # update to next iteration
      i <<- current_start_id + 1

      # find the outcome
      outcomes <- purrr::map_int(records, ~{
        as.integer(targets::tar_read_raw(.x)[["output"]][["outcome"]])
      })
      y_array <- array(outcomes, dim = c(length(outcomes), 1))
      rm(outcomes); gc()

      # return the batch
      list(
        x = list(
          input_s_cine = targets::tar_read(cine1Keras_train)[records, , , , , drop = FALSE],
          input_l2c_cine = targets::tar_read(cine2Keras_train)[records, , , , drop = FALSE],
          input_l3c_cine = targets::tar_read(cine3Keras_train)[records, , , , drop = FALSE],
          input_l4c_cine = targets::tar_read(cine4Keras_train)[records, , , , drop = FALSE],
          input_s_lge = targets::tar_read(lge1Keras_train)[records, , , , drop = FALSE],
          input_l2c_lge = targets::tar_read(lge2Keras_train)[records, , , drop = FALSE],
          input_l3c_lge = targets::tar_read(lge3Keras_train)[records, , , drop = FALSE],
          input_l4c_lge = l4_lge[records, , , drop = FALSE]
        ),
        y = y_array
      )
    }
  }

  # retrieve, subset, quick save individual batches for train and val
  gen_train <- train_generator(batch_size_train)
  train_files <- purrr::map_chr(seq_len(n_batches_train), ~{
    train_file <- fs::file_temp(
      pattern = paste0("train_batch-", .x, "-", n_batches_train, "_"),
      tmp_dir = here::here("dev/batches") |> fs::dir_create(),
      ext = "qs"
    )
    qs::qsave(gen_train(), train_file, preset = "fast")
    usethis::ui_done(
      "({.x}/{n_batches_train}): {usethis::ui_value('train_batch-{{.x}}.qs')} written on disk at {usethis::ui_code(train_file)}."
    )
    train_file
  })
  rm(n_train, gen_train, train_generator)

  train_files |>
    readr::write_rds(train_bfp_path)
}
rm(train_bfp_path)


# get path of or save batcehs
val_bfp_path <- glue::glue(
  "dev/{batch_size_val}-batch_val_file_paths.rds"
) |>
  here::here()

val_batches_file_paths <- if (fs::file_exists(val_bfp_path)) {
  readr::read_rds(val_bfp_path)
} else {
  val_generator <- function(batch_size) {
    # start iterator
    i <- 1

    # return an iterator function
    function() {
      current_start_id <- i + batch_size - 1

      l4_lge <- targets::tar_read(lge4Keras_val)
      nodes_name <- attr(l4_lge, "dimnames")[[1]]
      last_id <- length(nodes_name)

      # reset iterator if already seen all data
      if (current_start_id > last_id) i <<- 1

      # iterate current batch's rows
      rows <- i:min(current_start_id, last_id)
      records <- nodes_name[rows]

      # update to next iteration
      i <<- current_start_id + 1

      # find the outcome
      outcomes <- purrr::map_int(records, ~{
        as.integer(targets::tar_read_raw(.x)[["output"]][["outcome"]])
      })
      y_array <- array(outcomes, dim = c(length(outcomes), 1))
      rm(outcomes); gc()

      # return the batch
      list(
        x = list(
          input_s_cine = targets::tar_read(cine1Keras_val)[records, , , , , drop = FALSE],
          input_l2c_cine = targets::tar_read(cine2Keras_val)[records, , , , drop = FALSE],
          input_l3c_cine = targets::tar_read(cine3Keras_val)[records, , , , drop = FALSE],
          input_l4c_cine = targets::tar_read(cine4Keras_val)[records, , , , drop = FALSE],
          input_s_lge = targets::tar_read(lge1Keras_val)[records, , , , drop = FALSE],
          input_l2c_lge = targets::tar_read(lge2Keras_val)[records, , , drop = FALSE],
          input_l3c_lge = targets::tar_read(lge3Keras_val)[records, , , drop = FALSE],
          input_l4c_lge = l4_lge[records, , , drop = FALSE]
        ),
        y = y_array
      )
    }
  }

  gen_val <- val_generator(batch_size_val)
  val_files <- purrr::map_chr(seq_len(n_batches_val), ~{
    val_file <- fs::file_temp(
      pattern = paste0("val_batch-", .x, "-", n_batches_val, "_"),
      tmp_dir = here::here("dev/batches") |> fs::dir_create(),
      ext = "qs"
    )
    qs::qsave(gen_val(), val_file, preset = "fast")
    usethis::ui_done(
      "({.x}/{n_batches_val}): {usethis::ui_value('val_batch-{{.x}}.qs')} written on disk at {usethis::ui_code(val_file)}."
    )
    val_file
  })
  rm(n_val, gen_val, val_generator)

  val_files |>
    readr::write_rds(val_bfp_path)
}
rm(val_bfp_path)


# define batches generators
batches_generator <- function(file_paths) {
  # start iterator
  i <- 1

  # return an iterator function
  function() {
    # reset iterator if already seen all data
    if (i > length(file_paths)) i <<- 1

    # return current batch
    qs::qread(file_paths[[i]], nthreads = 16)
  }
}

gen_train_batches <- batches_generator(train_batches_file_paths)
gen_val_batches <- batches_generator(val_batches_file_paths)
rm(batches_generator)
gc(FALSE, TRUE);gc(FALSE, TRUE)
# val_all <- gen_val()
# rm(gen_val)

# keras:::as_generator.function
# train_1 <- gen_train()
# train_2 <- gen_train()
# rm(gen_train)
#
# val_1 <- gen_val()
# rm(gen_val)


#
# gen_train <- keras:::as_generator.function()
# gen_val <- keras:::as_generator.function()






# cine ------------------------------------------------------------
input_s_cine <- keras::layer_input(
  name = "input_s_cine",
  shape = c(30, 640, 480, 25)
)
input_l2c_cine <- keras::layer_input(
  name = "input_l2c_cine",
  shape = c(30, 640, 480, 1)
)
input_l3c_cine <- keras::layer_input(
  name = "input_l3c_cine",
  shape = c(30, 640, 480, 1)
)
input_l4c_cine <- keras::layer_input(
  name = "input_l4c_cine",
  shape = c(30, 640, 480, 1)
)


input_cine <- keras::layer_concatenate(
  c(input_s_cine, input_l2c_cine, input_l3c_cine, input_l4c_cine),
  axis = 4,
  name = "input_cine"
) %>%
  keras::layer_batch_normalization()  %>%
  keras::layer_dropout(rate = 0.2)

cine_l1 <- input_cine %>%
  keras::layer_conv_lstm_2d(
    name = "cine_timeflat",
    filters = 16L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(30L, 640L, 480L, 28L),
    activation = "relu",
    dropout = 0.7,
    recurrent_dropout = 0.7
  ) %>%
  keras::layer_batch_normalization()

cine_l1_padded <- cine_l1 %>%
  keras::layer_zero_padding_2d(
    name = "cine_padding",
    padding = list(list(63L, 64L), list(16L, 16L)),
    data_format = "channels_last"
  )

cine_l2 <- cine_l1_padded %>%
  keras::layer_max_pooling_2d(
    pool_size = c(2L, 2L),
    strides = c(3L, 2L),
    name = "pooling_cine"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.7)

# lge -------------------------------------------------------------
input_s_lge <- layer_input(
    name = "input_s_lge",
    shape = c(640, 480, 25)
  )

input_s_lge_padded <- input_s_lge  %>%
  keras::layer_zero_padding_2d(
    name = "lge_padding",
    padding = list(list(63L, 64L), list(16L, 16L)),
    data_format = "channels_last"
  )

input_s_lge_pooled <- input_s_lge_padded %>%
  keras::layer_max_pooling_2d(
    pool_size = c(2L, 2L),
    strides = c(3L, 2L),
    name = "pooling_lge"
  ) %>%
  keras::layer_batch_normalization()

input_l2c_lge <- layer_input(
  name = "input_l2c_lge",
  shape = c(256, 256, 1)
)
input_l3c_lge <- layer_input(
  name = "input_l3c_lge",
  shape = c(256, 256, 1)
)
input_l4c_lge <- layer_input(
  name = "input_l4c_lge",
  shape = c(256, 256, 1)
)

lge_l0 <- layer_concatenate(
  c(input_s_lge_pooled, input_l2c_lge, input_l3c_lge, input_l4c_lge),
  axis = 3,
  name = "lge_l0"
) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.2)

lge_l1 <- lge_l0 %>%
  keras::layer_conv_2d(
    name = "lge_l1",
    filters = 16L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(256L, 256L, 28L),
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.7)



# shared ----------------------------------------------------------
shared_cnn_l1 <- keras::layer_conv_2d(
  name = "shared_cnn_l1",
  filters = 16L,
  kernel_size = 2L,
  padding = "same",
  input_shape = c(256L, 256L, 32L),
  activation = "relu"
)

cine_l3 <- cine_l2 %>%
  shared_cnn_l1()
lge_l2 <- lge_l1 %>%
  shared_cnn_l1()
merged_l1 <- keras::layer_concatenate(
  c(cine_l3, lge_l2),
  axis = 3,
  name = "merged_l1"
) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.5)

merged_l2 <- merged_l1 %>%
  keras::layer_conv_2d(
    name = "merged_l2",
    filters = 16L,
    kernel_size = 2L,
    padding = "same",
    activation = "relu"
  ) %>%
  keras::layer_flatten(name = "flatting") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.5)


# output ----------------------------------------------------------
dense_l1 <- merged_l2 %>%
  keras::layer_dense(
    name = "dense_l1",
    units = 8,
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.5)

dense_l2 <- dense_l1 %>%
  keras::layer_dense(
    name = "dense_l2",
    units = 8,
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_dropout(rate = 0.5)

output <- dense_l2 %>%
  keras::layer_dense(
    name = "output",
    units = 1,
    activation = "sigmoid"
  )



# model -----------------------------------------------------------
model <- keras::keras_model(
  inputs = c(
    input_s_cine, input_l2c_cine, input_l3c_cine, input_l4c_cine,
    input_s_lge, input_l2c_lge, input_l3c_lge, input_l4c_lge
  ),
  outputs = output
)

summary(model)

# if (interactive()) {
#   plot(model, show_shapes = TRUE, show_layer_names = TRUE)
# }

model %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("acc")
)

tic <- Sys.time()
history <- model %>%
  keras::fit(
    x = gen_train_batches, # train_1[["x"]], # gen_train,
    # y = train_1[["y"]],
    # batch_size = 4,
    steps_per_epoch = n_batches_train, # 38,
    epochs = 10,
    # shuffle = FALSE,
    validation_data = gen_val_batches, # val_1, #
    validation_steps = n_batches_val,
    class_weight = list(0.851, 0.149) # ,
    # view_metrics = FALSE
  )
toc <- Sys.time() - tic

readr::write_rds(toc, "tictoc_model.rds")
readr::write_rds(history, "history.rds")
keras::save_model_hdf5(model, "model.hdf5")
keras::serialize_model(model) |>
readr::write_rds("serialized_model.rds")
