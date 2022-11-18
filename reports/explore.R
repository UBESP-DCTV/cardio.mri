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





data_generator <- function(batch_size) {

  s_cine  <- targets::tar_read(cine1Keras_train)
  l2_cine <- targets::tar_read(cine2Keras_train)
  l3_cine <- targets::tar_read(cine3Keras_train)
  l4_cine <- targets::tar_read(cine4Keras_train)

  s_lge  <- targets::tar_read(lge1Keras_train)
  l2_lge <- targets::tar_read(lge2Keras_train)
  l3_lge <- targets::tar_read(lge3Keras_train)
  l4_lge <- targets::tar_read(lge4Keras_train)

  nodes_name <- attr(l4_lge, "dimnames")[[1]]

  # start iterator
  i <- 1

  # return an iterator function
  function() {
    current_start_id <- i + batch_size - 1
    last_id <- length(nodes_name)

    # reset iterator if already seen all data
    if (current_start_id > last_id) i <<- 1

    # iterate current batch's rows
    rows <- i:min(current_start_id, last_id)

    # update to next iteration
    i <<- current_start_id + 1

    # find the outcome
    records <- nodes_name[rows]
    outcomes <- purrr::map_int(records, ~{
      as.integer(targets::tar_read_raw(.x)[["output"]][["outcome"]])
    })

    y_array <- array(
      c(seq_along(records), outcomes),
      dim = c(length(records), length(outcomes))
    )

    # return the batch
    res <- list(
      s_cine = s_cine[records, , , , , drop = FALSE],
      l2_cine = l2_cine[records, , , , drop = FALSE],
      l3_cine = l3_cine[records, , , , drop = FALSE],
      l4_cine = l4_cine[records, , , , drop = FALSE],
      s_lge = s_lge[records, , , , drop = FALSE],
      l2_lge = l2_lge[records, , , drop = FALSE],
      l3_lge = l3_lge[records, , , drop = FALSE],
      l4_lge = l4_lge[records, , , drop = FALSE],
      y = y_array
    )
    gc(FALSE, TRUE)
    gc(FALSE, TRUE)
    res
  }
}


gen <- data_generator(4)






library(reticulate)
use_condaenv("tf", required = TRUE)

library(tensorflow)
tf_config()
tf_gpu_configured()


library(keras)



# cine ------------------------------------------------------------
input_s_cine <- layer_input(
  name = "input_cine_s",
  shape = c(30, 640, 480, 25)
)
input_l2c_cine <- layer_input(
  name = "input_cine_l2",
  shape = c(30, 640, 480, 1)
)
input_l3c_cine <- layer_input(
  name = "input_cine_l3",
  shape = c(30, 640, 480, 1)
)
input_l4c_cine <- layer_input(
  name = "input_cine_l4",
  shape = c(30, 640, 480, 1)
)


input_cine <- layer_concatenate(
  c(input_s_cine, input_l2c_cine, input_l3c_cine, input_l4c_cine),
  axis = 4,
  name = "input_cine"
)

cine_l1 <- input_cine %>%
  keras::layer_conv_lstm_2d(
    name = "cine_timeflat",
    filters = 32L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(30L, 640L, 480L, 28L),
    activation = "relu"
  )

cine_l1_padded <- cine_l1 %>%
  keras::k_spatial_2d_padding(
    padding = list(list(63L, 64L), list(16L, 16L)),
    data_format = "channels_last"
  )

cine_l2 <- cine_l1_padded %>%
  keras::layer_max_pooling_2d(
    pool_size = c(2L, 2L),
    strides = c(3L, 2L),
    name = "pooling_cine"
  )

# lge -------------------------------------------------------------
input_s_lge <- layer_input(
    name = "input_s_lge",
    shape = c(640, 480, 25)
  )

input_s_lge_padded <- input_s_lge  %>%
  keras::k_spatial_2d_padding(
    padding = list(list(63L, 64L), list(16L, 16L)),
    data_format = "channels_last"
  )

input_s_lge_pooled <- input_s_lge_padded %>%
  keras::layer_max_pooling_2d(
    pool_size = c(2L, 2L),
    strides = c(3L, 2L),
    name = "pooling_lge"
  )

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
)

lge_l1 <- lge_l0 %>%
  keras::layer_conv_2d(
    name = "lge_l1",
    filters = 32L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(256L, 256L, 28L),
    activation = "relu"
  )



# shared ----------------------------------------------------------
shared_cnn_l1 <- keras::layer_conv_2d(
  name = "shared_cnn_l1",
  filters = 32L,
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
)

merged_l2 <- merged_l1 %>%
  keras::layer_conv_2d(
    name = "merged_l2",
    filters = 32L,
    kernel_size = 2L,
    padding = "same",
    activation = "relu"
  ) %>%
  keras::layer_flatten(name = "flatting")


# output ----------------------------------------------------------
dense_l1 <- merged_l2 %>%
  keras::layer_dense(
    name = "dense_l1",
    units = 256,
    activation = "relu"
  )

dense_l2 <- dense_l1 %>%
  keras::layer_dense(
    name = "dense_l2",
    units = 256,
    activation = "relu"
  )

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
keras:::plot.keras.engine.training.Model(model, show_shapes = TRUE, show_layer_names = TRUE)




