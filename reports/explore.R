## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------

library(targets)
# load all your custom functions
list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

library(reticulate)
use_condaenv("tf", required = TRUE)
np <- reticulate::import("numpy", convert = FALSE)

library(tensorflow)
library(keras)


# Code here below -------------------------------------------------
# use `tar_read(target_name)` to load a target anywhere (note that
# `target_name` is NOT quoted!)

# define batch sizes
batch_size_train <- 6
batch_size_val <- 6

train_batches_paths <- setup_batch_files(batch_size_train, "train")
gen_train_batches <- create_batch_generator(train_batches_paths)

val_batches_paths <- setup_batch_files(batch_size_val, "val")
gen_val_batches <- create_batch_generator(val_batches_paths)




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
    activation = "linear"
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


# https://github.com/keras-team/keras/issues/16291
# https://stackoverflow.com/questions/68354367/getting-an-error-when-using-tf-keras-metrics-mean-in-functional-keras-api
py_run_string('
from typing import Any, Dict, Iterable, Sequence, Tuple, Optional, Union
import tensorflow as tf
from sksurv.metrics import concordance_index_censored

class FixedMean(tf.keras.metrics.Mean):
  def update_state(self, y_true, y_pred, sample_weight=None):
      super().update_state(y_pred, sample_weight=sample_weight)

def cindex_score(y_true, y_pred):

    g = tf.subtract(tf.expand_dims(y_pred, -1), y_pred)
    g = tf.cast(g == 0.0, tf.float32) * 0.5 + tf.cast(g > 0.0, tf.float32)

    f = tf.subtract(tf.expand_dims(y_true[1], -1), y_true[1]) > 0.0
    f = tf.compat.v1.matrix_band_part(tf.cast(f, tf.float32), -1, 0)

    g = tf.reduce_sum(tf.multiply(g, f))
    f = tf.reduce_sum(f)

    return tf.where(tf.equal(g, 0), 0.0, g/f)
')

FixedMean <- reticulate::py_to_r(py$FixedMean)
cindex_score <- reticulate::py_to_r(py$cindex_score)

model %>%
  compile(
    optimizer = "adam",
    loss = coxph_loss,
    metrics = list(FixedMean(), cindex_score)
)

tic <- Sys.time()
history <- model %>%
  keras::fit(
    x = gen_train_batches, # 4
    steps_per_epoch = length(train_batches_paths), # 19,
    epochs = 10,
    # shuffle = FALSE,
    validation_data = gen_val_batches,
    validation_steps = length(val_batches_paths)#,
#    class_weight = list(0.851, 0.149)
  )
toc <- Sys.time() - tic

readr::write_rds(toc, "tictoc_model.rds")
readr::write_rds(history, "history.rds")
keras::save_model_hdf5(model, "model.hdf5")
keras::serialize_model(model) |>
readr::write_rds("serialized_model.rds")
