
on_cpu <- TRUE
debug <- FALSE

epochs <- 10

batch_size_train <- 6
batch_size_val <- 6

min_events <- 4

n_batch_train <- 32
n_batch_val <- 16

gc()

# setup -----------------------------------------------------------

library(targets)
# load all your custom functions
list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

library(reticulate)
use_condaenv("tf", required = TRUE)
np <- reticulate::import("numpy", convert = FALSE)

if (on_cpu) {
  Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
  reticulate::py_run_string('
# import os
# os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
# ')
}

library(tensorflow)
library(keras)

tf <- reticulate::import("tensorflow", convert = TRUE)
k <- reticulate::import("keras", convert = TRUE)
skm <- reticulate::import("sksurv.metrics", convert = TRUE)


# Setup -----------------------------------------------------------
# define batch sizes

# train_batches_paths <- setup_batch_files(batch_size_train, "train")
train_batches_paths <- setup_batch_files(
  batch_size_train,
  type = "train",
  random = TRUE,
  min_events_per_batch = min_events,
  steps_per_epoch = n_batch_train
)
gen_train_batches <- create_batch_generator(train_batches_paths)

# val_batches_paths <- setup_batch_files(batch_size_val, "val")
val_batches_paths <- setup_batch_files(
  batch_size_val,
  type = "val",
  random = TRUE,
  min_events_per_batch = min_events,
  steps_per_epoch = n_batch_val
)
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
  keras::layer_rescaling(
    name = "rescale_cine",
    scale = 1.0 / 255,
    offset = 1e-7
  ) %>%
  keras::layer_dropout(rate = 0.2)

cine_l1 <- input_cine %>%
  keras::layer_conv_lstm_2d(
    name = "cine_timeflat",
    filters = 32L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(30L, 640L, 480L, 28L),
    activation = "relu",
    dropout = 0.7,
    recurrent_dropout = 0.7
  )

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
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
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
) %>%
  keras::layer_rescaling(
    name = "rescale_lge",
    scale = 1.0 / 255,
    offset = 1e-7
  ) %>%
  keras::layer_dropout(rate = 0.2)

lge_l1 <- lge_l0 %>%
  keras::layer_conv_2d(
    name = "lge_l1",
    filters = 32L,
    kernel_size = 4L,
    padding = "same",
    input_shape = c(256L, 256L, 28L),
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
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
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_dropout(rate = 0.7)

merged_l2 <- merged_l1 %>%
  keras::layer_conv_2d(
    name = "merged_l2",
    filters = 16L,
    kernel_size = 2L,
    padding = "same",
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_dropout(rate = 0.7) %>%
  keras::layer_flatten(name = "flatting")


# output ----------------------------------------------------------
dense_l1 <- merged_l2 %>%
  keras::layer_dense(
    name = "dense_l1",
    units = 8,
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() |>
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_dropout(0.125)

dense_l2 <- dense_l1 %>%
  keras::layer_dense(
    name = "dense_l2",
    units = 8,
    activation = "relu"
  ) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)


output <- dense_l2 %>%
  keras::layer_dense(
    name = "output",
    units = 1,
    activation = "linear"
  )


# debug -----------------------------------------------------------

output_debug <- input_s_cine %>%
  keras::layer_rescaling(scale = 1.0 / 255, offset = 1e-7) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_conv_lstm_2d(
    name = "cine_timeflat",
    filters = 1L,
    kernel_size = 10L,
    strides = 10
  ) %>% # merged_l1 %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_max_pooling_2d(pool_size = c(8, 6)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_flatten(name = "flatting") %>%
  keras::layer_batch_normalization() %>%
  keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  keras::layer_dense(
    name = "output",
    units = 1,
    activation = "linear"
  )


# model -----------------------------------------------------------
model <- keras::keras_model(
  inputs = c(
    # input_s_cine
    input_s_cine, input_l2c_cine, input_l3c_cine, input_l4c_cine,
    input_s_lge, input_l2c_lge, input_l3c_lge, input_l4c_lge
  ),
  # outputs = output_debug
  outputs = output
)
summary(model)

if (debug) {
  tr_1 <- gen_train_batches()
  x1 <- tr_1$x
  y1_true <- tr_1$y_true

  y1_pred <- reticulate::r_to_py(predict(model, x1))
  y1_loss <- coxph_loss(y1_true, y1_pred)
  print(y1_true)
  print(tr_1$y_true[[3]])
  print(y1_pred)
  print(y1_loss)

  y2_pred <- reticulate::r_to_py(predict(model, x2))
  y2_loss <- coxph_loss(y2_true, y2_pred)
  print(y2_true)
  print(tr_2$y_true[[3]])
  print(y2_pred)
  print(y2_loss)
}


if (FALSE) {
  keras:::plot.keras.engine.training.Model()
  model |>
    plot(
      # show_shapes = TRUE,
      # show_layer_names = TRUE
      # expand_nested = TRUE,
      # show_layer_activations = TRUE,
    )
}

# # https://github.com/keras-team/keras/issues/16291
# # https://stackoverflow.com/questions/68354367/getting-an-error-when-using-tf-keras-metrics-mean-in-functional-keras-api
# # https://stackoverflow.com/a/43591066
# py_run_string('
# from typing import Any, Dict, Iterable, Sequence, Tuple, Optional, Union
# import tensorflow as tf
#
# class FixedMean(tf.keras.metrics.Mean):
#   def update_state(self, y_true, y_pred, sample_weight=None):
#       super().update_state(y_pred, sample_weight=sample_weight)
# ')
# FixedMean <- reticulate::py_to_r(py$FixedMean)

cindex_score <- function(y_true, y_pred) {
  y_true <- tf$cast(y_true, y_pred$dtype)

  # censored are the negative time
  event <- tf$transpose((tf$sign(y_true) + 1) / 2)
  time <- tf$transpose(tf$abs(y_true))

  event <- tf$cast(event, y_pred$dtype)
  time <- tf$cast(time, y_pred$dtype)

  y_pred <- tf$transpose(y_pred)
  event <- tf$squeeze(tf$cast(event, y_pred$dtype), axis = 0L)

  time <- tf$squeeze(tf$cast(time, y_pred$dtype), axis = 0L)
  y_pred <- tf$squeeze(y_pred, axis = 0L)

  # Hmisc::rcorr.cens(y_pred, survival::Surv(time, event))[["C Index"]]
  res <- skm$concordance_index_censored(
    event == 1L,
    time,
    tf$exp(y_pred)
  )[[1L]]
  res <- tf$cast(res, y_pred$dtype)
  tf$constant(res)
}

if (debug) cindex_score(y1_true, y1_pred)







{
  model %>%
    compile(
      optimizer = k$optimizers$Adam(
        # learning_rate = 1e-3,
        amsgrad = TRUE
        # clipnorm = 1,
        # global_clipnorm = 1,
        # clipvalue = 1e-3
      ),
      run_eagerly = TRUE,
      loss = coxph_loss,
      metrics = list(
        # FixedMean(),
        custom_metric("C", cindex_score)
      )
    )
}


{
  run_id <- glue::glue(paste0(
    "{if (debug) 'DEBUG-' else ''}",
    "run_{stringr::str_remove_all(lubridate::now(), '\\\\W')}-",
    "{n_batch_train}x{batch_size_train}gte{min_events}events"
  ))
  # tb_path <- here::here("logs", run_id)
  # tensorboard(tb_path)
  tic <- Sys.time()
  history <- model %>%
    keras::fit(
      x = gen_train_batches,
      steps_per_epoch = n_batch_train,
      epochs = epochs,
      validation_data = gen_val_batches,
      validation_steps = n_batch_val#,
      # view_metrics = FALSE
      # callbacks = keras::callback_tensorboard(
      #   tb_path
      # #   histogram_freq = TRUE,
      # #   write_grads = TRUE,
      # #   update_freq = "batch",
      # #   profile_batch = 1
      # )
    )
  (toc <- Sys.time() - tic)
}

# predict(model, aaa)

readr::write_rds(toc, glue::glue("{run_id}_tictoc_model.rds"))
readr::write_rds(history, glue::glue("{run_id}_history.rds"))
keras::save_model_hdf5(model, glue::glue("{run_id}_model.hdf5"))
keras::serialize_model(model) |>
  readr::write_rds(glue::glue("{run_id}_serialized_model.rds"))



# ## https://stackoverflow.com/a/61161718
# ## for continue training
# load_model_hdf5(
#   filepath = "modelpath",
#   custom_objects = list(coxph_loss = coxph_loss)
# )
# ##
# ## for prediction only
# load_model_hdf5(filepath = "modelpath", compile = False)

# saved_model <- readr::read_rds("serialized_model.rds")
# keras::unserialize_model(saved_model)
#
# modelll <- keras::load_model_hdf5("model.hdf5")
#
#
# #
# #
# ( tr_1 <- gen_train_batches() )
# (
#   y_true <- list(np$array(tr_1$y_true[[1]]), np$array(tr_1$y_true[[2]]))
# )
# # val_1 <- gen_val_batches()
# #
# ( pred_tr_1 <- predict(model, tr_1$x) )
# ( losses_tr_1 <- coxph_loss(y_true, pred_tr_1) )
#
#
#
# str(val_1, 1)
# str(val_1$x, 1)
# pred_val_1 <- predict(model, val_1$x)
# pred_val_1
# val_1$y_true
#
# y_true <- list(np$array(val_1$y_true[[1]]), np$array(val_1$y_true[[2]]))
# y_pred <- np$array(pred_val_1)
#
# (losses_val_1 <- coxph_loss(y_true, y_pred))
# cindex_score(y_true, y_pred)
#
# g = tf$subtract(tf$expand_dims(y_pred, -1L), y_pred)
# g = tf$cast(g == 0.0, tf$float32) * 0.5 + tf$cast(g > 0.0, tf$float32)
#
# f = tf$subtract(tf$expand_dims(y_true[[1]], -1L), y_true[[1]]) > 0.0
# f = tf$compat$v1$matrix_band_part(tf$cast(f, tf$float32), -1L, 0L)
#
# g = tf$reduce_sum(tf$multiply(g, f))
# f = tf$reduce_sum(f)
#
# tf$where(tf$equal(g, 0L), 0.0, g/f)
#
#
#
# loss_mean_absolute_error(y_true[[1]], y_pred)
#
# tf$squeeze(losses_val_1)

