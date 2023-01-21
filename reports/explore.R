list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

on_cpu <- TRUE
debug <- FALSE
with_clinic <- TRUE
continue <- TRUE
trainval <- TRUE
modelpath <- get_keras_model_path("covar")[[10]]
nepoch <- 29

epochs <- nepoch + 6

batch_size_train <- 6
batch_size_val <- 6
batch_size_test <- 6

min_events <- 4

n_batch_train <- 32
n_batch_val <- 16
n_batch_test <- 16


# setup -----------------------------------------------------------

library(targets)
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

# test_batches_paths <- setup_batch_files(batch_size_test, "test")
test_batches_paths <- setup_batch_files(
  batch_size_test,
  type = "test",
  random = TRUE,
  min_events_per_batch = min_events,
  steps_per_epoch = n_batch_test
)
gen_test_batches <- create_batch_generator(test_batches_paths)


gen_trainval_batches <- create_batch_generator(
  sample(c(train_batches_paths, val_batches_paths))
)
# b <- gen_trainval_batches()

model <- if (continue) {
 keras::load_model_hdf5(modelpath, compile = FALSE)
} else {
  define_keras_model(with_clinic, debug)
}

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

  cindex_score(y1_true, y1_pred)
}

if (FALSE) plot_model(model, to_file = "topology-full.png")

model %>%
  compile(
    optimizer = k$optimizers$Adam(amsgrad = TRUE),
    run_eagerly = TRUE,
    loss = coxph_loss,
    metrics = list(custom_metric("C", cindex_score))
  )

{
  run_id <- glue::glue(paste0(
    "{if (debug) 'DEBUG-' else ''}",
    "run_{stringr::str_remove_all(lubridate::now(), '\\\\W')}-",
    "{n_batch_train}x{batch_size_train}gte{min_events}events",
    "{if (continue) paste0('-CONTINUE_from_', nepoch) else ''}",
    "-{epochs}_epochs"
  ))
  # tb_path <- here::here("logs", run_id)
  # tensorboard(tb_path)

  x <- if (trainval) gen_trainval_batches else gen_train_batches
  spe <- if (trainval) (n_batch_train + n_batch_val) else n_batch_train
  ie <- if (continue) nepoch else 0
  vd <- if (trainval) gen_test_batches else gen_val_batches
  vs <- if (trainval) n_batch_test else n_batch_val

  tic <- Sys.time()
  history <- model %>%
    keras::fit(
      x = x,
      steps_per_epoch = spe,
      initial_epoch = ie,
      epochs = epochs,
      validation_data = vd,
      validation_steps = vs,
      callbacks = list(
        callback_model_checkpoint(
          filepath = "models_epoch-{epoch:02d}_C_{val_C:.2f}.hdf5",
          monitor = "val_C",
          mode = "max"
        )
      )
    )
  (toc <- Sys.time() - tic)
}

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

