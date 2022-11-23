library(reticulate)
library(tensorflow)
library(keras)

np <- import("numpy", convert = FALSE)



# Python functions and scripts ------------------------------------

py_run_string('
from typing import Any, Dict, Iterable, Sequence, Tuple, Optional, Union
import numpy as np
import tensorflow as tf

def logsumexp_masked(risk_scores: tf.Tensor,
                     mask: tf.Tensor,
                     axis: int = 0,
                     keepdims: Optional[bool] = None) -> tf.Tensor:
    """Compute logsumexp across `axis` for entries where `mask` is true."""
    risk_scores.shape.assert_same_rank(mask.shape)

    with tf.name_scope("logsumexp_masked"):
        mask_f = tf.cast(mask, risk_scores.dtype)
        risk_scores_masked = tf.math.multiply(risk_scores, mask_f)
        # for numerical stability, substract the maximum value
        # before taking the exponential
        amax = tf.reduce_max(risk_scores_masked, axis=axis, keepdims=True)
        risk_scores_shift = risk_scores_masked - amax

        exp_masked = tf.math.multiply(tf.exp(risk_scores_shift), mask_f)
        exp_sum = tf.reduce_sum(exp_masked, axis=axis, keepdims=True)
        output = amax + tf.math.log(exp_sum)
        if not keepdims:
            output = tf.squeeze(output, axis=axis)
    return output
')


# functions -------------------------------------------------------
coxph_loss <- function(
    y_true,
    y_pred
) {
  event <- y_true[["event"]]
  # cat("event:\n")
  # print(event)
  # cat("\n\n")

  riskset <- y_true[["riskset"]]
  cat("riskset: \n")
  print(riskset)
  cat("\n\n")

  predictions <- y_pred
  # cat("predictions: \n")
  # print(predictions)
  # cat("\n\n")

  event <- tf$cast(event, y_pred$dtype)
  # cat("casted event: \n")
  # print(event)
  # cat("\n\n")


  predictions <- safe_normalize(predictions)
  # cat("normalized predictions: \n")
  # print(predictions)
  # cat("\n\n")

  pred_t <- tf$transpose(predictions)
  cat("transposed pred_t: \n")
  print(pred_t)
  cat("\n\n")

  rr <- logsumexp_masked(
    pred_t, riskset, axis = 1L, keepdims = TRUE
  )
  cat("rr: \n")
  print(rr)
  cat("\n\n")

  tf$math$multiply(event, rr - predictions)
}



safe_normalize <- function(x) {
  x_min <- tf$reduce_min(x, axis = 0L)
  c <- tf$zeros_like(x_min)
  norm <- tf$where(x_min < 0L, -x_min, c)
  x + norm
}





logsumexp_masked <- function(
    risk_score,
    mask,
    axis = 0,
    keepdims = FALSE
) {
  mask_f <- tf$cast(mask, risk_score$dtype)
  risk_scores_masked <- tf$math$multiply(risk_score, mask_f)
  amax <- tf$reduce_max(
    risk_scores_masked,
    axis = axis,
    keepdims = keepdims
  )

  risk_score_shift <- tf$math$subtract(risk_scores_masked, amax)

  exp_masked <- tf$math$multiply(tf$math$exp(risk_score_shift),  mask_f)
  exp_sum <- tf$reduce_sum(
    exp_masked,
    axis = axis,
    keepdims = keepdims
  )
  out <- amax + tf$math$log(exp_sum)

  if (!keepdims) {
    out <- tf$squeeze(out, axis = axis)
  }
  out
}


.make_riskset <- function(time) {
  time <- np$array(time)
  out_order <- np$argsort(np$subtract(0, time), kind = "mergesort")

  n_samples <- length(time)
  risk_set <- np$zeros(c(n_samples, n_samples), dtype = np$bool_)

  risk_set <- py_to_r(risk_set)
  out_order <- py_to_r(out_order)
  # print(out_order)
  # print(time)
  for (i in seq_along(out_order)) {
    # cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
    # cat("i: ", i, "\n")
    ti <- time[out_order[i]]
    # cat("ti: ", py_to_r(ti), "\n")
    k <- i
    # cat("k: ", k, "\n")
    while (k <= n_samples && ti == time[out_order[k]]) {
      k <- k + 1L
      # cat("k: ", k, "\n")
    }

    risk_set[out_order[i] + 1, out_order[seq_len(k - 1)] + 1] <- TRUE
    # print(risk_set)
    # cat("???????????????????????????????\n\n")
  }
  risk_set
}



# run example -----------------------------------------------------


example_original_y <- list(
  event = c(1L, 0L, 0L, 1L, 1L, 1L, 0L),
  time = c(12, 15, 37, 24, 39, 14, 11)
)
er <- list(
  event = np$array(array(example_original_y$event, dim = c(7, 1))),
  riskset = .make_riskset(example_original_y$time)
)
coxph_loss(er, np$array(array(example_original_y$time, dim = c(7, 1))))

