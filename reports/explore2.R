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
    event_riskset,
    predictions
) {
  event <- event_riskset[["event"]]
  riskset <- event_riskset[["riskset"]]

  event <- tf$cast(event, predictions$dtype)


  predictions <- safe_normalize(predictions)

  pred_t <- tf$transpose(predictions)

  rr <- logsumexp_masked(
    pred_t, riskset, axis = 1L, keepdims = TRUE
  )

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

  print(mask)
  print(mask_f)
  print(risk_score)
  print(risk_scores_masked)
  print(risk_score_shift)

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
  out_order <- np$argsort(np$subtract(0, time), kind = "mergesort") |>
    py_to_r() |>
    as.integer()
  n_samples <- length(time)
  risk_set <- np$zeros(c(n_samples, n_samples), dtype = np$bool_) |>
    py_to_r() |>
    as.matrix()

  for (i in seq_along(out_order)) {
    ti <- time[out_order[i]]
    k <- i
    while (k < n_samples && ti == time[out_order[k]]) {
      k <- k + 1L
    }

    risk_set[out_order[i] + 1, out_order[seq_len(k - 1)] + 1] <- TRUE
    print(risk_set)
  }
  risk_set
}



# run example -----------------------------------------------------


example_original_y <- list(
  event = np$array(array(c(1L, 0L, 0L, 1L, 1L, 1L, 0L), dim = c(7, 1))),
  time = np$array(c(12, 15, 37, 24, 39, 14, 11))
)
riskset <- .make_riskset(example_original_y$time)
er <- list(event = example_original_y$event, riskset = riskset)
coxph_loss(er, example_original_y$time)

