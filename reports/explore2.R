library(reticulate)
library(tensorflow)
library(keras)

np <- import("numpy", convert = FALSE)


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
    pred_t, riskset, axis = 1, keepdims = TRUE
  )

  tf$math$multiply(event, rr - predictions)
}

safe_normalize <- function(x) {
  x_min <- tf$reduce_min(x, axis = 0L)
  c <- tf$zeros_like(x_min)
  norm <- tf$where(x_min < 0L, -x_min, c)
  x + norm
}

.make_riskset <- function(time) {
  out_order <- np$argsort(0 - time, kind = "mergesort")
  n_samples <- length(time)
  risk_set <- np$zeros(c(n_samples, n_samples), dtype = np$bool_) |>
    as.matrix()

  o <- as.integer(py_to_r(out_order)) + 1L
  for (i in seq_along(o)) {
    ti <- time[o[i]]
    k <- i
    while (k < n_samples && ti == time[o[k]]) {
      k <- k + 1L
    }

    risk_set[o[i], o[seq_len(k - 1)]] <- TRUE
  }
  risk_set
}


example_original_y <- data.frame(
  event = c(1, 0, 0, 1, 1, 1, 0),
  time = c(12, 15, 37, 24, 39, 14, 11)
)
# riskset <- .make_riskset(example_original_y$time)

