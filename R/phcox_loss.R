#' Cox Proportional Hazard non-linear loss function
#'
#'
#' @param y_true (list) of two element: [y_true][["event"]] is the event
#'   indicator (0 = censored, 1 = event), [y_true][["time"]] is the
#'   corresponding time (of event if event = 1, or censoring if event =
#'   0).
#' @param y_pred (2D array) single column array reporting predicted risk
#'   scores for subjects in each rows.
#'
#' @note Loss functions in tensorflow require 2 input only, i.e., y_ture
#'   (ground true references) and y_pred (network predictions). On the
#'   other hand for survival loss function we need to evaluate the loss
#'   at the batch level considering losses computed for each subject
#'   using their corresponding risk set. So, we need to pass to the
#'   function the risk set also. Moreover the loss for survival risk
#'   scores is only a matter of concordance between confrontable
#'   subjects (i.e., subjects in the same risk set), so the
#'   event/censoring indicator and time are sufficent end necessary to
#'   evaluate the loss.
#'
#' @return
#' @export
#'
#' @examples
#' example_original_y <- list(
#'   c(1L, 0L, 0L, 1L, 1L, 1L, 0L),
#'   c(12, 15, 37, 24, 39, 14, 11)
#' )
#' y_true <- list(
#'   array(example_original_y[[1]], dim = c(7, 1)),
#'   make_riskset(example_original_y[[2]])
#' )
#' y_pred <- np$array(array(example_original_y[[2]], dim = c(7, 1)))
#' batch_losses <- coxph_loss(y_true, y_pred)
#' batch_loss <- mean(reticulate::py_to_r(batch_losses))
coxph_loss <- function(
    y_true,
    y_pred
) {
  # cat(str(y_true))
  # censored are the negative time
  y_true <- tf$cast(y_true, y_pred$dtype)

  event <- (tf$sign(y_true) + 1) / 2
  time <- tf$abs(y_true)
  riskset <- make_riskset(time)
  # cat(str(event))
  # cat(str(time))
  # cat(str(riskset))
  #


  event <- tf$cast(event, y_pred$dtype)
  predictions <- safe_normalize(y_pred)
  pred_t <- tf$transpose(predictions)

  # logsumexp_masked -----------------------------------
  mask_f <- tf$cast(riskset, pred_t$dtype)
  risk_scores_masked <- tf$math$multiply(pred_t, mask_f)
  amax <- tf$reduce_max(
    risk_scores_masked,
    axis = 1L,
    keepdims = TRUE
  )
  risk_scores_shift <- tf$math$subtract(risk_scores_masked, amax)

  exp_masked <- tf$math$multiply(
    tf$math$exp(risk_scores_shift),
    mask_f
  )
  exp_sum <- tf$reduce_sum(
    exp_masked,
    axis = 1L,
    keepdims = TRUE
  )
  rr <- amax + tf$math$log(exp_sum + 1e-7)
  # logsumexp_masked -----------------------------------

  # rr <- logsumexp_masked(pred_t, riskset, axis = 1L, keepdims = TRUE)
  tf$math$multiply(event, tf$abs(rr - predictions)) + 1e-7
}






#' Safe normalize
#'
#' If there are negative scores, it slide everything to have a minimum
#' in zero. In fact, only concordance order with risk scores matters.
#'
#' @param x (2D tensor) single column 2D array, i.e. one row per
#'   subject, and onyl the column for the predicted risk score
#'
#' @return a tensor of the same shape of [x]
#' @export
#'
#' @examples
#' ok <- array(1:10, dim = c(10, 1))
#' ko <- array(-1:8, dim = c(10, 1))
#'
#' ok_normalized <- safe_normalize(ok)
#' ko_normalized <- safe_normalize(ko)
#'
#' identical(ok_normalized, ok)
#' identical(ko_normalized, ok)
safe_normalize <- function(x) {
  stopifnot(`x must be a single column array` = dim(x)[[2]] == 1)

  x_min <- tf$reduce_min(x, axis = 0L)
  c <- tf$zeros_like(x_min)
  norm <- tf$where(x_min < 0L, -x_min, c)
  x + norm
}



#' Make risk set matrix
#'
#' Creates the matrix representing the risk sets, i.e., every rows
#' represent a subject of reference, and every column a target subject,
#' the mask risk set matrix report TRUE for every target subject which
#' is in the risk set of reference subjects (i.e., ones wiht greater or
#' equal reported time (of event/censoring))
#'
#' @param time (num) vector of subjects' times
#'
#' @return (lgl, 2D array) reporting in cel (i, j) if subject j is in
#'   the risk set of subject i.
#' @export
#'
#' @examples
#' time <- c(12, 15, 37, 24, 39, 14, 11)
#' make_riskset(time)
make_riskset <- function(time) {
  np <- reticulate::import("numpy", convert = FALSE)
  time <- np$array(time)
  out_order <- np$argsort(np$subtract(0, time), kind = "mergesort")

  n_samples <- length(time)
  risk_set <- np$zeros(c(n_samples, n_samples), dtype = np$bool_)

  risk_set <- reticulate::py_to_r(risk_set)
  out_order <- reticulate::py_to_r(out_order)
  for (i in seq_along(out_order)) {
    ti <- time[out_order[i]]
    k <- i
    while (k <= n_samples && ti == time[out_order[k]]) {
      k <- k + 1L
    }
    risk_set[out_order[i] + 1, out_order[seq_len(k - 1)] + 1] <- TRUE
  }
  risk_set
}






#' Log (sum exp risks at risk)
#'
#' Compute the internal sum of the loss function over the subject at
#' risk for any give subject/time.
#'
#'
#' @param risk_scores (2D numerical tensor) single row 2D tensor with
#'   predicted risk scores
#' @param mask (2D logical tensor) squared tensor/matrix with both
#'   dimension equal to the number of columns in [risk_scores]. Every
#'   row represent a subject, in their column are reported indicators
#'   for subjects at risk at the time-of-event/censoring time for the
#'   corresponding reference subject.
#' @param axis (int, default is 0) Python 0-indexed axis representing
#'   the subjects (i.e batches)
#' @param keepdims (logical, default is FALSE) would you like to
#'   collapse the result the subject dimension (i.e. across the [axis])?
#'
#' @return 1D numerical tensor reporting the log of the internal sum of
#'   the partial likelihood equation for the chcox nonlinear model if
#'   [keepdims] is FALSE, or a 2D tensor otherwise.
#' @export
#'
#' @examples
#' time <- c(12, 15, 37, 24, 39, 14, 11)
#' risk_scores <- np$array(array(time, dim = c(length(time), 1)))
#' mask <- make_riskset(time)
#' logsumexp_masked(risk_scores, mask) # shape = (7)
#' logsumexp_masked(risk_scores, mask, keepdims = TRUE) # shape = (1, 7)
logsumexp_masked <- function(
    risk_scores,
    mask,
    axis = 0L,
    keepdims = FALSE
) {
  mask_f <- tf$cast(mask, risk_scores$dtype)
  risk_scores_masked <- tf$math$multiply(risk_scores, mask_f)
  amax <- tf$reduce_max(
    risk_scores_masked,
    axis = axis,
    keepdims = TRUE
  )

  # risk_scores_shift <- risk_scores_masked
  risk_scores_shift <- tf$math$subtract(risk_scores_masked, amax)

  exp_masked <- tf$math$multiply(tf$math$exp(risk_scores_shift),  mask_f)
  exp_sum <- tf$reduce_sum(
    exp_masked,
    axis = axis,
    keepdims = TRUE
  )
  # out <- tf$math$log(exp_sum)
  out <- amax + tf$math$log(exp_sum)

  if (!keepdims) {
    out <- tf$squeeze(out, axis = axis)
  }
  out
}
