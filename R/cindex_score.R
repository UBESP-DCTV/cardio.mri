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
