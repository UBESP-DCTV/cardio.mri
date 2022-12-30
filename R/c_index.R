c_index <- function(mri_pred) {
  Hmisc::rcorr.cens(
    mri_pred[["risk_score"]],
    survival::Surv(mri_pred[["time"]], mri_pred[["event"]])
  )
}

c_indexs <- function(idx) {
  dplyr::bind_rows(idx, .id = "set")
}
