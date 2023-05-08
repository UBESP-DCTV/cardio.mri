cardio_roc <- function(h, t = 60, set) {
  aux <- h |>
    dplyr::filter(time <= t) |>
    dplyr::group_by(target_id) |>
    dplyr::filter(time == max(time)) |>
    dplyr::ungroup() |>
    dplyr::mutate(current_event = event & (event_time <= time))

  rocobj <- pROC::roc(aux$current_event, aux$h)
  auc <- round(pROC::auc(aux$current_event, aux$h), 4)

  pROC::ggroc(rocobj, colour = 'steelblue', size = 1) +
    ggplot2::ggtitle(
      paste0(set, ' ', t, ' months ROC Curve ', '(AUC = ', auc, ')')
    ) +
    ggplot2::theme_minimal()
}
