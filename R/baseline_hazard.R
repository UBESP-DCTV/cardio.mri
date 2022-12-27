#' Baseline Hazard function
#'
#' For non-linear Cox models, compute h0 baseline hazard function.
#'
#' @param time (int) right-censored event/censoring time
#' @param status (lgl, 0/1) individual indicator for event (TRUE/1) or
#'   censored (FALSE/0).
#' @param log_risk (dbl) non-linear estimation of the individual
#'   log-risk component of the risk function.
#'
#' @return a [tibble][tibble::tibble-package] with two columns and
#'   number of rows equal to number of distinct time-to-event for the
#'   event occurred. Columns report time (`time`) and the value (`h0`)
#'   of the corresponding baseline hazard function for that time.
#' @references Therneau, T. M., & Grambsch, P. M. (2000). Modeling
#' Survival Data: Extending the Cox Model. Springer. Sec. 10.2.3 p.266.
#' https://doi.org/10.1007/978-1-4757-3294-8
baseline_hazard <- function(time, status, log_risk) {
  event_time <- time[status == 1]
  tab_event_time <- data.frame(table(event_time))

  times <- as.numeric(levels(tab_event_time[[1L]]))
  perm <- ord(unique_times)

  ord_times <- times[perm]
  ord_n_events <- tab_event_time[[2L]][perm]

  h0 <- purrr::map_dbl(seq_along(ord_times), ~{
    ord_n_events[.x] / sum(exp(log_risk[time >= ord_times[.x]]))
  })
  h0_var <- purrr::map_dbl(seq_along(ord_times), ~{
    ord_n_events[.x] / (sum(exp(log_risk[time >= ord_times[.x]])))^2
  })

  tibble::tibble(
    time = ord_times,
    h0 = h0,
    sd = sqrt(h0_var)
  )
}
