# To view your current pipeline status, and possibly run/update it,
# you can simply source/execute this script (maybe using the
# `dev/03-run_cycle.R` script)
{
  optimal_n_cores <- function(
    available_cores,
    left_free = min(2, available_cores - 1)
  ) {
    max_usable <- max(1, available_cores - left_free)
    nearest_power_2 <- trunc(log2(max_usable))
    2^nearest_power_2
  }

  function(proceed = TRUE, save_all = TRUE) {
    if (interactive()) {
      if (
        requireNamespace("rstudioapi") &&
        rstudioapi::isAvailable() &&
        save_all
      ) {
        rstudioapi::documentSaveAll()
      }

      targets::tar_visnetwork() |>
        print()

      proceed <- utils::menu(
        title = "How do you want to proceed? (0 to exit)",
        c("tar_make", "tar_make_future")
      )
    }

    withr::with_envvar(
      list(RSTUDIO_VERSION = "2021.09.0"), {
        # devtools::test(stop_on_failure = TRUE)
        switch(proceed + 1,
          return(usethis::ui_oops("tar network update interrupted.")),
          targets::tar_make(),
          targets::tar_make_future(
            workers = optimal_n_cores(future::availableCores())
          )
        )
      }
    )
    usethis::ui_done("tar network update completed.")
    targets::tar_visnetwork(targets_only = TRUE) |>
      print()
  }
}()

