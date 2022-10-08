read_tabular <- function(tabular_path) {
  readxl::read_xlsx(
      tabular_path,
      col_names = FALSE,
      col_types = "text"
    ) |>
    unheadr::mash_colnames(
      n_name_rows = 3,
      keep_names = FALSE,
      sliding_headers = TRUE
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        readr::parse_guess,
        guess_integer = TRUE
      ),
      dplyr::across(
        dplyr::contains("data"),
        ~lubridate::as_date(as.numeric(.x), origin = "1899-12-30")
      )
    )
}
