read_tabular <- function(tabular_path) {
  readxl::read_xlsx(
      tabular_path,
      col_names = FALSE,
      col_types = "text",
      na = c("", " ", "-")
    ) |>
    suppressMessages() |>
    unheadr::mash_colnames(
      n_name_rows = 3,
      keep_names = FALSE,
      sliding_headers = TRUE
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(where(is.character), tolower),
      dplyr::across(
        dplyr::everything(),
        readr::parse_guess,
        guess_integer = TRUE
      ),
      dplyr::across(
        dplyr::contains("data"),
        ~lubridate::as_date(as.numeric(.x), origin = "1899-12-30")
      ),
      dplyr::across(where(~all(.x %in% c(0, 1, NA))), as.logical),
      generalita_sesso = .data[["generalita_sesso"]] |>
        factor(labels = c("maschio", "femmina"))
    )
}


compose_outcome <- function(x) {
  x |>
    dplyr::transmute(
      name = paste(
        x[["generalita_cognome"]],
        x[["generalita_nome"]]
      ) |> stringr::str_replace_all(" ", "_"),
      outcome = x[["follow_up_aritmia_ventricolare"]] == 1,
      fup = x[["follow_up_mesi_follow_up"]]
    )
}

match_mri_out <- function(mri, out) {
  current_name <- attributes(mri[[1]])[["mri_info"]][["name"]]
  case_output <- out |>
    dplyr::filter(
      purrr::map_lgl(
        .data[["name"]],
        ~{ # one of them could be shorter for multiple first names
          stringr::str_detect(.x, current_name) ||
          stringr::str_detect(current_name, .x)
        }
      )
    )

    if (nrow(case_output) < 1) {
      usethis::ui_warn("No match for name {current_name}")
    }
    if (nrow(case_output) > 1) {
      usethis::ui_warn(
        "More than one match for name {current_name}.
        Only the first will be considered.
        "
      )
      case_output <- case_output[1, ]
    }

    mri$out <- case_output
    mri
}
