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


compose_clinical <- function(x) {
  x |>
    select_clinical_interest() |>
    dplyr::mutate(
      name = paste(
        .data[["generalita_cognome"]],
        .data[["generalita_nome"]]
      ) |> stringr::str_replace_all(" ", "_"),
      outcome = x[["follow_up_aritmia_ventricolare"]] == 1,
      fup = x[["follow_up_mesi_follow_up"]]
    ) |>
    dplyr::select(
      - .data[["generalita_cognome"]],
      - .data[["generalita_nome"]],
      - .data[["follow_up_aritmia_ventricolare"]],
      - .data[["follow_up_mesi_follow_up"]]
    )
}

match_mri_out <- function(mri, clinical) {
  current_name <- attributes(mri[[1]])[["mri_info"]][["name"]]
  case_output <- clinical |>
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

    mri$clinical <- case_output |>
      dplyr::select(
        -.data[["outcome"]],
        -.data[["fup"]]
      )
    mri$output <- case_output |>
      dplyr::select(
        .data[["outcome"]],
        .data[["fup"]]
      )
    mri
}

select_clinical_interest <- function(x) {
  x |>
    dplyr::select(
      .data[["generalita_cognome"]],
      .data[["generalita_nome"]],

      .data[["generalita_eta_auto"]],
      .data[["generalita_sesso"]],
      .data[["generalita_peso_kg"]],
      .data[["generalita_altezza_m"]],

      .data[["generalita_dislipidemia"]],
      .data[["generalita_ipertensione_arteriosa"]],
      .data[["generalita_fumatore"]],
      .data[["generalita_diabete_mellito"]],
      .data[["generalita_familiarita_cad"]],
      .data[["generalita_familiarita_cmp"]],
      .data[["generalita_familiarita_per_scd"]],
      .data[["generalita_bpco"]],
      .data[["generalita_creatinina_umol_l"]],
      .data[["generalita_nyha_rm"]],
      .data[["laboratorio_nt_pro_bnp_pg_l"]],
      .data[["ecg_device_ritmo_sinusale"]],
      .data[["ecg_device_fibrillazione_flutter_atriale"]],
      .data[["ecg_device_bb_sx"]],
      .data[["follow_up_data_follow_up_aritmia"]],
      .data[["follow_up_aritmia_ventricolare"]],
      .data[["follow_up_mesi_follow_up"]]
    )
}
