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
    tidyr::unite(
      "name",
      dplyr::all_of(c("generalita_cognome", "generalita_nome")),
      remove = FALSE,
      na.rm = TRUE
    ) |>
    dplyr::mutate(
      name = stringr::str_replace_all(.data[["name"]], " +", "_"),
      outcome = x[["follow_up_aritmia_ventricolare"]] == 1,
      fup = x[["follow_up_mesi_follow_up"]]
    ) |>
    dplyr::select(
      -dplyr::all_of(c(
        "generalita_cognome", "generalita_nome",
        "follow_up_aritmia_ventricolare", "follow_up_mesi_follow_up"
      ))
    )
}

match_mri_out <- function(mri, clinical) {
  current_name <- attributes(mri[[1]])[["mri_info"]][["name"]] |>
    stringr::str_replace_all(" +", "_")
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
      dplyr::select(-dplyr::all_of(c("outcome", "fup")))
    mri$output <- case_output |>
      dplyr::select(dplyr::all_of(c("outcome", "fup")))
    mri
}

select_clinical_interest <- function(x) {
  x |>
    dplyr::select(dplyr::all_of(c(
      "generalita_cognome",
      "generalita_nome",

      "generalita_eta_auto",
      "generalita_sesso",
      "generalita_peso_kg",
      "generalita_altezza_m",

      "generalita_dislipidemia",
      "generalita_ipertensione_arteriosa",
      "generalita_fumatore",
      "generalita_diabete_mellito",
      "generalita_familiarita_cad",
      "generalita_familiarita_cmp",
      "generalita_familiarita_per_scd",
      "generalita_bpco",
      "generalita_creatinina_umol_l",
      "generalita_nyha_rm",
      "laboratorio_nt_pro_bnp_pg_l",
      "ecg_device_ritmo_sinusale",
      "ecg_device_fibrillazione_flutter_atriale",
      "ecg_device_bb_sx",
      "follow_up_data_follow_up_aritmia",
      "follow_up_aritmia_ventricolare",
      "follow_up_mesi_follow_up"
    )))
}
