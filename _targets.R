library(targets)
library(tarchetypes)
library(future)
library("future.callr")
plan(callr)

list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


# Set target-specific options such as packages.
tar_option_set(
  error = "continue",
  workspace_on_error = TRUE
)

# End this file with a list of target objects.
list(


# import signals --------------------------------------------------

  tar_files_input(
    patientsFolders,
    get_input_data_path("2022-08-01_mri") |>
      list.dirs(recursive = FALSE)
  ),

  tar_target(
    patientsMrisPaths,
    normalizePath(list.files(
      patientsFolders,
      pattern = "\\.(png|avi)",
      full.names = TRUE
    )),
    pattern = map(patientsFolders),
    format = "file"
  ),

  tar_target(
    mris,
    purrr::map(patientsMrisPaths, read_mri),
    pattern = map(patientsMrisPaths),
    iteration = "list",
    format = "qs"
  ),


# Import tabular --------------------------------------------------

  tar_target(
    tabularPath,
    get_input_data_path("mri_tabular_latest.xlsx"),
    format = "file"
  ),

  tar_target(tabular_raw, read_tabular(tabularPath)),
  tar_target(clinical, compose_clinical(tabular_raw)),

  tar_target(
    matched,
    match_mri_out(mris, clinical),
    pattern = map(mris),
    iteration = "list",
    format = "qs"
  )



# Report ----------------------------------------------------------

  # compile your report
  # tar_render(report, here::here("reports/report.Rmd")),


# Share objects ---------------------------------------------------

  # Decide what to share with other, and do it in a standard RDS format
  # tar_target(
  #   objectToShare,
  #   list(mris = mris)  # 50 GB...
  # ),
  # tar_target(
  #   shareOutput,
  #   share_objects(objectToShare),
  #   format = "file",
  #   pattern = map(objectToShare)
  # )
)

# if (!length(tar_errored())) tar_destroy(destroy = "workspaces")
