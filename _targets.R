library(targets)
library(tarchetypes)

list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Set target-specific options such as packages.
tar_option_set(
  error = "continue",
  workspace_on_error = TRUE
)

# End this file with a list of target objects.
list(

  # Import your file from custom (shared) location, and preprocess them
  tar_files_input(
    patientsFolders,
    list.dirs(get_input_data_path(), recursive = FALSE)
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
    iteration = "list"
  ),

  # compile your report
  tar_render(report, here::here("reports/report.Rmd"))


  # Decide what to share with other, and do it in a standard RDS format
  # tar_target(
  #   objectToShare,
  #   list(mris = mris)
  # ),
  # tar_target(
  #   shareOutput,
  #   share_objects(objectToShare),
  #   format = "file",
  #   pattern = map(objectToShare)
  # )
)

# if (!length(tar_errored())) tar_destroy(destroy = "workspaces")
