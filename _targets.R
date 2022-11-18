library(targets)
library(tarchetypes)
library(future)
library("future.callr")
plan(callr)

list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


# Set target-specific options such as packages.
tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "high")
  ),
  garbage_collection = TRUE,
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
    {
      component_names <- purrr::map_chr(patientsMrisPaths, ~{
        get_info_from_filename(basename(.x))[c("type", "ch")] |>
          paste(collapse = "-")
      })
      names(patientsMrisPaths) <- component_names
      purrr::map(patientsMrisPaths, read_mri)
    },
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
  ),
  tar_target(matchedT, transposed(matched), format = "qs"),


  tar_target(
    testIndeces,
    sample(seq_along(matched), size = floor(0.3 * length(matched)))
  ),


  tar_target(
    cine1Keras_train,
    merge_cine_short(matchedT[["cine-1"]][-testIndeces]),
    format = "qs"
  ),
  tar_target(
    cine1Keras_test,
    merge_cine_short(matchedT[["cine-1"]][testIndeces]),
    format = "qs"
  ),


  tar_target(
    cine2Keras_train,
    merge_cine_long(matchedT[["cine-2"]][-testIndeces]),
    format = "qs"
  ),
  tar_target(
    cine2Keras_test,
    merge_cine_long(matchedT[["cine-2"]][testIndeces]),
    format = "qs"
  ),


  tar_target(
    cine3Keras_train,
    merge_cine_long(matchedT[["cine-3"]][-testIndeces]),
    format = "qs"
  ),
  tar_target(
    cine3Keras_test,
    merge_cine_long(matchedT[["cine-3"]][testIndeces]),
    format = "qs"
  ),


tar_target(
  cine4Keras_train,
  merge_cine_long(matchedT[["cine-4"]][-testIndeces]),
  format = "qs"
),
tar_target(
  cine4Keras_test,
  merge_cine_long(matchedT[["cine-4"]][testIndeces]),
  format = "qs"
),


tar_target(
  lge1Keras_train,
  merge_lge_short(matchedT[["lge-1"]][-testIndeces]),
  format = "qs"
),
tar_target(
  lge1Keras_test,
  merge_lge_short(matchedT[["lge-1"]][testIndeces]),
  format = "qs"
),


tar_target(
  lge2Keras_train,
  merge_lge_long(matchedT[["lge-2"]][-testIndeces]),
  format = "qs"
),
tar_target(
  lge2Keras_test,
  merge_lge_long(matchedT[["lge-2"]][testIndeces]),
  format = "qs"
),


tar_target(
  lge3Keras_train,
  merge_lge_long(matchedT[["lge-3"]][-testIndeces]),
  format = "qs"
),
tar_target(
  lge3Keras_test,
  merge_lge_long(matchedT[["lge-3"]][testIndeces]),
  format = "qs"
),

tar_target(
  lge4Keras_train,
  merge_lge_long(matchedT[["lge-4"]][-testIndeces]),
  format = "qs"
),
tar_target(
  lge4Keras_test,
  merge_lge_long(matchedT[["lge-4"]][testIndeces]),
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
