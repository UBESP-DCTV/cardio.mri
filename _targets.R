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
    qs = tar_resources_qs(preset = "fast")
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
    idx,
    {
      all_idx <- seq_along(patientsFolders)

      test_idx <- all_idx |>
        sample(size = floor(0.3 * length(all_idx)))

      train_val_idx <- setdiff(all_idx, test_idx)

      val_idx <- train_val_idx |>
        sample(size = floor(0.3 * length(train_val_idx)))

      train_idx <- setdiff(train_val_idx, val_idx)
      list(
        train_idx = train_idx, val_idx = val_idx, test_idx = test_idx
      )
    }
  ),

  tar_target(trainIdx, idx[["train_idx"]]),
  tar_target(valIdx, idx[["val_idx"]]),
  tar_target(testIdx, idx[["test_idx"]]),


  tar_target(
    cine1Keras_train,
    merge_cine_short(matchedT[["cine-1"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    cine1Keras_val,
    merge_cine_short(matchedT[["cine-1"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    cine1Keras_test,
    merge_cine_short(matchedT[["cine-1"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    cine2Keras_train,
    merge_cine_long(matchedT[["cine-2"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    cine2Keras_val,
    merge_cine_long(matchedT[["cine-2"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    cine2Keras_test,
    merge_cine_long(matchedT[["cine-2"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    cine3Keras_train,
    merge_cine_long(matchedT[["cine-3"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    cine3Keras_val,
    merge_cine_long(matchedT[["cine-3"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    cine3Keras_test,
    merge_cine_long(matchedT[["cine-3"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    cine4Keras_train,
    merge_cine_long(matchedT[["cine-4"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    cine4Keras_val,
    merge_cine_long(matchedT[["cine-4"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    cine4Keras_test,
    merge_cine_long(matchedT[["cine-4"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    lge1Keras_train,
    merge_lge_short(matchedT[["lge-1"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    lge1Keras_val,
    merge_lge_short(matchedT[["lge-1"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    lge1Keras_test,
    merge_lge_short(matchedT[["lge-1"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    lge2Keras_train,
    merge_lge_long(matchedT[["lge-2"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    lge2Keras_val,
    merge_lge_long(matchedT[["lge-2"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    lge2Keras_test,
    merge_lge_long(matchedT[["lge-2"]][testIdx]),
    format = "qs"
  ),


  tar_target(
    lge3Keras_train,
    merge_lge_long(matchedT[["lge-3"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    lge3Keras_val,
    merge_lge_long(matchedT[["lge-3"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    lge3Keras_test,
    merge_lge_long(matchedT[["lge-3"]][testIdx]),
    format = "qs"
  ),

  tar_target(
    lge4Keras_train,
    merge_lge_long(matchedT[["lge-4"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    lge4Keras_val,
    merge_lge_long(matchedT[["lge-4"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    lge4Keras_test,
    merge_lge_long(matchedT[["lge-4"]][testIdx]),
    format = "qs"
  ),

  tar_target(
    clinicKeras_train,
    merge_clinic(matchedT[["clinical"]][trainIdx]),
    format = "qs"
  ),
  tar_target(
    clinicKeras_val,
    merge_clinic(matchedT[["clinical"]][valIdx]),
    format = "qs"
  ),
  tar_target(
    clinicKeras_test,
    merge_clinic(matchedT[["clinical"]][testIdx]),
    format = "qs"
  ),

  tar_files_input(kerasCovarPath, get_keras_model_path("covar")),
  # tar_files_input(kerasNoCovarPath, get_keras_model_path("no-covar")),


  tar_target(
    predTrainCovar,
    pred_keras(kerasCovarPath, "train"),
    pattern = map(kerasCovarPath),
    iteration = "list"
  ),
  tar_target(
    predValCovar,
    pred_keras(kerasCovarPath, "val"),
    pattern = map(kerasCovarPath),
    iteration = "list"
  ),
  tar_target(
    predTestCovar,
    pred_keras(kerasCovarPath, "test"),
    pattern = map(kerasCovarPath),
    iteration = "list"
  ),
  # tar_target(
  #   predTrainNoCovar,
  #   pred_keras(kerasNoCovarPath, "train"),
  #   pattern = map(kerasNoCovarPath),
  #   iteration = "list"
  # ),
  # tar_target(
  #   predValNoCovar,
  #   pred_keras(kerasNoCovarPath, "val"),
  #   pattern = map(kerasNoCovarPath),
  #   iteration = "list"
  # ),
  # tar_target(
  #   predTestNoCovar,
  #   pred_keras(kerasNoCovarPath, "test"),
  #   pattern = map(kerasNoCovarPath),
  #   iteration = "list"
  # ),


  tar_target(
    h0TrainCovar,
    baseline_hazard(
      predTrainCovar$time,
      predTrainCovar$event,
      predTrainCovar$risk_score
    ),
    pattern = map(predTrainCovar),
    iteration = "list"
  ),
  tar_target(
    h0ValCovar,
    baseline_hazard(
      predValCovar$time,
      predValCovar$event,
      predValCovar$risk_score
    ),
    pattern = map(predValCovar),
    iteration = "list"
  ),
  tar_target(
    h0TestCovar,
    baseline_hazard(
      predTestCovar$time,
      predTestCovar$event,
      predTestCovar$risk_score
    ),
    pattern = map(predTestCovar),
    iteration = "list"
  ),
  # tar_target(
  #   h0TrainNoCovar,
  #   baseline_hazard(
  #     predTrainNoCovar$time,
  #     predTrainNoCovar$event,
  #     predTrainNoCovar$risk_score
  #   ),
  #   pattern = map(predTrainNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   h0ValNoCovar,
  #   baseline_hazard(
  #     predValNoCovar$time,
  #     predValNoCovar$event,
  #     predValNoCovar$risk_score
  #   ),
  #   pattern = map(predValNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   h0TestNoCovar,
  #   baseline_hazard(
  #     predTestNoCovar$time,
  #     predTestNoCovar$event,
  #     predTestNoCovar$risk_score
  #   ),
  #   pattern = map(predTestNoCovar),
  #   iteration = "list"
  # ),

  tar_target(
    hTrainCovar,
    hazard(predTrainCovar, h0TrainCovar),
    pattern = map(predTrainCovar, h0TrainCovar),
    iteration = "list"
  ),
  tar_target(
    hValCovar,
    hazard(predValCovar, h0ValCovar),
    pattern = map(predValCovar, h0ValCovar),
    iteration = "list"
  ),
  tar_target(
    hTestCovar,
    hazard(predTestCovar, h0TestCovar),
    pattern = map(predTestCovar, h0TestCovar),
    iteration = "list"
  ),
  # tar_target(
  #   hTrainNoCovar,
  #   hazard(predTrainNoCovar, h0TrainNoCovar),
  #   pattern = map(predTrainNoCovar, h0TrainNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   hValNoCovar,
  #   hazard(predValNoCovar, h0ValNoCovar),
  #   pattern = map(predValNoCovar, h0ValNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   hTestNoCovar,
  #   hazard(predTestNoCovar, h0TestNoCovar),
  #   pattern = map(predTestNoCovar, h0TestNoCovar),
  #   iteration = "list"
  # ),


  tar_target(
    HarrellCTrainCovar,
    c_index(predTrainCovar),
    pattern = map(predTrainCovar),
    iteration = "list"
  ),
  tar_target(
    HarrellCValCovar,
    c_index(predValCovar),
    pattern = map(predValCovar),
    iteration = "list"
  ),
  tar_target(
    HarrellCTestCovar,
    c_index(predTestCovar),
    pattern = map(predTestCovar),
    iteration = "list"
  ),
  # tar_target(
  #   HarrellCTrainNoCovar,
  #   c_index(predTrainNoCovar),
  #   pattern = map(predTrainNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   HarrellCValNoCovar,
  #   c_index(predValNoCovar),
  #   pattern = map(predValNoCovar),
  #   iteration = "list"
  # ),
  # tar_target(
  #   HarrellCTestNoCovar,
  #   c_index(predTestNoCovar),
  #   pattern = map(predTestNoCovar),
  #   iteration = "list"
  # ),


  tar_target(
    HarrellCs,
    c_indexs(list(
      train_covar = HarrellCTrainCovar,
      val_covar = HarrellCValCovar,
      test_covar = HarrellCTestCovar#,
      # train_nocovar = HarrellCTrainNoCovar,
      # val_nocovar = HarrellCValNoCovar,
      # test_nocovar = HarrellCTestNoCovar
    )),
    pattern = map(
      HarrellCTrainCovar, HarrellCValCovar, HarrellCTestCovar#,
      # HarrellCTrainNoCovar, HarrellCValNoCovar, HarrellCTestNoCovar
    ),
    iteration = "list"
  ),

  tar_target(
    valRoc1Covar,
    cardio_roc(hValCovar, 12, "Validation"),
    pattern = map(hValCovar),
    iteration = "list"
  ),
  tar_target(
    valRoc2Covar,
    cardio_roc(hValCovar, 24, "Validation"),
    pattern = map(hValCovar),
    iteration = "list"
  ),
  tar_target(
    valRoc3Covar,
    cardio_roc(hValCovar, 36, "Validation"),
    pattern = map(hValCovar),
    iteration = "list"
  ),
  tar_target(
    valRoc5Covar,
    cardio_roc(hValCovar, 60, "Validation"),
    pattern = map(hValCovar),
    iteration = "list"
  ),
  tar_target(
    valRoc8Covar,
    cardio_roc(hValCovar, 96, "Validation"),
    pattern = map(hValCovar),
    iteration = "list"
  ),

  tar_target(
    testRoc1Covar,
    cardio_roc(hTestCovar, 12, "Test"),
    pattern = map(hTestCovar),
    iteration = "list"
  ),
  tar_target(
    testRoc2Covar,
    cardio_roc(hTestCovar, 24, "Test"),
    pattern = map(hTestCovar),
    iteration = "list"
  ),
  tar_target(
    testRoc3Covar,
    cardio_roc(hTestCovar, 36, "Test"),
    pattern = map(hTestCovar),
    iteration = "list"
  ),
  tar_target(
    testRoc5Covar,
    cardio_roc(hTestCovar, 60, "Test"),
    pattern = map(hTestCovar),
    iteration = "list"
  ),
  tar_target(
    testRoc8Covar,
    cardio_roc(hTestCovar, 96, "Test"),
    pattern = map(hTestCovar),
    iteration = "list"
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
