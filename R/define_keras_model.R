define_keras_model <- function(with_clinic = TRUE, debug = FALSE) {
  # cine ------------------------------------------------------------
  input_s_cine <- keras::layer_input(
    name = "input_s_cine",
    shape = c(30, 640, 480, 25)
  )

  input_l2c_cine <- keras::layer_input(
    name = "input_l2c_cine",
    shape = c(30, 640, 480, 1)
  )
  input_l3c_cine <- keras::layer_input(
    name = "input_l3c_cine",
    shape = c(30, 640, 480, 1)
  )
  input_l4c_cine <- keras::layer_input(
    name = "input_l4c_cine",
    shape = c(30, 640, 480, 1)
  )


  input_cine <- keras::layer_concatenate(
    c(input_s_cine, input_l2c_cine, input_l3c_cine, input_l4c_cine),
    axis = 4,
    name = "input_cine"
  ) %>%
    keras::layer_rescaling(
      name = "rescale_cine",
      scale = 1.0 / 255,
      offset = 1e-7
    ) %>%
    keras::layer_dropout(rate = 0.2)

  cine_l1 <- input_cine %>%
    keras::layer_conv_lstm_2d(
      name = "cine_timeflat",
      filters = 32L,
      kernel_size = 4L,
      padding = "same",
      input_shape = c(30L, 640L, 480L, 28L),
      activation = "relu",
      dropout = 0.7,
      recurrent_dropout = 0.7
    )

  cine_l1_padded <- cine_l1 %>%
    keras::layer_zero_padding_2d(
      name = "cine_padding",
      padding = list(list(63L, 64L), list(16L, 16L)),
      data_format = "channels_last"
    )

  cine_l2 <- cine_l1_padded %>%
    keras::layer_max_pooling_2d(
      pool_size = c(2L, 2L),
      strides = c(3L, 2L),
      name = "pooling_cine"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(rate = 0.7)

  # lge -------------------------------------------------------------
  input_s_lge <- layer_input(
    name = "input_s_lge",
    shape = c(640, 480, 25)
  )

  input_s_lge_padded <- input_s_lge  %>%
    keras::layer_zero_padding_2d(
      name = "lge_padding",
      padding = list(list(63L, 64L), list(16L, 16L)),
      data_format = "channels_last"
    )

  input_s_lge_pooled <- input_s_lge_padded %>%
    keras::layer_max_pooling_2d(
      pool_size = c(2L, 2L),
      strides = c(3L, 2L),
      name = "pooling_lge"
    )

  input_l2c_lge <- layer_input(
    name = "input_l2c_lge",
    shape = c(256, 256, 1)
  )

  input_l3c_lge <- layer_input(
    name = "input_l3c_lge",
    shape = c(256, 256, 1)
  )

  input_l4c_lge <- layer_input(
    name = "input_l4c_lge",
    shape = c(256, 256, 1)
  )


  lge_l0 <- layer_concatenate(
    c(input_s_lge_pooled, input_l2c_lge, input_l3c_lge, input_l4c_lge),
    axis = 3,
    name = "lge_l0"
  ) %>%
    keras::layer_rescaling(
      name = "rescale_lge",
      scale = 1.0 / 255,
      offset = 1e-7
    ) %>%
    keras::layer_dropout(rate = 0.2)

  lge_l1 <- lge_l0 %>%
    keras::layer_conv_2d(
      name = "lge_l1",
      filters = 32L,
      kernel_size = 4L,
      padding = "same",
      input_shape = c(256L, 256L, 28L),
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(rate = 0.7)



  # shared ----------------------------------------------------------
  shared_cnn_l1 <- keras::layer_conv_2d(
    name = "shared_cnn_l1",
    filters = 16L,
    kernel_size = 2L,
    padding = "same",
    input_shape = c(256L, 256L, 32L),
    activation = "relu"
  )

  cine_l3 <- cine_l2 %>%
    shared_cnn_l1()
  lge_l2 <- lge_l1 %>%
    shared_cnn_l1()

  merged_l1 <- keras::layer_concatenate(
    c(cine_l3, lge_l2),
    axis = 3,
    name = "merged_l1"
  ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(rate = 0.7)

  merged_l2 <- merged_l1 %>%
    keras::layer_conv_2d(
      name = "merged_l2",
      filters = 16L,
      kernel_size = 2L,
      padding = "same",
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(rate = 0.7) %>%
    keras::layer_flatten(name = "flatting")


  # clinic ----------------------------------------------------------

  input_clinic <- keras::layer_input(
    name = "input_clinic",
    shape = c(18)
  )

  clinic_scaled <- input_clinic %>%
    keras::layer_normalization(
      name = "rescale_clinic",
      mean = c( # rescale binary factors to 0-1
        0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      variance = c( # scale by approximate maximum to radius approx 1
        100, 1, 200, 2.5, 1, 1, 1, 1, 1, 1, 1, 1, 200, 4, 20000, 1, 1, 1
      )^2
    ) %>%
    keras::layer_dropout(rate = 0.2)

  clinic_l1 <- clinic_scaled %>%
    keras::layer_dense(
      name = "clinic_l1",
      unit = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() |>
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(0.125)

  clinic_l2 <- clinic_l1 %>%
    keras::layer_dense(
      name = "clinic_l2",
      unit = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() |>
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(0.125)


  # output ----------------------------------------------------------

  dense_l1 <- merged_l2 %>%
    keras::layer_dense(
      name = "dense_l1",
      units = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() |>
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dropout(0.125)





  merged_l3 <- keras::layer_concatenate(
    c(dense_l1, clinic_l2),
    name = "merged_l3"
  )


  last_merged <- if (with_clinic) merged_l3 else dense_l1


  dense_l2 <- last_merged %>%
    keras::layer_dense(
      name = "dense_l2",
      units = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

  dense_l3 <- dense_l2 %>%
    keras::layer_dense(
      name = "dense_l3",
      units = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)


  last_hidden <- if (with_clinic) dense_l3 else dense_l2

  output <- last_hidden %>%
    keras::layer_dense(
      name = "output",
      units = 1,
      activation = "linear"
    )


  # debug -----------------------------------------------------------

  output_debug <- input_s_cine %>%
    keras::layer_rescaling(scale = 1.0 / 255, offset = 1e-7) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_conv_lstm_2d(
      name = "cine_timeflat",
      filters = 1L,
      kernel_size = 10L,
      strides = 10
    ) %>% # merged_l1 %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_max_pooling_2d(pool_size = c(8, 6)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_flatten(name = "flatting") %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dense(
      name = "output",
      units = 1,
      activation = "linear"
    )


  # model -----------------------------------------------------------

  keras::keras_model(
    inputs = c(
      input_s_cine,
      if (!debug) {
        c(input_l2c_cine, input_l3c_cine, input_l4c_cine, input_s_lge,
          input_l2c_lge, input_l3c_lge, input_l4c_lge
        )
      },
      if (with_clinic) input_clinic
    ),
    # outputs = output_debug
    outputs = output
  )
}
