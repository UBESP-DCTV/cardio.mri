define_keras_model <- function(with_clinic = TRUE, debug = FALSE) {

  conv <- function(
    ..., padding = "same", activation = "relu", separable = TRUE
  ) {
    layer <- if (separable) {
      keras::layer_separable_conv_2d
    } else {
      keras::layer_conv_2d
    }

    layer(..., padding = padding, use_bias = FALSE) |>
      keras::layer_batch_normalization() |>
      keras::layer_activation(activation = activation)
  }

  residual_conv_block <- function(
    x, filters, kernel_size, strides = c(1L, 1L), ..., separable = TRUE
  ) {
    layer <- if (separable) {
      keras::layer_separable_conv_2d
    } else {
      keras::layer_conv_2d
    }

    residual <- x

    x <- x |>
      conv(
        filters = filters,
        kernel_size = kernel_size,
        strides = strides,
        ...,
        separable = separable
      )

    if (filters != dim(residual)[4]) {
      residual <- residual |>
        layer(
          filters = filters,
          kernel_size = 1L,
          strides = strides
        )
    }

    keras::layer_add(list(x, residual))
  }



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
  ) |>
    keras::layer_rescaling(scale = 1.0 / 255)

  residual_input_cine <- input_cine |>
    keras::layer_conv_lstm_2d(
      filters = 32L,
      kernel_size = 1L,
      strides = 2L,
      padding = "same",
      input_shape = c(30L, 640L, 480L, 28L)
    )

  cine_timeflat <- input_cine |>
    keras::layer_conv_lstm_2d(
      name = "cine_timeflat",
      filters = 32L,
      kernel_size = 3L,
      strides = 2L,
      padding = "same",
      activation = "linear",
      input_shape = c(30L, 640L, 480L, 28L)
    ) |>
    keras::layer_batch_normalization() |>
    keras::layer_activation(activation = "tanh")

  cine <- keras::layer_add(list(cine_timeflat, residual_input_cine)) |>
    residual_conv_block(32L, 3L) |>
    residual_conv_block(64L, 3L, strides = 2L) |>
    residual_conv_block(64L, 3L) |>
    residual_conv_block(128L, 3L, strides = 2L) |>
    residual_conv_block(128L, 3L) |>
    residual_conv_block(256L, 3L, strides = 2L) |>
    residual_conv_block(256L, 3L) |>
    residual_conv_block(512L, 3L, strides = 2L) |>
    residual_conv_block(512L, 3L) |>
    keras::layer_global_average_pooling_2d() |>
    keras::layer_dropout(0.4)


  # lge -------------------------------------------------------------
  input_s_lge <- keras::layer_input(
    name = "input_s_lge",
    shape = c(640, 480, 25)
  )

  input_l2c_lge <- keras::layer_input(
    name = "input_l2c_lge",
    shape = c(256, 256, 1)
  )

  l2c_lge <- input_l2c_lge |>
    keras::k_resize_images(5, 15, "channels_last") |>
    keras::layer_average_pooling_2d(strides = c(2, 8))

  input_l3c_lge <- keras::layer_input(
    name = "input_l3c_lge",
    shape = c(256, 256, 1)
  )
  l3c_lge <- input_l3c_lge |>
    keras::k_resize_images(5, 15, "channels_last") |>
    keras::layer_average_pooling_2d(strides = c(2, 8))

  input_l4c_lge <- keras::layer_input(
    name = "input_l4c_lge",
    shape = c(256, 256, 1)
  )
  l4c_lge <- input_l4c_lge |>
    keras::k_resize_images(5, 15, "channels_last") |>
    keras::layer_average_pooling_2d(strides = c(2, 8))

  input_lge <- keras::layer_concatenate(
    c(input_s_lge, l2c_lge, l3c_lge, l4c_lge),
    axis = 3,
    name = "input_l_lge"
  )

  lge <- input_lge |>
    keras::layer_rescaling(scale = 1.0 / 255) |>
    residual_conv_block(32L, 3L, strides = 2L) |>
    residual_conv_block(32L, 3L) |>
    residual_conv_block(64L, 3L, strides = 2L) |>
    residual_conv_block(64L, 3L) |>
    residual_conv_block(128L, 3L, strides = 2L) |>
    residual_conv_block(128L, 3L) |>
    residual_conv_block(256L, 3L, strides = 2L) |>
    residual_conv_block(256L, 3L) |>
    residual_conv_block(512L, 3L, strides = 2L) |>
    residual_conv_block(512L, 3L) |>
    keras::layer_global_average_pooling_2d() |>
    keras::layer_dropout(0.4)

  # lge_l1 <- lge_l0 %>%
  #   keras::layer_conv_2d(
  #     name = "lge_l1",
  #     filters = 32L,
  #     kernel_size = 4L,
  #     padding = "same",
  #     input_shape = c(256L, 256L, 28L),
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()
  #
  #
  #
  # # shared ----------------------------------------------------------
  # shared_cnn_l1 <- keras::layer_conv_2d(
  #   name = "shared_cnn_l1",
  #   filters = 16L,
  #   kernel_size = 2L,
  #   padding = "same",
  #   input_shape = c(256L, 256L, 32L),
  #   activation = "relu"
  # )
  #
  # cine_l3 <- cine_l2 %>%
  #   shared_cnn_l1()
  # lge_l2 <- lge_l1 %>%
  #   shared_cnn_l1()
  #
  # merged_l1 <- keras::layer_concatenate(
  #   c(cine_l3, lge_l2),
  #   axis = 3,
  #   name = "merged_l1"
  # ) %>%
  #   keras::layer_batch_normalization()
  #
  # merged_l2 <- merged_l1 %>%
  #   keras::layer_conv_2d(
  #     name = "merged_l2",
  #     filters = 16L,
  #     kernel_size = 2L,
  #     padding = "same",
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization() %>%
  #   keras::layer_flatten(name = "flatting")


  # clinic ----------------------------------------------------------

  input_clinic <- keras::layer_input(
    name = "input_clinic",
    shape = c(18)
  )

  clinic <- input_clinic %>%
    keras::layer_normalization(
      name = "rescale_clinic",
      mean = c( # rescale binary factors to 0-1
        0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
      ),
      variance = c( # scale by approximate maximum to radius approx 1
        100, 1, 200, 2.5, 1, 1, 1, 1, 1, 1, 1, 1, 200, 4, 20000, 1, 1, 1
      )^2
    )

  # clinic_l1 <- clinic_scaled %>%
  #   keras::layer_dense(
  #     name = "clinic_l1",
  #     unit = 8,
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()
  #
  # clinic_l2 <- clinic_l1 %>%
  #   keras::layer_dense(
  #     name = "clinic_l2",
  #     unit = 8,
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()


  # output ----------------------------------------------------------

  # dense_l1 <- merged_l2 %>%
  #   keras::layer_dense(
  #     name = "dense_l1",
  #     units = 8,
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()





  merged <- keras::layer_concatenate(
    c(cine, lge, clinic),
    name = "merged"
  )


  # last_merged <- if (with_clinic) merged_l3 else dense_l1


  # dense_l2 <- merged %>%
  #   keras::layer_dense(
  #     name = "dense_l2",
  #     units = 8,
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()
  #
  # dense_l3 <- dense_l2 %>%
  #   keras::layer_dense(
  #     name = "dense_l3",
  #     units = 8,
  #     activation = "relu"
  #   ) %>%
  #   keras::layer_batch_normalization()
  #
  # last_hidden <- if (with_clinic) dense_l3 else dense_l2

  last_hidden <- merged

  output <- last_hidden %>%
    # keras::layer_dropout(0.4) |>
    keras::layer_dense(
      name = "output",
      units = 1,
      activation = "linear"
    )


  # debug -----------------------------------------------------------

  # output_debug <- input_s_cine %>%
  #   keras::layer_rescaling(scale = 1.0 / 255, offset = 1e-7) %>%
  #   keras::layer_batch_normalization() %>%
  #   keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  #   keras::layer_conv_lstm_2d(
  #     name = "cine_timeflat",
  #     filters = 1L,
  #     kernel_size = 10L,
  #     strides = 10
  #   ) %>% # merged_l1 %>%
  #   keras::layer_batch_normalization() %>%
  #   keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  #   keras::layer_max_pooling_2d(pool_size = c(8, 6)) %>%
  #   keras::layer_batch_normalization() %>%
  #   keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  #   keras::layer_flatten(name = "flatting") %>%
  #   keras::layer_batch_normalization() %>%
  #   keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
  #   keras::layer_dense(
  #     name = "output",
  #     units = 1,
  #     activation = "linear"
  #   )


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
