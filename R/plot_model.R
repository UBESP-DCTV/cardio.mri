#' Plot keras model
#'
#' Use
keras:::plot.keras.engine.training.Model
#'
#' @return
#' @export
#'
#'
plot_model <- function(
  model,
  to_file = NULL,
  show_shapes = TRUE,
  show_layer_names = TRUE,
  expand_nested = TRUE,
  show_layer_activations = TRUE
) {
  model |>
    plot(
      dpi = 192,
      show_shapes = show_shapes,
      show_layer_names = show_layer_names,
      expand_nested = expand_nested,
      show_layer_activations = show_layer_activations
    )
}
