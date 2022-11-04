first_keras <- function() {
  NULL
}


for_keras <- function(x) {
  purrr::transpose(x)
}


pad_array <- function(
    original_array,
    out_dims = NULL,
    pad_value = -1
) {
  if (is.null(out_dims)) return(original_array)
  original_array <- array(original_array)
  original_dim <- dim(original_array)

  original_pos <- do.call(
    expand.grid, purrr::map(original_dim, seq_len)
  ) |>
    as.matrix()

  out_pattern <- pad_value |>
    array(dim = out_dims, dimnames = dimnames(original_array))
  out_pattern[original_pos] <- original_array[original_pos]
  out_pattern
}
