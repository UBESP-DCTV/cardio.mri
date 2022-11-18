transposed <- function(x) {
  nms <- purrr::map(x, names) |> purrr::reduce(union)
  purrr::transpose(x, .names = nms)
}






merge_cine_short <- function(x, dims = c(30, 640, 480, 25)) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, ~is.array(.x) || is.null(.x))))

  n <- length(x)
  res <- array(
    NA_integer_,
    dim = c(n, dims),
    dimnames = list(names(x), rep(NULL, length(dims)))
  )

  for (i in seq_len(n)) {

    res[i, , , , ] <- if (is.null(x[[i]])) {
      array(-1L, dims)
    } else {
      pad_array(x[[i]], dims)
    }
    gc(FALSE, TRUE)
    gc(FALSE, TRUE)
  }
  res
}

merge_cine_long <- function(x, dims = c(30, 640, 480)) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, ~is.array(.x) || is.null(.x))))

  n <- length(x)
  res <- array(
    NA_integer_,
    dim = c(n, dims),
    dimnames = list(names(x), rep(NULL, length(dims)))
  )

  for (i in seq_len(n)) {
    res[i, , , ] <- if (is.null(x[[i]])) {
      array(-1L, dims)
    } else {
      pad_array(x[[i]], dims)
    }
    gc(FALSE, TRUE)
    gc(FALSE, TRUE)
  }
  res
}



merge_lge_short <- function(x, dims = c(640, 480, 25)) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, ~is.array(.x) || is.null(.x))))

  n <- length(x)
  res <- array(
    NA_integer_,
    dim = c(n, dims),
    dimnames = list(names(x), rep(NULL, length(dims)))
  )

  for (i in seq_len(n)) {

    res[i, , , ] <- if (is.null(x[[i]])) {
      array(-1L, dims)
    } else {
      pad_array(x[[i]], dims)
    }
    gc(FALSE, TRUE)
    gc(FALSE, TRUE)
  }
  res
}

merge_lge_long <- function(x, dims = c(256, 256)) {
  stopifnot(is.list(x))
  stopifnot(all(purrr::map_lgl(x, ~is.array(.x) || is.null(.x))))

  n <- length(x)
  res <- array(
    NA_integer_,
    dim = c(n, dims),
    dimnames = list(names(x), rep(NULL, length(dims)))
  )

  for (i in seq_len(n)) {

    res[i, , ] <- if (is.null(x[[i]])) {
      array(-1L, dims)
    } else {
      pad_array(x[[i]], dims)
    }
    gc(FALSE, TRUE)
    gc(FALSE, TRUE)
  }
  res
}




pad_array <- function(
    original_array = array(pad_value),
    out_dims = NULL,
    pad_value = -1L
) {
  if (is.null(out_dims)) return(original_array)
  stopifnot(is.array(original_array))
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
