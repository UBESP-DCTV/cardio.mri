get_info_from_filename <- function(filename) {


  component <- filename |>
    stringr::str_remove("\\.(avi|png)$") |>
    stringr::str_split("-", simplify = TRUE) |>
    as.list()

  if (!length(component) %in% c(3L, 5L)) {
    usethis::ui_stop(
      "Wrong number of components in {usethis::ui_value(filename)}"
    )
  }

  if (length(component) == 5) { # short cine
    names(component) <- c("name", "ch", "type", "t_max", "s_tot")
    component[["t_max"]] <- readr::parse_number(component[["t_max"]])
    component[["s_tot"]] <- readr::parse_number(component[["s_tot"]])
  } else  {
    names(component) <- c("name", "ch", "type")
  }

  if (component[["type"]] == "lge") {
    component[["t_max"]] <- 1L
  }

  component[["ch"]] <- readr::parse_number(component[["ch"]])
  if (component[["ch"]] != 1L) {
    component[["s_tot"]] <- 1L
  }

  # stop("component must be a list")

  component
}



read_mri <- function(mri_path) {

  mri_filename <- basename(mri_path)
  mri_info <- get_info_from_filename(mri_filename)

  channel <- as.character(mri_info[["ch"]])
  type <- mri_info[["type"]]

  if (length(type) * length(channel) == 0) {
    stop("type or channel are empty")
  }

  read_fun <- switch(channel,
    "1" = read_short,
    "2" = read_long,
    "3" = read_long,
    "4" = read_long,
    stop("Unknown channel provided.")
  )

  res <- read_fun(mri_path, type)
  attr(res, "mri_info") <- mri_info
  invisible(res)
}

read_short <- function(mri_path, type) {
  switch(type,
    "cine" = read_short_cine(mri_path),
    "lge" = read_short_lge(mri_path),
    stop("Unknown type provided.")
  )
}

read_long <- function(mri_path, type) {
  switch(type,
    "cine" = read_long_cine(mri_path),
    "lge" = read_long_lge(mri_path),
    stop("Unknown type provided.")
  )
}


read_short_cine <- function(mri_path) {
  stop(glue::glue(
    "The imported object from {mri_path} should be an 4D array (LxHxSxT)"
  ))
}

read_short_lge <- function(mri_path) {
  stop(glue::glue(
    "The imported object from {mri_path} should be an 3D array (LxHxS)"
  ))
}

read_long_cine <- function(mri_path) {
  stop(glue::glue(
    "The imported object from {mri_path} should be an 3D array (LxHxT)"
  ))
}

read_long_lge <- function(mri_path) {
  magick::image_read(mri_path)[[1]] |>
    gray3draw_to_gray2dint()

  # stop(glue::glue(
  #   "The imported object from {mri_path} should be an 2D array (LxH)"
  # ))
}


gray3draw_to_gray2dint <- function(img) {
  img[1, , , drop = TRUE] |>
    as.integer() |>
    matrix(nrow = 256, ncol = 256)
}
