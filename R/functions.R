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

  res <- read_fun(mri_path, type, mri_info)
  attr(res, "mri_info") <- mri_info
  invisible(res)
}

read_short <- function(mri_path, type, mri_info) {
  switch(type,
    "cine" = read_short_cine(mri_path, mri_info),
    "lge" = read_short_lge(mri_path),
    stop("Unknown type provided.")
  )
}

read_long <- function(mri_path, type, mri_info = NULL) {
  switch(type,
    "cine" = read_long_cine(mri_path),
    "lge" = read_long_lge(mri_path),
    stop("Unknown type provided.")
  )
}


read_short_cine <- function(mri_path, mri_info) {
  t_max <- mri_info[["t_max"]]
  s_max <- mri_info[["s_max"]]

  magick::image_read_video(mri_path, fps = NULL) |>
    video_to_4dint(t_max, s_max)
}

read_short_lge <- function(mri_path) {
  magick::image_read_video(mri_path, fps = NULL) |>
    video_to_3dint()
}

read_long_cine <- function(mri_path) {
  magick::image_read_video(mri_path, fps = NULL) |>
    video_to_3dint()
}

read_long_lge <- function(mri_path) {
  magick::image_read(mri_path)[[1]] |>
    gray3draw_to_gray2dint()
}

video_to_4dint <- function(avi, t_max, s_max) {
  ui_warn("viene importato sempre un ultimo frame in più rispetto TxS!")
  ui_todo("togliere l'ultimo frame prima dell'importazione")
  stop("to be implemented")
  full_seq <- seq_along(avi) |>
    purrr::map(~ avi[[.x]] |> gray3draw_to_gray2dint()) |>
    abind::abind(along = 3)
  # d_seq <- dim(full_seq)
  # stopifnot((t_max + s_max) != d_seq[[3]])
  # dim(full_seq) <- c(d_seq[[1]], d_seq[[2]], t_max, s_max)
  full_seq
}

video_to_3dint <- function(avi) {
  seq_along(avi) |>
    purrr::map(~ avi[[.x]] |> gray3draw_to_gray2dint()) |>
    abind::abind(along = 3)
}

gray3draw_to_gray2dint <- function(img) {
  dim_img <- dim(img)
  res <- img[1, , , drop = TRUE] |>
    as.integer() |>
    matrix(nrow = dim_img[[2]], ncol = dim_img[[3]])

  dimnames(res) <- list(R = NULL, C = NULL)
  res
}
