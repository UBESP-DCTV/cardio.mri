get_info_from_filename <- function(filename) {


  component <- filename |>
    stringr::str_remove("\\.(avi|png)$") |>
    stringr::str_replace_all(" ", "_") |>
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

  component[["ch"]] <- get_ch(readr::parse_number(component[["ch"]]))
  if (component[["ch"]] != 1L) {
    component[["s_tot"]] <- 1L
  }

  component
}




get_ch <- function(x) {
  as.character(x) |>
    stringr::str_sub(start = 1L, end = 1L) |>
    as.integer()
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
    usethis::ui_stop(
      "Unknown channel provided: {usethis::ui_value(channel)}.
      File name is: {usethis::ui_value(basename(mri_path))}."
    )
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
  s_tot <- mri_info[["s_tot"]]

  res <- magick::image_read_video(mri_path, fps = NULL) |>
    video_to_4dint(t_max, s_tot, mri_path)
  dimnames(res) <- list(
    time = NULL, row = NULL, col = NULL, slice = NULL
    )
  res
}

read_short_lge <- function(mri_path) {
  res <- magick::image_read_video(mri_path, fps = NULL) |>
    video_to_3dint(lge = TRUE)
  dimnames(res) <- list(row = NULL, col = NULL, slice = NULL)
  res
}

read_long_cine <- function(mri_path) {
  res <- magick::image_read_video(mri_path, fps = NULL) |>
    video_to_3dint(lge = FALSE)
  dimnames(res) <- list(time = NULL, row = NULL, col = NULL)
  res
}

read_long_lge <- function(mri_path) {
  magick::image_read(mri_path)[[1]] |>
    gray3draw_to_gray2dint()
}

video_to_4dint <- function(avi, t_max, s_tot, path = NULL) {
  n_frames <- length(avi) - 1L
  starts <- which(seq_len(n_frames) %% t_max == 1)
  if (length(starts) != s_tot) usethis::ui_stop("
    Desequencing error for file {path}.
    Possible issues in t_max/s_tot.
    I.e. t_max * s_tot is different from the number of frames
         in the video.
  ")

  res <- starts |>
    purrr::map(~ {
      avi[.x + seq_len(t_max) - 1L] |>
        video_to_3dint(lge = FALSE)
    }) |>
    abind::abind(along = 4)

  d_seq <- dim(res)
  stopifnot((t_max + s_tot) == sum(d_seq[c(1, 4)]))
  dim(res) <- c(t_max, d_seq[[2]], d_seq[[3]], s_tot)
  dimnames(res) <- list(
    time = NULL, row = NULL, col = NULL, slice = NULL
  )
  res
}

video_to_3dint <- function(avi, lge = TRUE) {
  res <- seq_along(avi) |>
    purrr::map(~ avi[[.x]] |> gray3draw_to_gray2dint()) |>
    abind::abind(along = 0.5 + 2.5 * lge)
  if (lge) {
    dimnames(res) <- list(row = NULL, col = NULL, slice = NULL)
  } else {
    dimnames(res) <- list(time = NULL, row = NULL, col = NULL)
  }
  res
}

gray3draw_to_gray2dint <- function(img) {
  dim_img <- dim(img)
  res <- img[1, , , drop = TRUE] |>
    as.integer() |>
    matrix(nrow = dim_img[[2]], ncol = dim_img[[3]])
  dimnames(res) <- list(row = NULL, col = NULL)
  res
}
