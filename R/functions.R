get_info_from_filename <- function(filename) {


  component <- filename |>
    stringr::str_remove("\\.(avi|png)$") |>
    stringr::str_split("-")

  if (!length(component) %in% c(3L, 5L)) {
    usethis::ui_stop(
      "Wrong number of components in {usethis::ui_value(filename)}"
    )
  }

  if (length(component) == 5) {
    names(component) <- c("name", "ch", "type", "t_max", "s_tot")
    component[["t_max"]] <- readr::parse_number(component[["t_max"]])
    component[["s_tot"]] <- readr::parse_number(component[["s_tot"]])
  } else  {
    names(component) <- c("name", "channel", "type")
  }

  if (component[["type"]] == "lge") {
    component[["t_max"]] <- 1L
  }

  component[["ch"]] <- readr::parse_number(component[["ch"]])
  if (component[["ch"]] != 1L) {
    component[["s_tot"]] <- 1L
  }

  list(
    name = component[[1L]],
    channel = channel,
    type = type,
    t_max = component[[4L]],
    s_tot = component[[5L]]
  )
}

read_mri <- function(mri_path) {

  mri_filename <- basename(mri_path)
  mri_info <- get_info_from_filename(mri_filename)

  channel <- mri_info[["channel"]]
  type <- mri_info[["type"]]

  if (length(type) * length(channel) == 0) {
    stop("type or channel are empty")
  }

  read_fun <- dplyr::case_when(
    channel == 1 ~ read_long(mri_path, type),
    channel %in% 2:3 ~ read_short(mri_path, type),
    TRUE ~ stop("Unknown channel provided.")
  )

  res <- read_fun(mri_path)
  attr(res, "mri_info") <- mri_info
  invisible(res)
}

read_long <- function(mri_path, type) {
  dplyr::case_when(
    type == "cine" ~ read_long_cine(mri_path),
    type == "lge" ~ read_long_lge(mri_path),
    TRUE ~ stop("Unknown type provided.")
  )
}

read_short <- function(mri_path, type) {
  dplyr::case_when(
    type == "cine" ~ read_short_cine(mri_path),
    type == "lge" ~ read_short_lge(mri_path),
    TRUE ~ stop("Unknown type provided.")
  )
}

read_long_cine <- function(mri_path) {
  mri_path
}

read_long_lge <- function(mri_path) {
  mri_path
}

read_short_cine <- function(mri_path) {
  mri_path
}

read_short_lge <- function(mri_path) {
  mri_path
}
