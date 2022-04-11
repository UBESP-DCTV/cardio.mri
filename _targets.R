library(targets)
library(tarchetypes)

list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Set target-specific options such as packages.
tar_option_set(error = "continue")

# End this file with a list of target objects.
list(

  # Import your file from custo (shared) location, and preprocess them
  tar_target(inputRawDataPath, get_input_data_path(), format = "file"),

  # compile yor report
  tar_render(report, here::here("reports/report.Rmd")),


  # Decide what to share with other, and do it in a standard RDS format
  tar_target(
    objectToShare,
    list()
  )
  # tar_target(
  #   shareOutput,
  #   share_objects(objectToShare),
  #   format = "file",
  #   pattern = map(objectToShare)
  # )
)
