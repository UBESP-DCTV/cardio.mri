## Use this script to run exploratory code maybe before to put it into
## the pipeline


# setup -----------------------------------------------------------

library(targets)
library(here)

# load all your custom functions
list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()


# Code here below -------------------------------------------------
# use `tar_read(target_name)` to load a target anywhere (note that
# `target_name` is NOT quoted!)



library(reticulate)
use_condaenv("tf")

library(tensorflow)
tf_config()
tf_gpu_configured()


library(keras)

input_l2c_cine <- layer_input(shape = c(256, 256, 1, 35), dtype = "int32")
input_l3c_cine <- layer_input(shape = c(256, 256, 1, 35), dtype = "int32")
input_l4c_cine <- layer_input(shape = c(256, 256, 1, 35), dtype = "int32")

input_l2c_lge <- layer_input(shape = c(256, 256), dtype = "int32")
input_l3c_lge <- layer_input(shape = c(256, 256), dtype = "int32")
input_l4c_lge <- layer_input(shape = c(256, 256), dtype = "int32")

input_c_cine <- layer_input(shape = c(256, 256, 25, 35), dtype = "int32")
input_c_lge <- layer_input(shape = c(256, 256, 25), dtype = "int32")
