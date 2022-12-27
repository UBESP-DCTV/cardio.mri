library(targets)
library(dplyr)

# tar_read(trainIdx) |> length()  # 76
# tar_read(valIdx) |> length()  # 32
# tar_read(testIdx) |> length()  # 46


lge_val <- tar_read(lge4Keras_val)
class(lge_val)
str(lge_val, 1)

matched_t <- tar_read(matchedT)
str(matched_t$clinical, 1)


nm <- names(matched_t$clinical[1])

  matched_t$clinical[[1]] |>
    select(-all_of(c("name", "follow_up_data_follow_up_aritmia"))) |>
    mutate(across(everything(), as.numeric)) |>
    array(dim = c(1L, 18L), dimnames = list(nm, rep(NULL, 18L)))


  tar_read(clinic_train) |> attr("dimnames")
  targets::tar_read_raw(glue::glue("{'clinic'}{''}Keras_{'train'}"))
