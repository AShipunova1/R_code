water_shape_prep_path <-
  file.path(my_paths$git_r,
            "get_data",
            "waters_shape_prep.R")

file.exists(water_shape_prep_path)

source(water_shape_prep_path)

# ===
tidygeocoder
if(!require(tidygeocoder)){
  install.packages("tidygeocoder")
  library(tidygeocoder)
}

# Needed for mapping
if(!require(ggmap)){
  install.packages("ggmap")
  library(ggmap)
}
