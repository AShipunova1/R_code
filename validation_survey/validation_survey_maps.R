# setup maps ----
needed_packages <- c("tidygeocoder", "ggmap")

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

lapply(needed_packages, library, character.only = TRUE)

devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

water_shape_prep_path <-
  file.path(my_paths$git_r,
            "get_data",
            "waters_shape_prep.R")

file.exists(water_shape_prep_path)

source(water_shape_prep_path)

# get city coords ----

