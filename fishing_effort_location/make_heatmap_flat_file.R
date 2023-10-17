# heatmap_make_flat_file.R
source("~/R_code_github/useful_functions_module.r")

dir_to_comb <- "~/R_code_github/fishing_effort_location"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")
sink(flat_file_name, append = TRUE)
