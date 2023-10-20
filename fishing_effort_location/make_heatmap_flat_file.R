# heatmap_make_flat_file.R
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/fishing_effort_location"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")


write_to_1_flat_file(flat_file_name,
                     "~/R_code_github/useful_functions_module.r")

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_heatmap.R)"
  )
)

sink()

