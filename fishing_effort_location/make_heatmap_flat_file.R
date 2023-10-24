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

fishing_effort_location_by_permit_and_end_port_path <-
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit_and_end_port.R)"
  )

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

file.exists(fishing_effort_location_by_permit_and_end_port_path)

write_to_1_flat_file(
  flat_file_name,
  fishing_effort_location_by_permit_and_end_port_path
)

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_heatmap.R)"
  )
)

# sink()

file.exists(flat_file_name)
# T
sink.number()
# 0 - right
# delete the file
# unlink(flat_file_name)
#
