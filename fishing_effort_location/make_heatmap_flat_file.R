# heatmap_make_flat_file.R
# Remove all "source" lines from the flat file when done.


source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/fishing_effort_location"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")

write_to_1_flat_file(flat_file_name,
                     "~/R_code_github/useful_functions_module.r")

cat(flat_file_name,
    'my_paths <- set_work_dir()',
    sep = "\n")

# data are from "by_permit"

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\fishing_effort_location_by_permit.R)"
  )
)

cat(
  flat_file_name,
  "library(ggplot2) # a visualization package",
  "library(ggmap) # extends 'ggplot2' for creating maps and working with spatial data.",
  sep = "\n"
)

write_to_1_flat_file(
  flat_file_name,
  file.path(
    my_paths$git_r,
    r"(fishing_effort_location\prepare_gom_heatmap_func.R)"
  )
)

# Build the path to the R script 'vessel_permit_corrected_list.R' by
# combining the base path 'my_paths$git_r' and the script name.
script_path <-
  file.path(my_paths$git_r,
            "vessel_permit_list/vessel_permit_corrected_list.R")

# Source (run) the R script using the constructed script path.
# source(script_path)
# Remove "source(script_path)" from the flat file

write_to_1_flat_file(
  flat_file_name,
  script_path
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
unlink(flat_file_name)
#
