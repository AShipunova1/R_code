source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

flat_file_name <-
  file.path(current_project_dir_name, "boats_number_flat.R")
sink(flat_file_name, append = TRUE)

cat("\n\n#### add-ons 1 ---- \n\n")

cat("
# setup current project ----
library(zoo)
# Determine the path of the executing script
library(this.path)

# Prints an R object in markdown, needed to print pretty table from list of dfs.
library(pander)

# maps:
library(mapview)
library(sf)
library(ggmap) ## extends 'ggplot2' for creating maps and working with spatial data.
library(viridis)
library(grid)
library(gridExtra)",
sep = "\n")

current_file_name = "~/R_code_github/useful_functions_module.r"
write_to_1_flat_file(flat_file_name, current_file_name)

cat("\n\n#### add-ons 2 ---- \n\n")
cat("
# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)
",  sep = "\n")


misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
write_to_1_flat_file(flat_file_name, misc_info_path)

boats_number_get_data_info_path <-
  file.path(current_project_dir_name,
            r"(boats_number_get_data.R)")

write_to_1_flat_file(flat_file_name, boats_number_get_data_info_path)

waters_shape_prep_path <-
  file.path(my_paths$git_r,
            r"(get_data\waters_shape_prep.R)")

# file.exists(waters_shape_prep_path)

write_to_1_flat_file(flat_file_name, waters_shape_prep_path)

# Main ----
boats_number_path <-
  file.path(current_project_dir_name,
            r"(boats_number.R)")


write_to_1_flat_file(flat_file_name, boats_number_path)


