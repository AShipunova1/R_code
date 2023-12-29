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
library(gridExtra)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

## additional data ----
# for qmd use #' {{< include .qmd >}} instead of source()

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
source(misc_info_path)

#' {{< include misc_info.qmd >}}
#'

boats_number_get_data_info_path <-
  file.path(current_project_dir_name,
            r"(boats_number_get_data.R)")

source(boats_number_get_data_info_path)

## Get shape files ----
waters_shape_prep_path <-
  file.path(my_paths$git_r,
            r"(get_data\waters_shape_prep.R)")

# file.exists(waters_shape_prep_path)

source(waters_shape_prep_path)
