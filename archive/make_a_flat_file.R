# to run this file ----
# install.packages("~/R_code_github/auxfunctions_1.0.tar.gz",
#                  repos = NULL,
#                  type = "source")

library(auxfunctions)

# source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

flat_file_name <-
  file.path(current_project_dir_name,
            str_glue("{current_project_basename}_flat.R"))

if (file.exists(flat_file_name)) {
  unlink(flat_file_name)
}

sink(flat_file_name, append = TRUE)

# Code files content ----
# Add here what is before "useful_functions_module" in the main file. Check quotation marks.

cat("\n#### add-ons 1 ---- \n\n")
cat("
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
",
sep = "\n")

write_to_1_flat_file(flat_file_name,
                     "~/R_code_github/useful_functions_module.r")

cat("\n#### add-ons 2 ---- \n\n")
cat("
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)
",
sep = "\n")

## additional data ----
# for qmd use #' {{< include .qmd >}} instead of source()

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
write_to_1_flat_file(flat_file_name, misc_info_path)

#' {{< include misc_info.qmd >}}
#'

get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

get_metrics_tracking_path <-
  file.path(my_paths$git_r,
                 get_data_from_fhier_dir,
                 "get_metrics_tracking.R")
write_to_1_flat_file(flat_file_name, get_metrics_tracking_path)

get_db_data_path <-
  file.path(my_paths$git_r, r"(get_data\get_db_data\get_db_data.R)")
write_to_1_flat_file(flat_file_name, get_db_data_path)

boats_number_get_data_info_path <-
  file.path(current_project_dir_name,
            r"(boats_number_get_data.R)")

write_to_1_flat_file(flat_file_name, boats_number_get_data_info_path)

## Get shape files ----
waters_shape_prep_path <-
  file.path(my_paths$git_r,
            r"(get_data\waters_shape_prep.R)")

# file.exists(waters_shape_prep_path)

write_to_1_flat_file(flat_file_name, waters_shape_prep_path)

# Main ----
main_file_path <-
  file.path(current_project_dir_name,
            str_glue("{current_project_basename}.R"))

write_to_1_flat_file(flat_file_name, main_file_path)

# comment out "source" ----
flat_file_r_text <-
  readLines(flat_file_name)
head(flat_file_r_text)

comment_out_sources <-
  function(flat_file_r_text) {
    flat_file_r_text <-
      gsub("source\\(", "# source(", flat_file_r_text)
    return(flat_file_r_text)
  }

flat_file_r_text <- comment_out_sources(flat_file_r_text)

## Write back ----
sink(flat_file_name, append = FALSE)

cat(flat_file_r_text, sep = "\n")

# Restore the default output behavior.
sink()

