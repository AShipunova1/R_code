# to run this file ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

flat_file_name <-
  file.path(current_project_dir_path, 
            str_glue("{current_project_basename}_flat.R"))

# Delete the flat file if exists
if (file.exists(flat_file_name)) {
  unlink(flat_file_name)
}

sink(flat_file_name, append = TRUE)

# Code files content ----
# Add here what is before "useful_functions_module". Check quotation marks.

cat("\n#### add-ons 1 ---- \n\n")
cat("
# Non compliant vessels (2022) by home port
# identifying any particular areas of high non-compliance to help focus future outreach efforts.
# do this as a map
# Included overriden compliance in the total counts

# home port from the permit as an area

# source the usual setup
# get data
# remove not in metrics
# separate by permit region
# add home port

# Load the 'maps' and 'mapdata' libraries, which provide functionality for working with maps in R.
library(maps)
library(mapdata)

# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)
# Load the 'leafpop' and 'leaflet' libraries, which are used for creating interactive maps in R.
library(leafpop)
library(leaflet)

library(viridis)

# The tidygeocoder package makes getting data from geocoder services easy.
# Check if the 'tidygeocoder' package is already installed; if not, install it and load the library.
if (!require(tidygeocoder)) {
  install.packages('tidygeocoder')  # Install 'tidygeocoder' package if not already installed.
  library(tidygeocoder)  # Load the 'tidygeocoder' library after installation.
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE
",
sep = "\n")

write_to_1_flat_file(flat_file_name,
                     "~/R_code_github/useful_functions_module.r")

cat("\n#### add-ons 2 ---- \n\n")
cat("
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)
",
sep = "\n")

# prepare data ----
get_data_from_fhier_dir <- "get_data/get_data_from_fhier"
fhier_files_to_combine_pathes <-
  c(
    file.path(my_paths$git_r,
                get_data_from_fhier_dir,
                 "get_srhs_vessels.R"),
    file.path(my_paths$git_r,
                get_data_from_fhier_dir,
                 "get_metrics_tracking.R"),
    file.path(my_paths$git_r,
                get_data_from_fhier_dir,
                 "metric_tracking_no_srhs.R")
  )

map(fhier_files_to_combine_pathes,
    \(one_path) {
      write_to_1_flat_file(flat_file_name, one_path)
    })

db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")

write_to_1_flat_file(flat_file_name, db_data_path)

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")

write_to_1_flat_file(flat_file_name, misc_info_path)

# fix_ports_file_path <-
#   file.path(current_project_dir_path,
#             "non_compliant_areas_fix_lat_lon.R")
# 
# write_to_1_flat_file(flat_file_name, fix_ports_file_path)

get_data_file_path <-
  file.path(current_project_dir_path,
            "non_compliant_areas_get_data.R")

write_to_1_flat_file(flat_file_name, get_data_file_path)

# Main ----
main_file_path <-
  file.path(current_project_dir_path,
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

