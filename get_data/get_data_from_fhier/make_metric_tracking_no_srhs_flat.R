# files to have:
# from "Logbook Processing (Do this before all Logbook Analyses)" google drive
# https://drive.google.com/drive/folders/18ociLUchXpLxrhb3-gJRuIV_0PaQGUFy?usp=sharing
# 1) "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv"
# 2) "2023SRHSvessels.csv"

library(tidyverse)
library(readxl)
# help functions in no srhs ----
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories for metricks project ----

get_current_file_directory <-
  function() {
    dirname(rstudioapi::getSourceEditorContext()$path)
  }

# change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()

  # for others
  add_dir <- ""
  # for Anna's computer
  if (get_username() == "anna.shipunova") {
    add_dir <- "R_files_local/test_dir"
  }

  # add an empty or Anna's folder in front
  main_r_dir <- file.path(add_dir, "SEFHIER/R code")

  in_dir <- "Inputs"
  # file.path instead of paste, because it provides correct concatenation, "\" or "/" etc.
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "Outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # git_r_dir <- "R_code_github"
  # full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir) #,
                   #"git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

set_work_dir_local <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"

  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  git_r_dir <- "R_code_github"
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

if (get_username() == "anna.shipunova") {
  set_work_dir <- set_work_dir_local
}

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

fix_names <- function(x) {
  x %>%
    # remove dots
    stringr::str_replace_all("\\.", "") %>%
    # all not letters and numbers to underscores
    stringr::str_replace_all("[^A-z0-9]", "_") %>%
    # letters only in the beginning
    stringr::str_replace_all("^(_*)(.+)", "\\2\\1") %>%
    # tolower
    my_headers_case_function()
}

my_paths <- set_work_dir()

#### Current file: ~/R_code_github/get_data_from_fhier/get_srhs_vessels.R ----

# get SRHS vessels to exclude ----

srhs_vessels_2023_path <-
  file.path(my_paths$inputs,
  r"(from_Fhier\2023SRHSvessels.csv)")

if (!file.exists(srhs_vessels_2023_path)) {
  srhs_vessels_2023 <-
    file.path(my_paths$inputs,
              file_name)
}

srhs_vessels_2023_info <-
  read_excel(
  srhs_vessels_2023,
  # add the sheet name if needed and uncomment the next line
  # sheet = sheet_n,
  # use my fix_names function for col names
  .name_repair = fix_names,
  # if omitted, the algorithm uses only the few first lines and sometimes guesses it wrong
  guess_max = 21474836,
  # read all columns as text
  col_types = "text"
)


#### Current file: ~/R_code_github/get_data_from_fhier/get_metrics_tracking.R ----

## fhier_reports_metrics_tracking ----

# Download from FHIER / Reports / Metrics Tracking
# Put dates in, e.g. 01/01/2022 - 12/31/2022
# Click "search"
# Under "Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)	" section below click "Actions", then "Download"

fhier_reports_metrics_tracking_file_names <-
  c("Detail_Report_12312021_12312022__08_23_2023.csv",
    "Detail_Report_12312022_12312023__08_23_2023.csv")

common_dir <-
  file.path(my_paths$inputs,
  r"(from_Fhier\Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source))")

# save all file names to a list
fhier_reports_metrics_tracking_file_path <-
  purrr::map(fhier_reports_metrics_tracking_file_names,
      ~ file.path(common_dir,
                  .x))

# test
purrr::map(fhier_reports_metrics_tracking_file_path,
    file.exists)
# T

# read each csv in a list of dfs
fhier_reports_metrics_tracking_list <-
  purrr::map(
    fhier_reports_metrics_tracking_file_path,
    ~ readr::read_csv(
      .x,
      # read as character
      col_types = cols(.default = 'c'),
      name_repair = fix_names
    )
  )

# check how many in diff years ----
dplyr::setdiff(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# [1] 669

dplyr::setdiff(fhier_reports_metrics_tracking_list[[2]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[1]]$vessel_official_number) |>
  length()
# [1] 493

# in both years
dplyr::intersect(fhier_reports_metrics_tracking_list[[1]]$vessel_official_number,
         fhier_reports_metrics_tracking_list[[2]]$vessel_official_number) |>
  length()
# 2965

#### Current file: ~/R_code_github/get_data_from_fhier/metric_tracking_no_srhs.R ----

## exclude srhs vessels from metric traking ----
fhier_reports_metrics_tracking_not_srhs_ids <-
  # create one df
  purrr::map_df(
    fhier_reports_metrics_tracking_list,
    # for each df
    ~ .x |>
      # exclude SRHS vessels
      dplyr::filter(!vessel_official_number %in% srhs_vessels_2023_info$uscg__)
  ) |>
  # keep only the vessel_official_numbers, remove all other columns
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

# the same, but result kept in a list
fhier_reports_metrics_tracking_not_srhs_ids_list <-
  purrr::map(
    fhier_reports_metrics_tracking_list,
    ~ .x |>
      dplyr::filter(!vessel_official_number %in% srhs_vessels_2023_info$uscg__) |>
      dplyr::select(vessel_official_number) |>
      dplyr::distinct()
  )

# check results ----
dim(fhier_reports_metrics_tracking_not_srhs_ids)
# [1] 2981    1

purrr::map(fhier_reports_metrics_tracking_list, dim)
# [[1]]
# [1] 3634   13
#
# [[2]]
# [1] 3460   13

purrr::map(fhier_reports_metrics_tracking_not_srhs_ids_list, dim)
# [[1]]
# [1] 3571    1
#
# [[2]]
# [1] 3399    1
