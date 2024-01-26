# quantify_compliance_start.R

# need files:
# Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)
# Detail_Report_12312021_12312022__08_23_2023.csv
# FHIER_Compliance_2023__01_24_2023.csv
# Compliance_Error_Types_03_29_2023.csv
# Permits_2023-03-29_1611_active.csv

my_year <- "2023"
my_beginning <- "2023-01-01"
my_end <- "2023-12-31"

# Quantify program compliance.

# 2023
# dual + SA
library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
library(zoo)   # Load the 'zoo' library, which deals with time series data.
library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

# Read R Code from a File
source("~/R_code_github/useful_functions_module.r")

# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions' definition.
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

project_name <- current_project_basename
  "quantify_compliance"

# Read R Code from files
quantify_compliance_functions_path <- 
  file.path(current_project_dir_name,
            "quantify_compliance_functions.R")

source(quantify_compliance_functions_path)

quantify_compliance_get_data_path <- 
  file.path(current_project_dir_name,
            "get_data.R")

source(quantify_compliance_get_data_path)

source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")
# fhier_reports_metrics_tracking_not_srhs_ids

# Uses the file.path function to construct a file path. The components used are:
# my_paths$outputs: A variable containing a directory path.
# "quantify_compliance": A directory name to be appended to the path.
# today(): Represents a function used to include the current date, creating a date-specific path.
plot_file_path <-
  file.path(my_paths$outputs, project_name, today())
# create dir if doesn't exists
create_dir_if_not(plot_file_path)

plot_colors <- list("compliant" = "skyblue1",
                    "non_compliant" = "#0570B0",
                    "nc_bucket" = "deepskyblue",
                    "non_compliant_by_month" = "blue")

title_permits <- data.frame(
  # title = c("SA Only", "GOM + Dual", "2023: SA + Dual"),
  title = c("2023: SA Only", "2023: Dual only", "2023: SA + Dual"),
  year_permit = c("2022 sa_only",
                  "2022 dual_only",
                  "2023 sa_dual"),
  second_part = c("Permitted Vessels",
                  "Permitted Vessels",
                  "Permitted Vessels")
)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int_c |>
  dplyr::filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

dim(compl_clean_sa_vs_gom_m_int_1)
# [1] 146066     24

# remove 2023 gom_only ----
remove_23_gom <- function(my_df) {
  my_df |>
    dplyr::filter(!(year == "2023" & permit_sa_gom == "gom_only")) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered <-
  # from get_data
  remove_23_gom(compl_clean_sa_vs_gom_m_int_1)

# save vsl count for future checks ----
count_all_vessels <-
  n_distinct(compl_clean_sa_vs_gom_m_int_1$vessel_official_number)
# 4017 
# 3411 (2023)

count_not_gom23_vessels <-
n_distinct(compl_clean_sa_vs_gom_m_int_filtered$vessel_official_number)
# 3887 
# 2355 (2023)

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  dplyr::select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  dplyr::count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  dplyr::count(compliant_, year_permit) %>%
  dplyr::arrange(year_permit, compliant_)
# vessels
#  NO         2022 gom_dual   304
#  YES        2022 gom_dual  1482
#  NO         2022 sa_only   1289
#  YES        2022 sa_only   1617
#  NO         2023 sa_dual   1628
#  YES        2023 sa_dual   2125
# [1] "2024-01-25"
# 1 NO         2023 sa_dual  1483
# 2 YES        2023 sa_dual  2041

# metrics
# vessels_compl_or_not_per_y_r_not_gom23
# 1 NO         2022 gom_dual   290
# 2 YES        2022 gom_dual  1298
# 3 NO         2022 sa_only   1263
# 4 YES        2022 sa_only   1602
# 5 NO         2023 sa_dual   1615
# 6 YES        2023 sa_dual   2111

# ls(pattern = "metric")
# to use
# fhier_reports_metrics_tracking_not_srhs_all_cols_list

# year ----
quantify_compliance_from_fhier_year_path <- file.path(
  my_paths$git_r,
  project_name,
  "quantify_compliance_from_fhier_year.R"
)

source(quantify_compliance_from_fhier_year_path)

# year (100 % not reporting) ----
quantify_compliance_from_fhier_year_100_nc_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_year_100_nc.R")
source(quantify_compliance_from_fhier_year_100_nc_path)

# month ----
quantify_compliance_from_fhier_month_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_month.R")

source(quantify_compliance_from_fhier_month_path)

# vms ----
quantify_compliance_from_fhier_vms_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_vms.R")
source(quantify_compliance_from_fhier_vms_path)

# Create a read.me file with numbers of total, active and expired ----
## by year ----
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc defined in quantify_compliance_from_fhier_year.R
# Create a new dataset 'year_permit_cnts' by performing a series of operations.

year_permit_cnts <-
  # Extract unique 'year_permit' values from the specified column and sort them.
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  sort() |>

  # For each unique 'year_permit', apply a function using 'purrr::map_df'.
  purrr::map_df(function(curr_year_permit) {
    # Create a subset 'curr_df' of the original dataset for the current 'year_permit'.
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # Extract unique 'total_vsl_y' values for the current 'year_permit'.
    total_vsls <- unique(curr_df$total_vsl_y)

    # Extract and create a subset of data for 'active' permits.
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Extract and create a subset of data for 'expired' permits.
    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Create a data frame 'out_df' with relevant information.
    out_df <- as.data.frame(c(curr_year_permit, total_vsls, active_permits, expired_permits))
    names(out_df) <- c("year_permit", "total", "active_permits", "expired_permits")

    return(out_df)
  })

# View(year_permit_cnts)

# # 2 ) - not needed, gets non compliant numbers
# count_year1 <-
#   count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
#   unique() %>%
#   sort() %>%
#   # repeat for each year_permit
#   purrr::map_df(function(curr_year_permit) {
#     curr_df <-
#       count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
#       dplyr::filter(year_permit == curr_year_permit)
# 
#     total_non_compl_df <-
#       curr_df %>%
#       dplyr::select(vsls_per_y_r) %>%
#       dplyr::distinct()
#     # browser()
# 
#     active_permits <- curr_df %>%
#       dplyr::filter(perm_exp_y == "active") %>%
#       dplyr::select(exp_y_tot_cnt) |>
#       dplyr::distinct()
# 
#     expired_permits <- curr_df %>%
#       dplyr::filter(perm_exp_y == "expired") %>%
#       dplyr::select(exp_y_tot_cnt) |>
#       dplyr::distinct()
# 
#     out_df <-
#       as.data.frame(c(
#         curr_year_permit,
#         total_non_compl_df,
#         active_permits,
#         expired_permits
#       ))
#     names(out_df) <-
#       c("year_permit", "total", "active_permits", "expired_permits")
# 
#     return(out_df)
#   })

# glimpse(year_permit_cnts)
# glimpse(count_year1)
# all.equal(year_permit_cnts, count_year1)
# [1] "Component “total”: Mean relative difference: 0.4411713"

## 3) by month ----

# Create a new dataset 'counts_by_month_read_me' by piping the dataset
# 'compl_clean_sa_vs_gom_m_int_c_exp_diff_d' through a series of data transformation operations.

counts_by_month_read_me <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>

  # Group the data by 'year_month', 'year_permit', and 'perm_exp_m'.
  dplyr::group_by(year_month, year_permit, perm_exp_m) |>

  # Calculate the number of distinct 'vessel_official_number' within each group
  # and create a new column 'permit_cnt_m' with those counts.
  dplyr::mutate(permit_cnt_m = n_distinct(vessel_official_number)) |>

  # Remove the grouping.
  dplyr::ungroup() |>

  # Select specific columns: 'year_permit', 'year_month', 'total_vsl_m', 'perm_exp_m',
  # and the newly created 'permit_cnt_m'.
  dplyr::select(year_permit, year_month, total_vsl_m, perm_exp_m, permit_cnt_m) |>

  # Remove duplicate rows in the dataset.
  dplyr::distinct()

# print_df_names(counts_by_month_read_me)

# Create a new dataset 'counts_by_month_read_me_clean' by piping the dataset
# 'counts_by_month_read_me' through a series of data transformation operations.
counts_by_month_read_me_clean <-
  counts_by_month_read_me |>
  # Reshape the data by pivoting it wider using specific columns as identifiers.
  tidyr::pivot_wider(
    id_cols = c(year_permit, year_month, total_vsl_m),
    names_from = perm_exp_m,
    values_from = permit_cnt_m,
    names_glue = "{perm_exp_m}_permits",
    values_fill = 0
  ) |>
  # Arrange the rows in ascending order based on the 'year_month' column.
  dplyr::arrange(year_month)

# View(counts_by_month_read_me_clean)

## by year another way ----
# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)

# 38

# Create a new dataset 'counts_by_year_read_me_clean' by piping the dataset
# 'compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc'
# through a series of data transformation operations.

counts_by_year_read_me_clean <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc |>

  # Remove the 'perc_c_nc' column from the dataset.
  dplyr::select(-perc_c_nc) |>

  # Remove duplicate rows in the dataset.
  dplyr::distinct() |>

  # Reshape the data by pivoting it wider, using 'compl_or_not' for column names
  # and 'cnt_y_p_c' for values, filling missing values with 0.
  tidyr::pivot_wider(
    names_from = compl_or_not,
    values_from = cnt_y_p_c,
    values_fill = 0
  ) |>

  # Further reshape the data by pivoting it wider, using 'perm_exp_y' for column names
  # and 'cnt_y_p_e' for values, with column names formatted as '{perm_exp_y}_permits',
  # and filling missing values with 0.
  tidyr::pivot_wider(
    names_from = perm_exp_y,
    values_from = cnt_y_p_e,
    names_glue = "{perm_exp_y}_permits",
    values_fill = 0
  )

glimpse(counts_by_year_read_me_clean)

# TODO: why 2 "2023 sa_dual"?

# ==
# # make a flat file ----
# dir_to_comb <- "~/R_code_github/quantify_compliance"
# 
# files_to_combine <-
#   c(
#     "~/R_code_github/useful_functions_module.r",
#     file.path(dir_to_comb, "quantify_compliance_functions.R"),
#     file.path(dir_to_comb, "get_data.R"),
#     r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)",
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_2022.R"),
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_year.R"),
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_year_100_nc.R"),
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_month.R"),
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_line_plots.R"),
#     file.path(dir_to_comb, "quantify_compliance_from_fhier_vms.R")
#   )
# 
# # run as needed
# # make_a_flat_file(file.path(dir_to_comb, "flat_file_quantify_compliance.R"), files_to_combine)
# 
# # Add
# # from quantify_compliance start
# # to the flat file:
# library(zoo)
# library(gridExtra)
# library(cowplot)
# 
# # source("~/R_code_github/useful_functions_module.r")
# my_paths <- set_work_dir()
