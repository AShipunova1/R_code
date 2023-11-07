# quantify_compliance_start_from_mv.R

# Quantify program compliance for Gulf, dual Gulf/SA and SA permitted vessels.

# Michelle Masi
# Some caveats I have run into, in trying to quantify - are that folks may be missing 1 of 100 reports (e.g.) and that makes them non-compliant at the time you pull the compliance report data
# proportion of trip_submitted
# 2022 - 90% compliance, but what about # of reports

# 2022
# dual + GOM vs. SA
# 2023
# dual + SA
library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
library(zoo)   # Load the 'zoo' library, which deals with time series data.
library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.

project_name <- "quantify_compliance"

# Read R Code from a File
source("~/R_code_github/useful_functions_module.r")

# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions' definition.
my_paths <- set_work_dir()

# Read R Code from a File
source("~/R_code_github/quantify_compliance/quantify_compliance_functions.R")

# old
# source("~/R_code_github/quantify_compliance/get_data.R")

# get data from db ----
# Read R Code from a File
source(
  file.path(
    my_paths$git_r,
    r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"
  )
)
# got
# all_sheets_l
# vessels_22_sa
# vessels_to_remove_from_ours
# all_logbooks_db_data_2022_short_p_region

# Use the 'source' function to execute R code from a file.
source(
  file.path(
    my_paths$git_r,  # Construct the file path using elements from 'my_paths' list.
    r"(get_data\get_data_from_fhier\metric_tracking_no_srhs.R)"  # Specify the file path with the script name.
  )
)

# fhier_reports_metrics_tracking_not_srhs_ids

# Uses the file.path function to construct a file path. The components used are:
# my_paths$outputs: A variable containing a directory path.
# "quantify_compliance": A directory name to be appended to the path.
# today(): Represents a function used to include the current date, creating a date-specific path.
plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today())
# create dir if doesn't exists
create_dir_if_not(plot_file_path)

plot_colors <- list("compliant" = "skyblue1",
                    "non_compliant" = "#0570B0",
                    "nc_bucket" = "deepskyblue",
                    "non_compliant_by_month" = "blue")

title_permits <- data.frame(
  # title = c("SA Only", "GOM + Dual", "2023: SA + Dual"),
  title = c("2022: SA Only", "2022: GOM + Dual", "2023: SA + Dual"),
  year_permit = c("2022 sa_only",
                  "2022 gom_dual",
                  "2023 sa_dual"),
  second_part = c("Permitted Vessels",
                  "Permitted Vessels",
                  "Permitted Vessels")
)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  # old:
  # compl_clean_sa_vs_gom_m_int |>
  all_logbooks_db_data_2022_short_p_region |>
  dplyr::filter(
    VESSEL_OFFICIAL_NBR %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# check
# setdiff(all_logbooks_db_data_2022_short_p_region$VESSEL_OFFICIAL_NBR,
        # fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number)

# remove 2023 gom_only ----
# The function removes rows where the 'year' column is equal to "2023" and the 'permit_sa_gom' column is equal to "gom_only." 
# remove_23_gom <- function(my_df) {
#   my_df |>
#     dplyr::filter(!(year == "2023" &
#                       permit_sa_gom == "gom_only")) %>%
#     return()
# }

names(all_logbooks_db_data_2022_short_p_region) <-
  names(all_logbooks_db_data_2022_short_p_region) |>
  my_headers_case_function()

all_logbooks_db_data_2022_short_p_region <- 
  all_logbooks_db_data_2022_short_p_region |> 
  dplyr::rename(vessel_official_number = vessel_official_nbr)

# compl_clean_sa_vs_gom_m_int_filtered <-
#   # from get_data
#   remove_23_gom(compl_clean_sa_vs_gom_m_int_1)

# save vsl count for future checks ----
count_all_vessels <-
  all_logbooks_db_data_2022_short_p_region %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels
count_all_vessels[1]
# 3776
# 1876    

# count_not_gom23_vessels <-
#   compl_clean_sa_vs_gom_m_int_filtered %>%
#   select(vessel_official_number) %>%
#   unique() %>%
#   dim()
# 3887 vessels
# count_not_gom23_vessels[1]
# 3658

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  dplyr::count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number, compliant_, year_permit) %>%
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

# metrics
# vessels_compl_or_not_per_y_r_not_gom23
# 1 NO         2022 gom_dual   290
# 2 YES        2022 gom_dual  1298
# 3 NO         2022 sa_only   1263
# 4 YES        2022 sa_only   1602
# 5 NO         2023 sa_dual   1615
# 6 YES        2023 sa_dual   2111

# year ----
source(file.path(
  my_paths$git_r,
  project_name,
  "quantify_compliance_from_fhier_year.R"
))

# month ----
source(
  file.path(
    my_paths$git_r,
    project_name,
    "quantify_compliance_from_fhier_month.R"
  )
)

# vms ----
source(file.path(
  my_paths$git_r,
  project_name,
  "quantify_compliance_from_fhier_vms.R"
))


# Create a read.me file with numbers of total, active and expired ----
## by year ----
year_permit_cnts <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  sort() |>
  # repeat for each year_permit
  purrr::map_df(function(curr_year_permit) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

      # TODO: add compliant, not compliant
    out_df <- as.data.frame(c(curr_year_permit, total_vsls, active_permits, expired_permits))
    names(out_df) <- c("year_permit", "total", "active_permits", "expired_permits")

    return(out_df)
  })

# View(year_permit_cnts)

# 2 ) - not needed, gets non compliant numbers
count_year1 <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map_df(function(curr_year_permit) {
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(vsls_per_y_r) %>%
      dplyr::distinct()
    # browser()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt) |>
      dplyr::distinct()

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt) |>
      dplyr::distinct()

    out_df <-
      as.data.frame(c(
        curr_year_permit,
        total_non_compl_df,
        active_permits,
        expired_permits
      ))
    names(out_df) <-
      c("year_permit", "total", "active_permits", "expired_permits")

    return(out_df)
  })

glimpse(year_permit_cnts)
glimpse(count_year1)
all.equal(year_permit_cnts, count_year1)
# [1] "Component “total”: Mean relative difference: 0.4411713"

## 3) by month ----
# View(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
counts_by_month_read_me <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
  dplyr::group_by(year_month, year_permit, perm_exp_m) |>
  dplyr::mutate(permit_cnt_m =
           n_distinct(vessel_official_number)) |>
  dplyr::ungroup() |>
  select(year_permit, year_month, total_vsl_m, perm_exp_m, permit_cnt_m) |>
  dplyr::distinct()

# print_df_names(counts_by_month_read_me)

counts_by_month_read_me_clean <-
  counts_by_month_read_me |>
  tidyr::pivot_wider(
    id_cols = c(year_permit, year_month, total_vsl_m),
    names_from = perm_exp_m,
    values_from = permit_cnt_m,
    names_glue = "{perm_exp_m}_permits",
    values_fill = 0
  ) |>
  dplyr::arrange(year_month)

# View(counts_by_month_read_me_clean)

## by year another way ----
# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)

# 38

counts_by_year_read_me_clean <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc |>
  select(-perc_c_nc) |>
  dplyr::distinct() |>
  tidyr::pivot_wider(names_from = compl_or_not,
                     values_from = cnt_y_p_c,
                     values_fill = 0) |>
  tidyr::pivot_wider(
    # not needed, by default used all but names and values columns
    # id_cols = -c("perm_exp_y", "perm_exp_y"),
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
