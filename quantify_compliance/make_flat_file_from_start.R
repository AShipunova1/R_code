# quantify_compliance_make_flat_file.R from quantify_compliance_start.R

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/quantify_compliance"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_quantify_compliance.R")
sink(flat_file_name, append = TRUE)

cat("\n\n#### add-ons 1 ---- \n\n")
cat("
library(grid)  # Load the 'grid' library, which provides low-level graphics functions.
library(zoo)   # Load the 'zoo' library, which deals with time series data.
library(gridExtra)  # Load the 'gridExtra' library for arranging and combining grid graphics.
library(cowplot)  # Load the 'cowplot' library for creating publication-ready plots with ggplot2.
",  sep = "\n")

project_name <- "quantify_compliance"
# cat('
# project_name <- "quantify_compliance"
# ',  sep = "\n")

# Read R Code from a File
current_file_name = "~/R_code_github/useful_functions_module.r"
write_to_1_flat_file(flat_file_name, current_file_name)

cat("\n\n#### add-ons 2 ---- \n\n")
cat('
# Use a function defined in "useful_functions_module.r". Use F2 to see a custom functions definition.
my_paths <- set_work_dir()
',  sep = "\n")

# Read R Code from files
current_file_name = "~/R_code_github/quantify_compliance/quantify_compliance_functions.R"
write_to_1_flat_file(flat_file_name, current_file_name)

current_file_name = "~/R_code_github/quantify_compliance/get_data.R"
write_to_1_flat_file(flat_file_name, current_file_name)

get_data_from_fhier_dir <- "get_data/get_data_from_fhier"

current_file_name <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_metrics_tracking.R")
write_to_1_flat_file(flat_file_name, current_file_name)

current_file_name <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "get_srhs_vessels.R")
write_to_1_flat_file(flat_file_name, current_file_name)

current_file_name <-
  file.path(my_paths$git_r,
            get_data_from_fhier_dir,
            "metric_tracking_no_srhs.R")
write_to_1_flat_file(flat_file_name, current_file_name)


cat("\n\n#### add-ons 3 ---- \n\n")
cat('
# Uses the file.path function to construct a file path. The components used are:
# my_paths$outputs: A variable containing a directory path.
# "quantify_compliance": A directory name to be appended to the path.
# today(): Represents a function used to include the current date, creating a date-specific path.
plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today())
# create dir if doesn_t exists
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
  compl_clean_sa_vs_gom_m_int_c |>
  dplyr::filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

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
  compl_clean_sa_vs_gom_m_int_1 %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels
count_all_vessels[1]
# 3776

count_not_gom23_vessels <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels
count_not_gom23_vessels[1]
# 3658

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

# metrics
# vessels_compl_or_not_per_y_r_not_gom23
# 1 NO         2022 gom_dual   290
# 2 YES        2022 gom_dual  1298
# 3 NO         2022 sa_only   1263
# 4 YES        2022 sa_only   1602
# 5 NO         2023 sa_dual   1615
# 6 YES        2023 sa_dual   2111
',  sep = "\n")

# year ----
quantify_compliance_from_fhier_year_path <- file.path(
  my_paths$git_r,
  project_name,
  "quantify_compliance_from_fhier_year.R"
)
current_file_name = quantify_compliance_from_fhier_year_path
write_to_1_flat_file(flat_file_name, current_file_name)

# year (100 % not reporting) ----
quantify_compliance_from_fhier_year_100_nc_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_year_100_nc.R")
current_file_name = quantify_compliance_from_fhier_year_100_nc_path
write_to_1_flat_file(flat_file_name, current_file_name)

# month ----
quantify_compliance_from_fhier_month_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_month.R")

current_file_name = quantify_compliance_from_fhier_month_path
write_to_1_flat_file(flat_file_name, current_file_name)

# vms ----
quantify_compliance_from_fhier_vms_path <-
  file.path(my_paths$git_r,
            project_name,
            "quantify_compliance_from_fhier_vms.R")
current_file_name = quantify_compliance_from_fhier_vms_path
write_to_1_flat_file(flat_file_name, current_file_name)

cat("\n\n#### add-ons 3 ---- \n\n")
cat('
# Create a read.me file with numbers of total, active and expired ----
## by year ----
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc defined in quantify_compliance_from_fhier_year.R
# Create a new dataset "year_permit_cnts" by performing a series of operations.

year_permit_cnts <-
  # Extract unique "year_permit" values from the specified column and sort them.
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  sort() |>

  # For each unique "year_permit", apply a function using "purrr::map_df".
  purrr::map_df(function(curr_year_permit) {
    # Create a subset "curr_df" of the original dataset for the current "year_permit".
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # Extract unique "total_vsl_y" values for the current "year_permit".
    total_vsls <- unique(curr_df$total_vsl_y)

    # Extract and create a subset of data for "active" permits.
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Extract and create a subset of data for "expired" permits.
    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # Create a data frame "out_df" with relevant information.
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
# View(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
counts_by_month_read_me <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
  dplyr::group_by(year_month, year_permit, perm_exp_m) |>
  dplyr::mutate(permit_cnt_m =
           n_distinct(vessel_official_number)) |>
  dplyr::ungroup() |>
  dplyr::select(year_permit, year_month, total_vsl_m, perm_exp_m, permit_cnt_m) |>
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
  dplyr::select(-perc_c_nc) |>
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
',  sep = "\n")

sink()

# Remove all "source" lines from the flat file when done. ----
# source( to #source(
# Read the content of a flat file specified by 'flat_file_name' and store it in 'flat_file_content'
flat_file_content <- readr::read_lines(flat_file_name)

# Create a new variable 'flat_file_content_1' by replacing instances of "source(" with "# source(" in 'flat_file_content'
flat_file_content_1 <- 
  gsub("source\\(", "# source(", flat_file_content)

# Write the contents of 'flat_file_content_1' back to the original flat file specified by 'flat_file_name'
cat(flat_file_content_1, file = flat_file_name, sep = "\n")

file.exists(flat_file_name)
# T
sink.number()
# 0 - right
# delete the file
# unlink(flat_file_name)
#

