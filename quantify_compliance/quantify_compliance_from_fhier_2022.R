# Quantify program compliance for Gulf and dual Gulf/SA permitted vessels.

# Michelle Masi
# Some caveats I have run into, in trying to quantify - are that folks may be missing 1 of 100 reports (e.g.) and that makes them non-compliant at the time you pull the compliance report data
# proportion of trip_submitted
# 2022 - 90% compliance, but what about # of reports

# 2022
# dual + GOM vs. SA
# 2023
# dual + SA
library(grid)
library(zoo)
library(gridExtra)
library(cowplot)

project_name <- "quantify_compliance"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today())
# create dir if doesn't exists
create_dir_if_not(plot_file_path)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int |>
  filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# remove 2023 gom_only ----
remove_23_gom <- function(my_df) {
  my_df |>
    filter(!(year == "2023" & permit_sa_gom == "gom_only")) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered <-
  # from get_data
  remove_23_gom(compl_clean_sa_vs_gom_m_int_1)

# save vsl count for future checks ----
count_all_vessels <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels
count_all_vessels[1]
# 3776

count_not_gom23_vessels <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels
count_not_gom23_vessels[1]
# 3658

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  count(compliant_, year_permit) %>%
  arrange(year_permit, compliant_)
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
      distinct()
    # browser()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt) |>
      distinct()

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt) |>
      distinct()

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
  group_by(year_month, year_permit, perm_exp_m) |>
  mutate(permit_cnt_m =
           n_distinct(vessel_official_number)) |>
  ungroup() |>
  select(year_permit, year_month, total_vsl_m, perm_exp_m, permit_cnt_m) |>
  distinct()

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
  arrange(year_month)

# View(counts_by_month_read_me_clean)

## by year another way ----
# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)

# 38

counts_by_year_read_me_clean <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc |>
  select(-perc_c_nc) |>
  distinct() |>
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
# make a flat file ----
dir_to_comb <- "~/R_code_github/quantify_compliance"

files_to_combine <-
  c(
    "~/R_code_github/useful_functions_module.r",
    file.path(dir_to_comb, "quantify_compliance_functions.R"),
    file.path(dir_to_comb, "get_data.R"),
    r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)",
    file.path(dir_to_comb, "quantify_compliance_from_fhier_2022.R")
  )

# run as needed
# make_a_flat_file(file.path(dir_to_comb, "flat_file_quantify_compliance.R"), files_to_combine)

# Add
# from quantify_compliance start
# to the flat file:
library(zoo)
library(gridExtra)
library(cowplot)

# source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
