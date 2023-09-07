# quantify_compliance_make_flat_file.R
source("~/R_code_github/useful_functions_module.r")

dir_to_comb <- "~/R_code_github/quantify_compliance"

flat_file_name <-
  file.path(dir_to_comb, "flat_file_quantify_compliance.R")
sink(flat_file_name, append = TRUE)
    
cat("\n\n#### add-ons 1 ---- \n\n")
cat("library(grid)",  sep = "\n")
cat('library(zoo)', sep = "\n")
cat('library(gridExtra)', sep = "\n")
cat('library(cowplot)', sep = "\n")

cat('project_name <- "quantify_compliance"', sep = "\n")

current_file_name = "~/R_code_github/useful_functions_module.r"
write_to_1_flat_file(flat_file_name, current_file_name)

cat("\n\n#### add-ons 2 ---- \n\n")

cat('my_paths <- set_work_dir()', sep = "\n")

current_file_name <- "~/R_code_github/quantify_compliance/quantify_compliance_functions.R"
write_to_1_flat_file(flat_file_name, current_file_name)

current_file_name <- "~/R_code_github/quantify_compliance/get_data.R"
write_to_1_flat_file(flat_file_name, current_file_name)

current_file_name <- r"(~\R_code_github\get_data_from_fhier\metric_tracking_no_srhs.R)"
write_to_1_flat_file(flat_file_name, current_file_name)

cat('plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today()))', sep = "\n")
cat('create_dir_if_not(plot_file_path))', sep = "\n")

cat('
# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int |>
  filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )', sep = "\n")

cat('
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
', sep = "\n")

# year ----
current_file_name = file.path(
  dir_to_comb,
  "quantify_compliance_from_fhier_year.R"
)
write_to_1_flat_file(flat_file_name, current_file_name)

# month ----
current_file_name = file.path(
  dir_to_comb,
  "quantify_compliance_from_fhier_month.R"
)
write_to_1_flat_file(flat_file_name, current_file_name)

# line_plots ----
current_file_name = file.path(dir_to_comb, "quantify_compliance_from_fhier_line_plots.R")
write_to_1_flat_file(flat_file_name, current_file_name)

# vms ----
current_file_name = file.path(
  dir_to_comb,
  "quantify_compliance_from_fhier_vms.R"
)
write_to_1_flat_file(flat_file_name, current_file_name)

# close the file
sink()
