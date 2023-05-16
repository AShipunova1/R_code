library(xlsx)
# 3 sets of spp:
# 1a) SEDAR lists;
# 2b) MRIP / Recreational ACL tops;
# 3c) All FHIER spp
# For each:
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

# write_excel_csv(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom, "w_r_gom.csv")

# add to an existing excel
add_to_report_xls <- function(my_df, current_sheet_name) {
  report_file_path <-
    file.path(my_paths$outputs,
              r"(\compare_catch\report\12_categories_tables.xlsx)")
  
  xlsx::write.xlsx(
    as.data.frame(my_df),
    file = report_file_path,
    sheetName = current_sheet_name,
    # row.names = TRUE,
    col.names = TRUE,
    append = TRUE
  )
}

# 1) By wave and region 1a) SEDAR ----
## keep only entries for spp. in the top ten list, separately for each region ----

# gom ----
add_to_report_xls(fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar, "1.1a wave and region SEDAR GOM")

# sa ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar %>%
  add_to_report_xls("1.1a wave and region SEDAR SA")

