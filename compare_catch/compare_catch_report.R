# 3 sets of spp:
# 1a) SEDAR lists;
# 2b) MRIP / Recreational ACL tops;
# 3c) All FHIER spp
# For each:
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

# 1) By wave and region
write_excel_csv(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom, "w_r_gom.csv")

report_file_path <- file.path(my_paths$outputs, r"(\compare_catch\report\12_categories_tables.xlsx)")

# add to an existing excel
add_to_report_xls <- function(my_df, report_file_path, current_sheet_name) {
  write.xlsx(
    as.data.frame(my_df),
    file = report_file_path,
    sheetName = current_sheet_name,
    # rowNames = TRUE,
    colNames = TRUE,
    append = TRUE
  )
}
