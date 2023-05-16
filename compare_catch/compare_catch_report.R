library(xlsx)
# 3 sets of spp:
# 1a) SEDAR lists;
# 2b) MRIP / Recreational ACL tops;
# 3c) All FHIER spp
# For each:
# 1) By wave region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

# write_excel_csv(fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom, "w_r_gom.csv")

# add to an existing excel
add_to_report_xls <- function(my_df, current_sheet_name, report_file_path = NULL) {
  # default path
  if (is.null(report_file_path)) {
    report_file_path <-
      file.path(my_paths$outputs,
                r"(\compare_catch\report\12_categories_tables_reg.xlsx)")
  }
  
  xlsx::write.xlsx(
    as.data.frame(my_df),
    file = report_file_path,
    sheetName = current_sheet_name,
    row.names = FALSE,
    col.names = TRUE,
    append = TRUE
  )
}

# all ----
fhier_acl_catch_by_species_state_region_waves %>%
  add_to_report_xls("counts by species_state_region_waves")

# 1) By wave region 1a) SEDAR ----
## keep only entries for spp. in the top ten list, separately for each region ----

## GOM ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar
  add_to_report_xls("1.1a wave region SEDAR GOM")

## SA ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot_sa_sedar %>%
  add_to_report_xls("1.1a wave region SEDAR SA")

# 1) By wave region 2b) Recreational ACL tops; ----
## GOM ----
gom_acl_top_to_plot %>%
  add_to_report_xls("1.2b wave region top MRIP GOM")

## SA ----
sa_acl_top_to_plot %>%
  add_to_report_xls("1.2b wave region top MRIP SA")

# 1) By wave region 3c) All FHIER spp ----
## GOM ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot$gom %>%
  add_to_report_xls("1.3c wave region all FHIER GOM")

## SA ----
fhier_acl_catch_by_species_state_region_waves_list_for_plot$sa %>%
  add_to_report_xls("1.3c wave region all FHIER SA")

# 2) By wave and state
View(state_wave_has_rec_acl_data_list_new)
# 3 sets of spp:
# 1a) SEDAR lists;
# 2b) MRIP / Recreational ACL tops;
# 3c) All FHIER spp

report_file_path2 <- file.path(my_paths$outputs,
                                    r"(\compare_catch\report\12_categories_tables_states.xlsx)")

# 1a) SEDAR lists;
map(names(state_wave_has_rec_acl_data_list_state_sedar),
    function(current_state_abbr) {
      sheet_name = paste0("2.1a wave state SEDAR lists ", current_state_abbr)
      # browser()
      state_wave_has_rec_acl_data_list_state_sedar[[current_state_abbr]] %>%
        add_to_report_xls(sheet_name, report_file_path = report_file_path2)
    })
    
