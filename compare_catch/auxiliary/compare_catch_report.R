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
                r"(compare_catch\report\12_categories_tables_reg.xlsx)")
  }
  # browser()
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
fhier_acl_catch_by_species_state_region_waves_list_for_plot_gom_top_sedar %>% 
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

# 2) By wave and state ----

# 3 sets of spp:
# 1a) SEDAR lists;
# 2b) MRIP / Recreational ACL tops;
# 3c) All FHIER spp

report_file_path2_states <- file.path(my_paths$outputs,
                                    r"(compare_catch\report\12_categories_tables_states.xlsm)")

## 1a) SEDAR lists ----
# c("sa", "gom")
View(state_wave_has_rec_acl_data_list_state_sedar)

map(names(state_wave_has_rec_acl_data_list_state_sedar),
    function(sa_or_gom) {
      # browser()
      current_df_list <-
        state_wave_has_rec_acl_data_list_state_sedar[[sa_or_gom]]
      map(names(current_df_list),
          function(current_state_abbr) {
            sheet_name = paste("2.1a wave state SEDAR", sa_or_gom, current_state_abbr)
            # browser()
            current_df_list[[current_state_abbr]] %>%
              add_to_report_xls(sheet_name, report_file_path = report_file_path2_states)
          })
    })
# if file exists gives an error:
# ! no field, method or inner class called 'use_cli_format' 

# 2) year and state
## 2b) MRIP / Recreational ACL tops ----

# View(state_wave_has_rec_acl_data_list_state_top_mrip)
# View(state_wave_has_rec_acl_data_list_state_sedar)
map(names(state_wave_has_rec_acl_data_list_state_top_mrip),
    function(sa_or_gom) {
      # browser()
      current_df_list <-
        state_wave_has_rec_acl_data_list_state_top_mrip[[sa_or_gom]]
      map(names(current_df_list),
          function(current_state_abbr) {
            sheet_name = paste("2 2b wave state top MRIP", sa_or_gom, current_state_abbr)
            # browser()
            current_df_list[[current_state_abbr]] %>%
              add_to_report_xls(sheet_name, report_file_path = report_file_path2_states)
          })
    })

## 3c) All FHIER spp ----
# View(fhier_acl_catch_by_species_state_region_waves_states_list)

wave_sate_all_fhier_spp <-
  fhier_acl_catch_by_species_state_region_waves_states_list %>%
  map(remove_no_mrip_cnts)

map(names(wave_sate_all_fhier_spp),
    function(sa_or_gom) {
      # browser()
      current_df_list <-
        wave_sate_all_fhier_spp[[sa_or_gom]]
      map(names(current_df_list),
          function(current_state_abbr) {
            sheet_name = paste("2 3c wave st", current_state_abbr, toupper(sa_or_gom), "All FHIER spp.")
            # browser()
            current_df_list[[current_state_abbr]] %>%
              add_to_report_xls(sheet_name, report_file_path = report_file_path2_states)
          })
    })

# 3) By year and region ----
report_file_path_year <- 
  file.path(my_paths$outputs,
                                    r"(compare_catch\report\12_categories_tables_year.xlsx)")

## 1a) SEDAR lists ----
map(c("sa", "gom"),
    function(sa_or_gom) {
      sheet_name <- paste("3 1a year", sa_or_gom,  "SEDAR")
      # choose a spp list by sa_or_gom
      top_list <- get(paste0(sa_or_gom, "_top_spp"))

      fhier_acl_catch_by_species_region_year_list[[sa_or_gom]] %>%
        dplyr::filter(scientific_name %in% top_list$scientific_name) %>%
        add_to_report_xls(sheet_name, report_file_path = report_file_path_year)
    })

## 2b) MRIP / Recreational ACL tops ----
### SA ----
my_limit <- 2000
sa_or_gom <- "SA"
sheet_name <- paste("3 2b year", sa_or_gom,  "top MRIP")

fhier_acl_catch_by_species_region_year_list$sa %>%
  # keep only top r_acl cnts
  dplyr::filter(rec_acl_cnts_by_year > my_limit) %>%
  add_to_report_xls(sheet_name, report_file_path = report_file_path_year)

### GOM ----
my_limit <- 6000
sa_or_gom <- "GOM"
sheet_name <- paste("3 2b year", sa_or_gom,  "top MRIP")

fhier_acl_catch_by_species_region_year_list$gom %>%
  # keep only top r_acl cnts
  dplyr::filter(rec_acl_cnts_by_year > my_limit) %>%
  add_to_report_xls(sheet_name, report_file_path = report_file_path_year)

## 3c) All FHIER spp ----
map(c("sa", "gom"),
    function(sa_or_gom) {
      sheet_name <- paste("3 3c year", sa_or_gom,  "all species")
      
      fhier_acl_catch_by_species_region_year_list[[sa_or_gom]] %>%
        add_to_report_xls(sheet_name, report_file_path = report_file_path_year)
    })

# 4) By year and state ----
## 1a) SEDAR lists ----

state_year_sedar <-
  # has rec_acl data
  names(state_year_has_rec_acl_data_list_new) %>%
  # repeat for each state
  map(function(state_abbr) {
    sheet_name <-
      paste("4 1a year", state_abbr, "SEDAR spp.")
    
    # get data for this state
    fhier_acl_catch_by_species_state_year_list[[state_abbr]] %>%
      # keep only spp in the SEDAR spp lists
      dplyr::filter(
        scientific_name %in% gom_top_spp$scientific_name |
          scientific_name %in% sa_top_spp$scientific_name
      ) %>%
      add_to_report_xls(sheet_name, report_file_path = report_file_path_year)
  })

# 2b) MRIP / Recreational ACL tops;
names(state_year_has_rec_acl_data_list_new) %>%
  map(function(state_abbr) {
    sheet_name <-
      paste("4.2b year", state_abbr, "top MRIP")
    # get data for this state
    state_year_has_rec_acl_data_list_new[[state_abbr]] %>%
      # keep only top r_acl cnts
            dplyr::filter(
        scientific_name %in% gom_acl_top_spp$scientific_name |
          scientific_name %in% sa_acl_top_spp$scientific_name
      ) %>%
      add_to_report_xls(sheet_name, 
                        report_file_path = report_file_path_year)
  })

## 3c) All FHIER spp ----
names(state_year_has_rec_acl_data_list_new) %>%
  map(function(state_abbr) {
    sheet_name <-
      paste("4 3c year", state_abbr, "all species")
    # get data for this state
    state_year_has_rec_acl_data_list_new[[state_abbr]] %>%
      # write to excel
      add_to_report_xls(sheet_name,
                        report_file_path = report_file_path_year)
  })
