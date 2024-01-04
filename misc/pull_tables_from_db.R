con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

# download and save db tables ----
safis_table_names <-
  c("ADDL_ELEMENTS",
    "ADDRESSES",
    "AREAS_FISHED",
    "CATCH_SOURCE",
    "CATCH_SPECIES_ATTRIBUTES",
    "CATCHES_ADDL_MEASURE",
    "CATCHES",
    "DISPOSITIONS",
    "ECONOMIC_FIELDS",
    "EFFORTS_GEAR_ATTRIBUTES_DFT",
    "EFFORTS",
    "FULL_PARTICIPANT",
    "GEAR_ATTRIBUTES_DFT",
    "GEARS",
    "GRADE_CATEGORIES",
    "MARKET_CATEGORIES",
    "NIGHTLY_JOB_TASK_LOG",
    "NOAA_PRELANDING_LOCATION",
    "PARTICIPANT_ADDRESSES",
    "PARTICIPANT_EMAIL",
    "PARTICIPANTS",
    "PARTNERS",
    "PERMITS",
    "SPECIES",
    "SRFH_ACCSP_OBJ_MAP",
    "STATE",
    "SUBMIT_METHOD",
    "UNIVERSAL_TRIPIDS",
    "VALID_PORTS",
    "VESSELS",
    "VMS_LANDING_LOCATIONS",
    "VTRACK_CATCHES",
    "VTRACK_FIELDS_VS_SAFIS_E",
    "VTRACK_FIELDS_VS_SAFIS",
    "VTRACK_NOTIFICATIONS",
    "VTRACK_TRIPS")

srh_table_names <-
  c("dim_dates",
    "elog_trip",
    "gtt_srfh_vessel_comp_override",
    "hb_report",
    "mv_comp_ole",
    "mv_comp_srfh_trip_w_decl",
    "mv_effort_target",
    "mv_safis_gom_vessels",
    "mv_safis_sa_vessels",
    "mv_safis_trip_download",
    "mv_sero_fh_permits_his",
    "mv_srfh_ole_permit_info",
    "mv_srfh_trips",
    "mv_srh_safis_in_survey_period",
    "mv_srh_safis_vessel_mapping",
    "mv_tms_trip_notifications",
    "mv_vtrack_srfh_notifications",
    "ole_violation_data",
    "p_u_srfh_vessel_comp_detail",
    "port",
    "srfh_assignment",
    "srfh_corr_log",
    "srfh_for_hire_type",
    "srfh_vessel_comp",
    "srfh_vessel_comp_del_log",
    "srfh_vessel_comp_detail",
    "srfh_vessel_comp_err",
    "srfh_vessel_comp_err_gom_sd",
    "srfh_vessel_comp_gom_sd",
    "srh_sero_comp_error_type",
    "srh_sero_vessel_comp",
    "srh_sero_vessel_comp_err",
    "srh_vessel_comp_err",
    "trips_new_w_new_filter",
    "udp_sero_vessel_on_hold",
    "user_profile",
    "v_admin_dashboard",
    "v_admin_dashboard_prl",
    "v_car_report",
    "v_comp_capt_not_rep_error",
    "v_comp_corr_vtrack",
    "v_comp_det_error",
    "v_comp_main_error_report",
    "v_comp_srfh_comp_err_detail",
    "v_comp_srfh_detail",
    "v_comp_srfh_gom_error_rt",
    "v_comp_srfh_gom_val_err",
    "v_comp_srfh_main_error_report",
    "v_comp_srfh_max_contact_dt",
    "v_comp_srfh_sa_error_rt",
    "v_comp_srfh_trip_after_pmt",
    "v_comp_srfh_vessel_by_week",
    "v_corr_vtrack",
    "v_pa_dashboard",
    "v_pa_val_summ",
    "v_pa_val_ty_summ",
    "v_participant_info_tn",
    "v_safis_trips",
    "v_safis_trips_neg",
    "v_srfh_vessels",
    "v_tms_trip_notifications",
    "v_val_addtl_fields",
    "v_val_addtl_fields_ori",
    "v_val_srfh_pending",
    "val_log",
    "val_log_srfh",
    "val_param",
    "val_tr_res",
    "val_tr_res_srfh",
    "z_del_srfh_vessel_comp",
    "z_srfh_ves_comp_on_hold")

# through R ----
# current_table <- "VALID_PORTS"
data_from_db <- function(current_table) {
  cut(current_table)
  request_query <-
    str_glue("select * from {table_owner}.{current_table}@secapxdv_dblk.sfsc.noaa.gov"
)

  db_data = dbGetQuery(con,
                       request_query)

  # View(db_data)
  # getwd()
  # "select * from safis.SUBMIT_METHOD@secapxdv_dblk.sfsc.noaa.gov"
  # phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

  csv_path <- file.path("my_inputs/from_db",
                        table_owner,
                        paste0(current_table,
                        ".csv"))
  write_csv(db_data, csv_path)
}

map(safis_table_names, data_from_db)

dbDisconnect(con)

# through oracle sql script ----
# Run in sql developer @"...\download_tables.sql"

# current_table_name <- "VALID_PORTS"
# getwd()

## create a query sql ----
table_owner <- "srh"
file_addr <-
  normalizePath(c(getwd(),
                r"(my_inputs\from_db\{table_owner}\{current_table_name}.csv)"),
                winslash = "\\")

create_a_query_for_one_table <-
  function(current_table_name, table_owner) {
    cat(current_table_name)
    cat(" \n")

    current_table_file_name <-
      stringr::str_glue(r"(my_inputs\from_db\{table_owner}\{current_table_name}.csv)")

    # Convert file paths to canonical form
    file_addr <-
      normalizePath(c(
        "~",
        current_table_file_name,
        winslash = "\\",
        mustWork = FALSE
      ))

    request_query <-
      stringr::str_glue(
        'spool "{file_addr[[2]]}"
SELECT /*+ parallel */* FROM
        {table_owner}.{current_table_name}@secapxdv_dblk.sfsc.noaa.gov;
spool off;
'
      )

    return(request_query)
  }

### create queries for all tables ----
srh_tables_sql <-
  purrr::map(srh_table_names,
      \(current_table_name) {
        create_a_query_for_one_table(current_table_name,
                                     table_owner)
      })

# str(safis_tables_sql)

## write a file ----

sql_query_file_path <-
  paste0("my_inputs/from_db/", table_owner, "_tables_queries.sql")

### open the file ----
sink(sql_query_file_path)

# add to the top of the sql file
writeLines(c("set term off",
             "set feed off",
             "set sqlformat csv"))

# walk, not map, to suppress nulls
# walk() returns the input .x (invisibly).  The return value of .f() is ignored
purrr::walk(srh_tables_sql,
            \(curr_line) cat(curr_line, sep = "\n\n"))

### close the output file ----
sink()

