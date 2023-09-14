con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

# download and safe db safis ----
table_names <-
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

# through R ----
# current_table <- "VALID_PORTS"
data_from_db <- function(current_table) {
  cut(current_table)
  request_query <-
    str_glue("select * from safis.{current_table}@secapxdv_dblk.sfsc.noaa.gov"
)

  db_data = dbGetQuery(con,
                       request_query)

  # View(db_data)
  # getwd()
  # "select * from safis.SUBMIT_METHOD@secapxdv_dblk.sfsc.noaa.gov"
  # phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

  csv_path <- file.path("my_inputs/from_db/safis",
                        paste0(current_table,
                        ".csv"))
  write_csv(db_data, csv_path)
}

map(table_names, data_from_db)

dbDisconnect(con)

# create a query file ----
# Run in sql developer @"...\download_tables.sql"
# current_table_name <- "VALID_PORTS"
# getwd()

file_addr <- file.path(getwd(),
                       r"(my_inputs\from_db\safis\{current_table_name}.csv)")

create_file_with_queries <-
  function(current_table_name) {

    cat(current_table_name)
    cat(" \n")
    request_query <-
      stringr::str_glue(
        'spool "C:\\Users\\anna.shipunova\\Documents\\R_files_local\\my_inputs\\from_db\\safis\\{current_table_name}.csv";
select * from safis.{current_table_name}@secapxdv_dblk.sfsc.noaa.gov;
spool off;
'
      )

    return(request_query)
  }

safis_tables_sql <-
  map(table_names, create_file_with_queries)

str(safis_tables_sql)

sql_file_path <- file.path("my_inputs/from_db/safis_tables_queries.sql")

sink(sql_file_path)
# walk, not map, to supress nulls
# walk() returns the input .x (invisibly).  The return value of .f() is ignored

purrr::walk(safis_tables_sql,
            \(curr_line) cat(curr_line, sep = "\n\n"))
sink()

