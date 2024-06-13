# get data for logbooks and catch comparison
# see read.me.R

# set up ----
# Turn off the scientific notation
options(scipen = 999)

library(ROracle)

try(con <- auxfunctions::connect_to_secpr())

my_year <- "2022"
my_date_beg <- stringr::str_glue('01-JAN-{my_year}') |> 
  lubridate::dmy()
my_date_end <- stringr::str_glue('31-DEC-{my_year}') |> 
  lubridate::dmy()

# load Validation Survey data ----
# https://drive.google.com/drive/folders/1JDlzVXcTkdY17Sux8hZOZbxFnj2_E9eh?usp=drive_link
# Dominique Lazarre, Sept 14, 2023
# "my_inputs\validation_survey\Merged_Validation_Survey_Data.zip"

validation_survey_data_dir_path <- file.path(my_paths$inputs,
                                         "validation_survey")

# dir.exists(validation_survey_data_dir_path)

# read 
csv_filenames <-
  list.files(validation_survey_data_dir_path,
             pattern = "*.csv",
             full.names = TRUE)

str(csv_filenames)
# 5

# loop through all files from the list and run the function on each one
survey_data_l <-
  csv_filenames |>
  purrr::map(
    ~readr::read_csv(
      .x,
      col_types = readr::cols(.default = 'c'),
      trim_ws = TRUE,
      na = c("", "NA", "NaN"),
      name_repair = auxfunctions::fix_names
    ) |>
      readr::type_convert(guess_integer = TRUE)
  )

## make short names ----
short_names <-
  csv_filenames |>
  purrr::map(basename) |>
  stringr::str_replace("([^_]+)_.+", "\\1")

names(survey_data_l) <- short_names

# dplyr::glimpse(survey_data_l)

## remove fields with all NAs ----
survey_data_l_not_na <-
  survey_data_l |>
  purrr::map(auxfunctions::remove_empty_cols)

# check
# survey_data_l |>
#   purrr::imap(\(x, idx) {
#     diffdf::diffdf(survey_data_l[[idx]], survey_data_l_not_na[[idx]])
#   })
# $aga
#    intcd2  
#    start4  
#     stop4  
#    tsite4  
# $ref
#    la_charter_permit_number 
#       interviewee_m_name    
#       interviewee_suffix    

# Pull out 2022 only ----
# survey_data_l |> purrr::map(print_df_names)
survey_data_l_2022 <-
  survey_data_l |>
  purrr::map(~ dplyr::filter(.x, year == my_year))

# check
survey_data_l_2022 |>
  purrr::map(~ dplyr::select(.x, year) |>
                dplyr::distinct())
# all 2022 only, ok

survey_data_l_2022 |> 
  purrr::map(dim)
# $aga
# [1] 1175   39
# 
# $i1
# [1] 1835   33
# 
# $i2
# [1] 3060    9
# 
# $i3
# [1] 11333    12
# 
# $ref
# [1] 19 33

# ---- write survey_data_l_2022 out ----

survey_data_l_2022 |>
  readr::write_rds(file.path(curr_proj_output_path, "survey_data_l_2022.rds"))

# ---- get logbooks from FHIER - not enough fields ----

## processed logbooks ----
processed_logbooks_2022 <-
  readr::read_rds(
    file.path(
      my_paths$inputs,
      r"(processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2022.rds)"
    )
  )

dim(processed_logbooks_2022)
# [1] 330441    179

grep("permit", names(processed_logbooks_2022), ignore.case = T, value = T)
# permit_sa_gom

processed_logbooks_2022_calendar <-
  processed_logbooks_2022 |>
  dplyr::filter(TRIP_END_DATE >= my_date_beg &
           TRIP_START_DATE <= my_date_end)

nrow(processed_logbooks_2022_calendar) -
  nrow(processed_logbooks_2022)
# [1] -4712

db_logbooks_query <-
  stringr::str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_end_date >= '{my_date_beg}'
  AND trip_start_date <= '{my_date_end}'
")

db_logbooks_file_name <-
  file.path(curr_proj_input_path,
                      stringr::str_glue("logbooks_db_{my_year}.rds"))

file.exists(db_logbooks_file_name)

db_logbooks_fun <-
  function(db_logbooks_query) {
    return(try(DBI::dbGetQuery(con,
                      db_logbooks_query)))
  }

get_db_logbooks <-
  function() {
    auxfunctions::read_rds_or_run(db_logbooks_file_name,
                    db_logbooks_query,
                    db_logbooks_fun)
  }

db_logbooks_2022 <- 
  get_db_logbooks() |> 
  auxfunctions::remove_empty_cols()
# 2024-05-25 run for logbooks_db_2022.rds: 162.69 sec elapsed
# File: logbooks_db_2022.rds modified Sat May 25 12:04:54 2024

dim(db_logbooks_2022)
# [1] 328086    149
# [1] 328086    128

# check db logbooks vs. processed logbooks ----
grep("permit", names(db_logbooks_2022), ignore.case = T, value = T)
# [1] "ACCSP_PERMIT_LICENSE_NBR" "SERO_VESSEL_PERMIT"       "GARFO_VESSEL_PERMIT"     

n_distinct(db_logbooks_2022$TRIP_ID)
# 94857
n_distinct(processed_logbooks_2022$TRIP_ID)
# 95342
n_distinct(processed_logbooks_2022_calendar$TRIP_ID)
# 94104

trips_in_db_not_in_processed <-
  setdiff(db_logbooks_2022$TRIP_ID, processed_logbooks_2022$TRIP_ID)
# 753

trips_in_db_not_in_processed_cal <-
  setdiff(db_logbooks_2022$TRIP_ID,
          processed_logbooks_2022_calendar$TRIP_ID)
length(trips_in_db_not_in_processed_cal)
# 753

trips_in_processed_not_in_db <-
  setdiff(processed_logbooks_2022$TRIP_ID, db_logbooks_2022$TRIP_ID)
length(trips_in_processed_not_in_db)
# 1238

trips_in_processed_cal_not_in_db <-
  setdiff(processed_logbooks_2022_calendar$TRIP_ID,
          db_logbooks_2022$TRIP_ID)
length(trips_in_processed_cal_not_in_db)
# 0 ok

processed_logbooks_2022 |> 
  dplyr::filter(TRIP_ID %in% trips_in_processed_not_in_db) |> 
  # filter(!permit_sa_gom == "sa_only") |> 
  dplyr::select(COMP_WEEK_START_DT, COMP_WEEK_END_DT) |> 
    dplyr::distinct()
#   COMP_WEEK_START_DT  COMP_WEEK_END_DT   
#   <dttm>              <dttm>             
# 1 2022-12-26 00:00:00 2023-01-01 00:00:00
# 2 2021-12-27 00:00:00 2022-01-02 00:00:00

db_logbooks_2022 |>
  dplyr::filter(TRIP_ID %in% trips_in_db_not_in_processed) |>
  dplyr::select(TRIP_START_DATE, TRIP_END_DATE) |>
  dplyr::distinct() |> 
  dim()
# [1] 352   2
# all diff
min(db_logbooks_2022$TRIP_START_DATE)
# [1] "2021-12-31 23:00:00 EST"

min(processed_logbooks_2022_calendar$TRIP_START_DATE)
# [1] "2022-01-01"

# get DNF ----
db_dnfs_query <-
  stringr::str_glue(
    "SELECT
  trip_id,
  trip_date,
  tn.vessel_id vessel_id,
  tn.de,
  tn.ue,
  coast_guard_nbr,
  state_reg_nbr,
  sero_official_number vessel_official_number
FROM
       safis.trips_neg@secapxdv_dblk.sfsc.noaa.gov tn
  JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
  ON ( tn.vessel_id = v.vessel_id )
WHERE
    trip_date BETWEEN TO_DATE('{my_date_beg}', 'yyyy-mm-dd') AND
TO_DATE('{my_date_end}', 'yyyy-mm-dd')
"
  )

db_dnfs_file_name <-
  file.path(curr_proj_input_path,
                      stringr::str_glue("dnfs_db_{my_year}.rds"))

file.exists(db_dnfs_file_name)

db_dnfs_fun <-
  function(db_dnfs_query) {
    return(try(DBI::dbGetQuery(con, db_dnfs_query)))
  }

get_db_dnfs <-
  function() {
    auxfunctions::read_rds_or_run(db_dnfs_file_name,
                    db_dnfs_query,
                    db_dnfs_fun)
  }

db_dnfs_2022 <-
  get_db_dnfs() |>
  auxfunctions::remove_empty_cols()

dim(db_dnfs_2022)
# [1] 804410      8

# result names ----
data_names <-
  c("survey_data_l_2022",
    "processed_logbooks_2022",
    "processed_logbooks_2022_calendar",
    "db_logbooks_2022",
    "db_dnfs_2022")

auxfunctions::pretty_print(my_title = "Data are in:", 
                           my_text = data_names)
