# get data for logbooks and catch comparison
# see read.me.R

# set up ----
# Turn off the scientific notation
options(scipen = 999)

library(ROracle)

my_year <- "2022"
my_date_beg <- stringr::str_glue('01-JAN-{my_year}')
my_date_end <- stringr::str_glue('31-DEC-{my_year}')

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

glimpse(survey_data_l)

## remove fields with all NAs ----
survey_data_l_not_na <-
  survey_data_l |>
  purrr::map(auxfunctions::remove_empty_cols)

# check
survey_data_l |>
  purrr::imap(\(x, idx) {
    diffdf::diffdf(survey_data_l[[idx]], survey_data_l_not_na[[idx]])
  })
# $aga
#    intcd2  
#    start4  
#     stop4  
#    tsite4  
#   ---------
# $ref
#    la_charter_permit_number 
#       interviewee_m_name    
#       interviewee_suffix    
#   --------------------------

# Pull out 2022 only ----
# survey_data_l |> purrr::map(print_df_names)
survey_data_l_2022 <-
  survey_data_l |>
  purrr::map(~ dplyr::filter(.x, year == my_year))

# check
survey_data_l_2022 |> 
  purrr::map(~dplyr::select(.x, year) |> 
               dplyr::distinct())

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

# dat = dbGetQuery(con, "SELECT * FROM srh.mv_safis_trip_download@secapxdv_dblk
#                  WHERE trip_start_date >= '01-JAN-2022'")

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
    return(DBI::dbGetQuery(con,
                      db_logbooks_query))
  }

try(con <- auxfunctions::connect_to_secpr())

get_db_logbooks <-
  function() {
    read_rds_or_run(db_logbooks_file_name,
                    db_logbooks_query,
                    db_logbooks_fun)
  }

db_logbooks <- get_db_logbooks()
db_logbooks <- get_db_logbooks()
# 2024-02-21 run for logbooks_2022.rds: 55.92 sec elapsed
# File: logbooks_2022.rds modified Wed Feb 21 15:35:26 2024

dim(db_logbooks)
# [1] 327987    149


fhier_logbooks_path_add <- "logbooks_from_fhier"

# all logbooks, by month, not all fields
load_all_fhier_logbooks <- function() {
  fhier_logbooks <-
    list.files(path = file.path(my_paths$inputs,
                                fhier_logbooks_path_add),
               pattern = "*.csv",
               full.names = TRUE)  %>%
    purrr::map_df(~read_csv(.x,
                     name_repair = fix_names,
                     show_col_types = FALSE) %>%
             dplyr::mutate(across(.fns = as.character))) %>%
    # Re-convert character columns
    # guess integer types for whole numbers
    type_convert(guess_integer = TRUE)

  return(fhier_logbooks)
}

fhier_logbooks <- load_all_fhier_logbooks()

# data_overview(fhier_logbooks)

## get "logbooks_from_fhier\FHIER_all_logbook_data.csv"
# is it different from "All logbooks" downloaded from FHIER?

fhier_all_logbook_data_csv <-
  file.path(my_paths$inputs,
            fhier_logbooks_path_add,
            "FHIER_all_logbook_data.csv"
            ) %>%
    read_csv(name_repair = fix_names,
           show_col_types = FALSE) %>%
    # dplyr::mutate(across(.fns = as.character))
# %>%
    # Re-convert character columns
    # guess integer types for whole numbers
    type_convert(guess_integer = TRUE)

# problems(fhier_all_logbook_data_csv)
# data_overview(fhier_all_logbook_data_csv)
