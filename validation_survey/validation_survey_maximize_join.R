#' title: Validation Survey and Logbooks join
#' 
#' 1) get and prepare data separately
#' 2) use fuzzy match to PIMS (from Oracle db) to get more vessel ids for survey
#' 3) join survey with logbooks by day and vessel
#' 4a) if an interview (id_code) has no match in logbooks - check by state/county and captain name
#' 4b) if an interview (id_code) has more than 1 match in logbooks - check by state/county and captain name
#' 
#' 
# Set up ----

library('devtools')

if (!require('auxfunctions')) {
  devtools::install_github("AShipunova1/R_code/auxfunctions@development")
  
  library('auxfunctions')
}

library(lubridate)
library(ROracle)
library(tidycensus)
library(usmap)

Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

my_paths <- auxfunctions::set_work_dir()

#' get this project name
current_project_dir_name <- this.path::this.dir()

#' find its base name
current_project_name <-
  basename(current_project_dir_name)

#' use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
#' %%%%% Get data

get_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_path)

source(get_data_path)

#' Data are in:
#' 
#' survey_data_l_2022
#'
#' processed_logbooks_2022
#'
#' processed_logbooks_2022_calendar
#'
#' db_logbooks_2022
#'
#' db_dnfs_2022
#'
#' permit_info_from_db
#'
#' permits_from_pims__split1_short__split2
#'
#' vessels_from_pims_double_bind
#'
#' vessel_permit_owner_from_db
#'
#' vesl_suppressed_logbooks_clean_2022
#' 
# ---

# Prepare survey data ----
## Get dates from survey's id_code ----
get_date_from_id_code_survey <- 
  function(my_df) {
    my_df__w_dates <-
      my_df |>
      dplyr::mutate(
        id_sps = stringr::str_replace(
          id_code,
          "(\\d{5})(2022)(\\d{2})(\\d{2})(\\d{3})",
          stringr::str_c("\\1 \\2 \\3 \\4 \\5")
        )
      ) |>
      tidyr::separate_wider_delim(
        id_sps,
        delim = " ",
        names = c(
          "assignment_num_sampler_id",
          "int_year",
          "int_month",
          "int_day",
          "intercept_num"
        )
      ) |>
      dplyr::select(-c(assignment_num_sampler_id, intercept_num)) |>
      dplyr::mutate(interview_date =
               lubridate::make_date(int_year, int_month, int_day))
    
    return(my_df__w_dates)
  }

survey_data_l_2022_vsl_date <-
  survey_data_l_2022$i1 |>
  dplyr::select(vsl_num, id_code, time, hrsf) |>
  dplyr::distinct() |>
  get_date_from_id_code_survey()

#' check if correct split
survey_data_l_2022_vsl_date |>
  dplyr::mutate(date_paste = paste0(int_year, int_month, int_day)) |>
  dplyr::rowwise() |>
  # filter(!grepl(date_paste, id_code)) |>
  # 0
  dplyr::filter(grepl(date_paste, id_code)) |>
  dplyr::ungroup() |>
  dim()
# 1835

#' check if correct interview_date
survey_data_l_2022_vsl_date |>
  dplyr::mutate(date_paste = paste0(int_year, int_month, int_day),
         interview_date_str = as.character(interview_date)
           ) |>
  dplyr::mutate(interview_date_str_1 = 
           stringr::str_replace_all(interview_date_str, "\\W", "")) |> 
  dplyr::rowwise() |>
  # filter(!date_paste == interview_date_str_1) |>
  # 0
  dplyr::filter(date_paste == interview_date_str_1) |>
  dplyr::ungroup() |>
  dim()
# 1835

survey_data_l_2022_vsl_date_time <-
  survey_data_l_2022_vsl_date |>
  dplyr::mutate(hour_sec =
           stringr::str_replace(time, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(hour_sec,
                              delim = " ",
                              names = c("int_hour", "int_sec")) |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with("int_"), ~ as.numeric(.x))) |>
  dplyr::mutate(
    interview_date_time =
      lubridate::make_datetime(int_year, int_month, int_day, int_hour, int_sec, tz = Sys.timezone())
  )

# str(survey_data_l_2022_vsl_date_time)

survey_data_l_2022_i1_w_dates <-
  dplyr::inner_join(survey_data_l_2022$i1,
                    survey_data_l_2022_vsl_date_time)

# id_code                  1835
# interview_date            232
# interview_date_time      1780
# vsl_num                   476
# vessel_name               598
# interviewee_f_name        354
# interviewee_l_name        521
# cnty                       23
# st                          6

## clean up survey vessel ids ----
clean_up_survey_vessel_ids <- function(my_df) {
  my_df |>
    dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, " ", "")) |>
    dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, "-", "")) |>
    dplyr::mutate(vsl_num = tolower(vsl_num))
  
}

survey_data_l_2022_i1_w_dates_clean_vsl <-
  survey_data_l_2022_i1_w_dates |>
  clean_up_survey_vessel_ids()

## change NA vsl_num from survey ----
survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num <- 
  survey_data_l_2022_i1_w_dates_clean_vsl |> 
  mutate(survey_vessel_id = tidyr::replace_na(vsl_num, ""))

# View(survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num)

# Prepare PIMS data ----

## vessel_permit_owner_from_db tolower vessel ids ----
vessel_permit_owner_from_db_clean_vsl <-
  vessel_permit_owner_from_db |> 
  mutate(permit_vessel_id = tolower(P_VESSEL_ID))
  
## unify county names ----
words_to_remove <- " county| parish| municipio| islands| island| municipality| district| city"

unify_county_names <- function(my_df, county_col_name) {
  res_df <-
    my_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) |> 
    dplyr::mutate(
      county_short =
        stringr::str_replace_all(!!dplyr::sym(county_col_name),
                                 words_to_remove, "") |>
        stringr::str_replace_all("\\bst\\.* ", "saint ") |>
        stringr::str_replace_all("[^A-z0-9 ]", "") |>
        stringr::str_squish()
    )
  
  return(res_df)  
}

#### unify_county_names in fips code ----
fips_code_to_use <-
  tidycensus::fips_codes |>
  unify_county_names("county")

# glimpse(fips_code_to_use)

#### unify_county_names in PIMS data ----

vessel_permit_owner_from_db_clean_vsl__cln_county <- 
  vessel_permit_owner_from_db_clean_vsl |>
  unify_county_names("SERO_HOME_PORT_COUNTY")

vessel_permit_owner_from_db_clean_vsl__cln_county |> 
  head() |> 
  glimpse()

#' check 
grep("john", 
     vessel_permit_owner_from_db_clean_vsl__cln_county$county_short, 
     value = T) |> 
  unique()
# [1] "saint johns"

grep("john", 
     fips_code_to_use$county_short, 
     value = T) |> 
  unique()

#### shorten pims data ----
pims_data_cols_to_keep <-
  c(
    "SERO_HOME_PORT_COUNTY",
    "SERO_HOME_PORT_STATE",
    "SERO_OFFICIAL_NUMBER",
    "VESSEL_NAME",
    "PERMIT",
    "P_VESSEL_ID",
    "FIRST_NAME",
    "MIDDLE_NAME",
    "LAST_NAME",
    "STATE",
    "permit_vessel_id",
    "county_short"
  )

vessel_permit_owner_from_db_clean_vsl__cln_county__short <- 
  vessel_permit_owner_from_db_clean_vsl__cln_county |> 
  dplyr::select(tidyselect::all_of(pims_data_cols_to_keep)) |> 
  distinct()

dim(vessel_permit_owner_from_db_clean_vsl__cln_county__short)
# [1] 18992    12

# Fuzzyjoin PIMS & survey by vessel_ids ----
fuzzyjoin_vessel_ids <-
  fuzzyjoin::stringdist_left_join(
    survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num,
    vessel_permit_owner_from_db_clean_vsl__cln_county,
    by = c("survey_vessel_id" = "permit_vessel_id"),
    distance_col = "vessel_id_dist"
  ) |> 
  distinct()

dim(fuzzyjoin_vessel_ids)
# [1] 63758    82

fuzzyjoin_vessel_ids_short <-
  fuzzyjoin_vessel_ids |>
  select(
    id_code,
    vessel_name,
    interviewee_f_name,
    interviewee_l_name,
    interviewee_m_name,
    permit_number1,
    permit_number2,
    vsl_num,
    cnty,
    st,
    comments,
    interview_date,
    survey_vessel_id,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER,
    VESSEL_NAME,
    PERMIT,
    P_VESSEL_ID,
    FIRST_NAME,
    MIDDLE_NAME,
    LAST_NAME,
    STATE,
    permit_vessel_id,
    county_short,
    vessel_id_dist
  ) |>
  distinct()

dim(fuzzyjoin_vessel_ids_short)
# [1] 39068    25

#' too many vessels for the same id_code
#' 
#' filters for fuzzy matching vessel ids
#' permits

permits_filter <-
  rlang::quo(permit_number1 == PERMIT |
               permit_number2 == PERMIT)

fuzzyjoin_vessel_ids_short |>
  filter(!!permits_filter) |>
  dim()
# [1] 5111   25

geo_filter <-
  rlang::quo(permit_number1 == PERMIT |
               permit_number2 == PERMIT)
