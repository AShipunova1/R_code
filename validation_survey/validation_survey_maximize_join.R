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

# Explanations:
# - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
# - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
# 
# This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
if (!require('auxfunctions')) {
  devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = TRUE)
  
  library('auxfunctions')
}

library(lubridate)
library(ROracle)
library(tidycensus)
library(usmap)
library(stringdist)

## Install and attach R packages for googlesheets ----
# https://felixanalytix.medium.com/how-to-read-write-append-google-sheet-data-using-r-programming-ecf278108691#:~:text=There%20are%203%20ways%20to%20read%20this%20Google%20sheet%20into%20R.&text=Just%20to%20take%20the%20URL,URL%20but%20just%20the%20ID).
if (!require(googlesheets4)) install.packages("googlesheetsa") 
if (!require(googledrive)) install.packages("googledrive") 
library(googlesheets4) # Google Sheets via the Sheets API v4 
library(googledrive) # interact with Google Drive 

#' don't convert long numbers to scientific notation
options(scipen = 999)

#' Keep the same timezone across subsystems
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

#' Change the following 2 lists to your environment if needed. The variable names are used throughout the code, so please change only the values inside the lists.

if (auxfunctions::get_username() == "anna.shipunova") {
  # If the condition is true, use Anna's directory structure
  my_paths <- auxfunctions::set_work_dir()
  current_in_out_paths <- auxfunctions::current_project_paths()
  
} else {
  
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  #' 1) General directories (to look up additional files, e.g. processed data)
  my_paths <- list(inputs  = "~/my_inputs",
                   outputs = "~/my_outputs",
                   git_r   = "~/R_code")
  
  #' 2) Current project code, input and output directories
  current_in_out_paths <-
    list(
      project_name = "validation_survey",
      code = "~/validation_survey/code",
      input = "~/validation_survey/input",
      output = "~/validation_survey/output"
    )
}

#' The following section uses provided directory names lists to create separate variables for future use and create current input/output directories if they do not exists.
#' 

current_project_name <- current_in_out_paths$project_name

current_project_dir_name <- current_in_out_paths$code
            
curr_proj_input_path <- current_in_out_paths$input

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- current_in_out_paths$output

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

# Aux methods ----
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

## shorten survey data ----
survey_data_cols_to_keep <-
  c(
    "id_code",
    "vessel_name",
    "interviewee_f_name",
    "interviewee_l_name",
    "interviewee_m_name",
    # "prefix1",
    # "prefix2",
    "vsl_num",
    "cnty",
    "st",
    "interview_date",
    "survey_vessel_id"
  )

survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short <-
  survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num |>
  dplyr::select(tidyselect::all_of(survey_data_cols_to_keep)) |>
  distinct()

dim(survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short)
# [1] 1835   10

## Restore state by vessel_id, cnty ----
#' For the same vessel same cnty, st is NA or the same
#' Group by vessel/county and use the same state for this vessel if NA
#' 
#' Combine states in a list by vessel and county
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v <- 
  survey_data_l_2022_i1_w_dates_clean_vsl |>
  dplyr::select(vsl_num, st, cnty) |>
  dplyr::distinct() |>
  dplyr::group_by(vsl_num, cnty) |>
  dplyr::mutate(st_with_char_na =
                  dplyr::case_when(is.na(st) ~ "NA", .default = st)) |>
  dplyr::mutate(states_l_by_cnty_v = list(paste(unique(
    sort(st_with_char_na)
  )))) |>
  dplyr::ungroup() |>
  dplyr::select(-st_with_char_na) |>
  dplyr::distinct() |>
  dplyr::arrange(vsl_num, cnty)

#' check: for each vessel_county there are no more than 2 states
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v |>
  rowwise() |> 
  filter(length(states_l_by_cnty_v) > 2) |> 
  glimpse()
# 0
  
#' restore state for each vessel/county if possible,
#' collapse "NA" and the state code in a string and extract the code only
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored <- 
  survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v |>
  dplyr::rowwise() |>
  dplyr::mutate(temp_res =
                  case_when(is.na(st) ~
                              paste(unlist(states_l_by_cnty_v), 
                                collapse = ""), 
                            .default = st)) |>
  dplyr::mutate(restored_st =
                  stringr::str_extract(temp_res, "\\d+")) |>
  dplyr::select(-temp_res) |>
  dplyr::ungroup()

#' check 
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored |> 
  distinct() |>
  head() |> 
  glimpse()

## format state and county codes ----
format_state_and_county_codes <-
  function(my_df, state_code_field) {
    my_df |>
      dplyr::mutate(st_2 =
               case_when(is.na(!!sym(state_code_field)) ~ "00", .default =
                           stringr::str_pad(!!sym(state_code_field), 2, pad = "0"))) |>
      dplyr::mutate(cnty_3 = stringr::str_pad(cnty, 3, pad = "0"),
             fips = paste0(st_2, cnty_3))
  }

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips <-
  survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored |>
  format_state_and_county_codes("restored_st")

glimpse(survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips)

### shorten restored st df ----
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short <-
  survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips |>
  select(vsl_num, st_2, cnty_3) |>
  distinct()

dim(survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short)
# 430

n_distinct(survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short$vsl_num)
# 429

#' why diff? 
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short |> 
     count(vsl_num) |> 
     filter(n > 1)
# A tibble: 1 × 2
  # vsl_num     n
# 1 999999      2
# ok

## combine processed survey data and restored states ----

survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short__restored_st <-
  survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short |>
  left_join(
    survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short,
    relationship = "many-to-many"
  )
# Joining with `by = join_by(vsl_num)`

#' check
#   Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 200 of `x` matches multiple rows in `y`.
# ℹ Row 117 of `y` matches multiple rows in `x`.

survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short[200,]$vsl_num == "999999"
# T

survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short |>
  filter(
    vsl_num ==
      survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips__short[117, ]$vsl_num
  ) |>
  glimpse()
#' same vsl has many id_codes, correct

    
# Prepare FIPS codes ----
## unify_county_names in fips code ----
fips_code_to_use <-
  tidycensus::fips_codes |>
  unify_county_names("county") |> 
  distinct()

#' there are duplicates
#      state state_code state_name county_code           county county_short
# 1196    md         24   maryland         005 baltimore county    baltimore
# 1217    md         24   maryland         510   baltimore city    baltimore

# Prepare PIMS data ----

## tolower vessel ids ----
vessel_permit_owner_from_db_clean_vsl <-
  vessel_permit_owner_from_db |> 
  mutate(permit_vessel_id = tolower(P_VESSEL_ID))

## unify_county_names in PIMS data ----

vessel_permit_owner_from_db_clean_vsl__cln_county <- 
  vessel_permit_owner_from_db_clean_vsl |>
  unify_county_names("SERO_HOME_PORT_COUNTY")

vessel_permit_owner_from_db_clean_vsl__cln_county |> 
  head() |> 
  glimpse()

#' compare counties PIMS vs. FIPS
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
    "SERO_HOME_PORT_CITY",
    "SERO_HOME_PORT_COUNTY",
    "SERO_HOME_PORT_STATE",
    "SERO_OFFICIAL_NUMBER",
    "VESSEL_NAME",
    # "PERMIT",
    "FIRST_NAME",
    "MIDDLE_NAME",
    "LAST_NAME",
    "county_short"
  )

#' check vessel_id columns
vessel_permit_owner_from_db_clean_vsl__cln_county |>
  filter(!SERO_OFFICIAL_NUMBER == P_VESSEL_ID) |> 
  nrow()
# 0

vessel_permit_owner_from_db_clean_vsl__cln_county |>
  filter(!SERO_OFFICIAL_NUMBER == permit_vessel_id) |> 
  nrow()
# 0

#' we can use either of these cols

vessel_permit_owner_from_db_clean_vsl__cln_county__short <- 
  vessel_permit_owner_from_db_clean_vsl__cln_county |> 
  dplyr::select(tidyselect::all_of(pims_data_cols_to_keep)) |> 
  distinct()

dim(vessel_permit_owner_from_db_clean_vsl__cln_county__short)
# 6034

## add fips codes to pims data ----

vessel_permit_owner_from_db_clean_vsl__cln_county__short__fips <-
  left_join(
    vessel_permit_owner_from_db_clean_vsl__cln_county__short,
    fips_code_to_use,
    join_by(SERO_HOME_PORT_STATE == state, county_short),
    relationship = "many-to-many"
  )

### check relationship = "many-to-many" ----
#' ℹ Row 4953 of `x` matches multiple rows in `y`.

vessel_permit_owner_from_db_clean_vsl__cln_county__short_4953 <-
  vessel_permit_owner_from_db_clean_vsl__cln_county__short[4953, ]

fips_code_to_use |>
  filter(
    county_short == vessel_permit_owner_from_db_clean_vsl__cln_county__short_4953$county_short &
      state == vessel_permit_owner_from_db_clean_vsl__cln_county__short_4953$SERO_HOME_PORT_STATE
  )
#      state state_code state_name county_code           county county_short
# 1196    md         24   maryland         005 baltimore county    baltimore
# 1217    md         24   maryland         510   baltimore city    baltimore
#' duplicates in FIPS county_short
#' Check manually in further analysis

#' ℹ Row 380 of `y` matches multiple rows in `x`.

fips_code_to_use_380 <-
  fips_code_to_use[380, ]

vessel_permit_owner_from_db_clean_vsl__cln_county__short |>
  filter(
    county_short == fips_code_to_use_380$county_short &
      SERO_HOME_PORT_STATE == fips_code_to_use_380$state
  ) |> 
  glimpse()
#' Correct, many vessels have the same st & county

#' check survey vsl id
survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short |> 
  filter(is.na(vsl_num)) |> 
  glimpse()
#' no vsl_num!

# Fuzzyjoin PIMS & survey by vessel_ids ----
fuzzyjoin_vessel_ids <-
  fuzzyjoin::stringdist_left_join(
    survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short__restored_st,
    vessel_permit_owner_from_db_clean_vsl__cln_county__short__fips,
    by = c("survey_vessel_id" = "SERO_OFFICIAL_NUMBER"),
    distance_col = "vessel_id_dist"
  ) |> 
  distinct()

dim(fuzzyjoin_vessel_ids)
# [1] 12088    27

## keep each vessel in only one distance group ----

# Note the extra distance column, which in this case will always be less than or equal to 2. We could then pick the closest match for each.

fuzzyjoin_vessel_ids__closest <- 
  fuzzyjoin_vessel_ids %>%
  group_by(survey_vessel_id) %>%
  slice_min(vessel_id_dist, n = 1) %>%
  ungroup()

#' check
dim(fuzzyjoin_vessel_ids__closest)
# [1] 4355   27

n_distinct(fuzzyjoin_vessel_ids$SERO_OFFICIAL_NUMBER)
# 832

n_distinct(fuzzyjoin_vessel_ids__closest$survey_vessel_id)
# 429

n_distinct(fuzzyjoin_vessel_ids__closest$SERO_OFFICIAL_NUMBER)
# 376

#' check a vessel with no id 
fuzzyjoin_vessel_ids__closest |> 
  filter(tolower(vessel_name) == "yellowfin") |> 
  glimpse()
#' same vsl name, diff names and geo

glimpse(fuzzyjoin_vessel_ids__closest)

# too many vessels for the same id_code ----
#' 
#' filters for fuzzy matching vessel ids

## by geo ----
geo_filter <-
  rlang::quo(state_code == st_2 &
               county_code == cnty_3)

# fuzzyjoin_vessel_ids__closest__geo <- 
#   fuzzyjoin_vessel_ids__closest |> 
#   mutate(same_geo = case_when(!!geo_filter))

#' same cnty/state
fuzzyjoin_vessel_ids__closest |> 
  filter(!!geo_filter) |> 
  dim()
# [1] 2930   27

#' different cnty/state

fuzzyjoin_vessel_ids__closest__diff_geo <-
  fuzzyjoin_vessel_ids__closest |>
  filter(!(!!geo_filter))

fuzzyjoin_vessel_ids__closest__diff_geo |> 
  dim()
# [1] 1272   27

glimpse(fuzzyjoin_vessel_ids__closest__diff_geo)

## by captain/interviewee names ----

#' unify names
to_clean_names <- function(name) {
  name |>
    tolower() |>
    stringr::str_replace_all(" ii+", " i") |> 
    stringr::str_replace_all("\\W+", "") |> 
    stringr::str_squish()
}

#' clean names 
fuzzyjoin_vessel_ids__closest__clean_name <-
  fuzzyjoin_vessel_ids__closest |>
  dplyr::mutate(dplyr::across(
    c(
      "interviewee_f_name",
      "FIRST_NAME",
      "interviewee_l_name",
      "LAST_NAME"
    ),
    ~ to_clean_names(.)
  )) |> 
  distinct()

dim(fuzzyjoin_vessel_ids__closest__clean_name)
# [1] 4355   27

name_filter <-
    rlang::quo(interviewee_f_name == FIRST_NAME &
               interviewee_l_name == LAST_NAME)
# interviewee_m_name == MIDDLE_NAME              

#' same name
fuzzyjoin_vessel_ids__closest__clean_name |> 
  filter(!!name_filter) |> 
  dim()
# [1] 159  25

#' diff name 
fuzzyjoin_vessel_ids__closest__clean_name |> 
  filter(!(!!name_filter)) |> 
  dim()
# [1] 2408   27

## by vessel names ----
fuzzyjoin_vessel_ids__closest__clean_vsl_name <-
  fuzzyjoin_vessel_ids__closest |>
  dplyr::mutate(dplyr::across(c("vessel_name", "VESSEL_NAME"), 
                              ~ to_clean_names(.))) |> 
  distinct()

dim(fuzzyjoin_vessel_ids__closest__clean_vsl_name)
# [1] 4355   27

vsl_name_filter <-
  rlang::quo(vessel_name == VESSEL_NAME)

#' same vessel name
fuzzyjoin_vessel_ids__closest__clean_vsl_name |> 
  filter(!!vsl_name_filter) |> 
  dim()
# [1] 2380   27

#' diff vessel name 
fuzzyjoin_vessel_ids__closest__clean_vsl_name |> 
  filter(!(!!vsl_name_filter)) |> 
  dim()
# [1] 1834   27

# 1834 + 2380 = 4214

#' check
diff_vessel_names <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name |>
  filter(!(!!vsl_name_filter)) |>
  select(vessel_name, VESSEL_NAME) |>
  distinct()

glimpse(diff_vessel_names)

### measure vessel names distance and check again ----
#' measure similarity between vessel_name and VESSEL_NAME
#' turn into vsl_names_dissim so it looks close to Levenstein distance (as in vessel_id fuzzy match)

fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist <- 
  fuzzyjoin_vessel_ids__closest__clean_vsl_name |> 
  group_by(SERO_OFFICIAL_NUMBER) |> 
  mutate(vsl_names_dist = 
           stringdist::stringsim(vessel_name, VESSEL_NAME)) |> 
  mutate(vsl_names_dist_round = round(vsl_names_dist, 1),
         vsl_names_dissim = 1 - vsl_names_dist_round) |> 
  ungroup()

#' check 
fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  select(SERO_OFFICIAL_NUMBER, vsl_names_dissim) |> 
  distinct() |> 
  count(vsl_names_dissim)
  # count(wt = n)
  # 456

fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  select(SERO_OFFICIAL_NUMBER, survey_vessel_id, vsl_names_dissim) |> 
  distinct() |> 
  count(vsl_names_dissim)

#' similar vessel name
#' check the threshold 
fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  filter(vsl_names_dissim == 0.4) |> 
    select(vessel_name, VESSEL_NAME) |> 
    distinct() |> 
    glimpse()
#' not good enough

fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  filter(vsl_names_dissim <= 0.3) |> 
  select(survey_vessel_id) |> 
  distinct() |> 
  dim()
# 250

fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  mutate(vsl_names_category = cut(
    vsl_names_dissim,
    breaks = c(-Inf, 0.0, 0.3, Inf),
    labels = c("same", "similar", "dissimilar")
  )) |> 
  select(survey_vessel_id, vsl_names_category) |> 
  distinct() |> 
  count(vsl_names_category)
# 1 same                 233
# 2 similar               43
# 3 dissimilar           120
# 4 NA                    75

fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |> 
  select(survey_vessel_id, SERO_OFFICIAL_NUMBER,
         vessel_name, VESSEL_NAME,
         vsl_names_dissim) |> 
  distinct() |> 
  head() |> 
  glimpse()

## combine all filters ----
### check filters combination ----
#' the result has at least one filter match 

fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs_pass <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |>
  filter(!!geo_filter |
           !!name_filter |
           !!vsl_name_filter |
           vsl_names_dissim <= 0.3)

#' check dist 2
fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs_pass |>
  filter(vessel_id_dist == 2) |>
  select(
    -c(
      id_code,
      interview_date,
      vsl_num,
      cnty,
      st,
      SERO_HOME_PORT_CITY,
      vsl_names_dist,
      vsl_names_dist_round
    )
  ) |>
  distinct() |>
  head() |>
  glimpse()

#' check survey_vessel_id amount

n_distinct(fuzzyjoin_vessel_ids__closest$survey_vessel_id)
# 429

n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist$survey_vessel_id)
# 429
#' OK, same

n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs_pass$survey_vessel_id)
# 290
# Only these who passed

#' check SERO_OFFICIAL_NUMBER amount
n_distinct(fuzzyjoin_vessel_ids__closest$SERO_OFFICIAL_NUMBER)
# 376

n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist$SERO_OFFICIAL_NUMBER)
# 376

n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs_pass$SERO_OFFICIAL_NUMBER)
# 258
# OK, Only these who passed

# 258*100/832
# 31% vessels pass 1 of the filters

### mark if passed any of the filters ----
fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |>
  mutate(
    passed_a_filter = case_when(
      !!geo_filter |
        !!name_filter |
        !!vsl_name_filter |
        vsl_names_dissim <= 0.3 ~
        "pass",
      .default = "not pass"
    )
  )

#' check 
n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs$SERO_OFFICIAL_NUMBER)
# 376, correct, see above

# View(fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs)

fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs |>
  select(survey_vessel_id, SERO_OFFICIAL_NUMBER, passed_a_filter) |>
  distinct() |>
  count(passed_a_filter)
#   passed_a_filter     n
# 1 not pass          220
# 2 pass              290
# 220*100/(220+290)
# 43.13725% not passed any filters

### Check combinations of vessel_ids distance and additional filters ---- 
fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs |>
  select(survey_vessel_id, SERO_OFFICIAL_NUMBER, vessel_id_dist, passed_a_filter) |>
  distinct() |>
  count(passed_a_filter, vessel_id_dist)

#   passed_a_filter vessel_id_dist     n
# 1 not pass                     0    14
# 2 not pass                     1     8
# 3 not pass                     2   123
# 4 not pass                    NA    75
# 5 pass                         0   250
# 6 pass                         1    22
# 7 pass                         2    18

fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs |> 
  filter(passed_a_filter == "not pass" & vessel_id_dist == 0) |> 
  distinct() |> 
  glimpse()

columns_to_remove_from_marked_filters <- 
  c(
    "interviewee_m_name",     # not used
    "MIDDLE_NAME",            # not used
    "SERO_HOME_PORT_CITY",    # not used
    "county",                 # full name, not used
    "cnty",                   # use cnty_3
    "st",                     # use st_2
    "vsl_num",                # same as survey_vessel_id,
    "vsl_names_dist",         # use vsl_names_dissim
    "vsl_names_dist_round"    # use vsl_names_dissim
  )

survey_n_pims__same_vsl_id__diff_all_else <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs |>
  # to prevent sci notation
  mutate(id_code = as.character(id_code)) |> 
  filter(passed_a_filter == "not pass" & vessel_id_dist == 0) |>
  select(-all_of(columns_to_remove_from_marked_filters)) |>
  distinct()

# mark passed filters ----
fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__vsl_name_dist |>
  mutate(
    st_pass = case_when(state_code == st_2 ~ "st_pass"),
    cnty_pass = case_when(county_code == cnty_3 ~ "cnty_pass"),
    name_pass = case_when(!!name_filter ~ "same_names_pass"),
    vsl_name_pass = case_when(vsl_names_dissim <= 0.3 ~ "similar_vsl_name_pass")
  )

## sorten marked filters df ----
fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs |>
  select(-all_of(columns_to_remove_from_marked_filters)) |>
  # to prevent sci notation
  mutate(id_code = as.character(id_code)) |> 
  distinct()

dim(fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv)
# [1] 4355   24

# glimpse(survey_n_pims__same_vsl_id__diff_all_else)

#' check            vessel_id_dist     n
# 6 pass                         1    22
# 7 pass                         2    18

survey_n_pims__not_vsl_id__ok_filters <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__all_filtrs |>
  filter(passed_a_filter == "pass" & vessel_id_dist %in% c(1, 2)) |>
  select(-all_of(columns_to_remove_from_marked_filters)) |>
  distinct()

glimpse(survey_n_pims__not_vsl_id__ok_filters)

#' uncomment if needed
# Write to google sheets ----
# my_ss <- gs4_find("validation_survey")

# write_sheet(
#   survey_n_pims__same_vsl_id__diff_all_else,
#   ss = my_ss,
#   sheet = "survey_n_pims__same_vsl_id__diff_all_else"
# )

# write_sheet(
#   fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv,
#   ss = my_ss,
#   sheet = "survey_n_pims_by_vessel_ids_fuzzy_join__filtrs"
# )

#' result survey with PIMS is in 
#' fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv

# Join logbooks ----
#' TODO: mv to a separate file

# prepare logbooks ----
db_logbooks_2022_clean_vesl <-
  db_logbooks_2022 |>
  dplyr::mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR))

## shorten
#' as.character(TRIP_ID) to avoid sci notation in csv
db_logbooks_2022_short0 <-
  db_logbooks_2022_clean_vesl |>
  dplyr::select(
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    TRIP_START_DATE,
    TRIP_START_TIME,
    TRIP_END_DATE,
    TRIP_END_TIME
  ) |>
  dplyr::distinct() |> 
  dplyr::mutate(trip_end_date_only = lubridate::date(TRIP_END_DATE),
                TRIP_ID = as.character(TRIP_ID))
  
db_logbooks_2022_short_date_time <-
  db_logbooks_2022_short0 |>
  dplyr::mutate(start_hour_sec =
           stringr::str_replace(TRIP_START_TIME, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(
    start_hour_sec,
    delim = " ",
    names = c("trip_start_hour", "trip_start_sec")
  ) |>
  dplyr::mutate(end_hour_sec =
           stringr::str_replace(TRIP_END_TIME, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(end_hour_sec,
                              delim = " ",
                              names = c("trip_end_hour", "trip_end_sec")) |>
  dplyr::mutate(dplyr::across(
    c(
      "trip_start_hour",
      "trip_start_sec",
      "trip_end_hour",
      "trip_end_sec"
    ),
    ~ as.numeric(.x)
  )) |>
  dplyr::mutate(
    trip_start_date_time = lubridate::make_datetime(
      lubridate::year(TRIP_START_DATE),
      lubridate::month(TRIP_START_DATE),
      lubridate::day(TRIP_START_DATE),
      as.numeric(trip_start_hour),
      as.numeric(trip_start_sec),
      tz = Sys.timezone()
    )
  ) |>
  dplyr::mutate(
    trip_end_date_time = lubridate::make_datetime(
      lubridate::year(TRIP_END_DATE),
      lubridate::month(TRIP_END_DATE),
      lubridate::day(TRIP_END_DATE),
      as.numeric(trip_end_hour),
      as.numeric(trip_end_sec),
      tz = Sys.timezone()
    )
  )

# str(db_logbooks_2022_short_date_time)

#' logbooks data to use
#' db_logbooks_2022_short_date_time 

# JOIN interview (fuzyy joined to PIMS) and logbooks by day and vessel ----
lgb_join_i1 <-
  dplyr::right_join(
    db_logbooks_2022_short_date_time,
    fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv,
    dplyr::join_by(
      VESSEL_OFFICIAL_NBR == SERO_OFFICIAL_NUMBER,
      trip_end_date_only == interview_date
    ),
    relationship = "many-to-many"
  )

## Investigate relationship = "many-to-many" ----
#' ℹ Row 193  of `x` matches multiple rows in `y`.
#' ℹ Row 2662 of `y` matches multiple rows in `x`.

lgb_193 <-
  db_logbooks_2022_short_date_time[193, ]

fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv |>
  filter(
    lgb_193$VESSEL_OFFICIAL_NBR == SERO_OFFICIAL_NUMBER,
    lgb_193$trip_end_date_only == interview_date
  ) |> 
  glimpse()
#' many names for the same entry
#' also could be 2 interviews in one day for the same vessel

#' ℹ Row 2662 of `y` matches multiple rows in `x`.
survey_2662 <- 
  fuzzyjoin_vessel_ids__closest__clean_vsl_name__filtrs__to_csv[2662,]

db_logbooks_2022_short_date_time |> 
  filter(
    VESSEL_OFFICIAL_NBR == survey_2662$SERO_OFFICIAL_NUMBER,
    trip_end_date_only == survey_2662$interview_date
  ) |> 
  glimpse()

#' 2 trips/logbooks on the day with 1 interview, ok

# count interviews and logbooks ----

#' overview
lgb_join_i1 |> 
  data_overview()

# TRIP_ID               1161
# id_code               1835

#' count days 
lgb_join_i1__short <-
  lgb_join_i1 |>
  select(TRIP_ID, id_code, trip_end_date_only) |>
  distinct()

dim(lgb_join_i1__short)
# 2062

lgb_join_i1__short |> 
  filter(!is.na(TRIP_ID)) |> 
    summarise(n_distinct(trip_end_date_only))
#' 187 days with at least one trip

#' count interviews/lgbks

total_interviews <- n_distinct(lgb_join_i1$id_code)
# 1835

# has at least one lgb logbooks
int_has_lgb <-
  lgb_join_i1 |>
  filter(!is.na(TRIP_ID))

int_w_lgb_cnt <-
  int_has_lgb |>
  summarise(n_distinct(id_code))
# 1006 interviews with a lgb

int_no_lgb_cnt <- total_interviews - int_w_lgb_cnt
# 829

int_no_lgb_cnt * 100 / total_interviews
#' 45% interviews have no logbooks

summarise(int_has_lgb, n_distinct(id_code)) * 100 /
  n_distinct(lgb_join_i1$id_code)
#' 55% interviews have logbooks

#' The same interview has and do not have a trip id? Yes, if more than 1 per day.

#' Check if combinations of trip/id_code are unique

lgb_join_i1_lgb_int_cnt <-
  lgb_join_i1 |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, trip_end_date_only, id_code) |>
  distinct() |>
  add_count(TRIP_ID, id_code, name = "trip_int_pair") |>
  arrange(desc(trip_int_pair))

lgb_join_i1_lgb_int_cnt |> 
  filter(!is.na(TRIP_ID)) |>
  filter(trip_int_pair > 1) |>
  nrow()
# 0
#' Yes, if there is a trip per interview, there is only 1


#' check by vessel

lgb_join_i1__vsl_lgb_int_cnt <-
  lgb_join_i1 |>
  select(TRIP_ID, VESSEL_OFFICIAL_NBR, trip_end_date_only, id_code) |>
  distinct() |>
  add_count(VESSEL_OFFICIAL_NBR, 
            TRIP_ID, id_code, name = "vsl_trip_int_pair") |>
  arrange(desc(vsl_trip_int_pair))

lgb_join_i1__vsl_lgb_int_cnt |>
  filter(!is.na(TRIP_ID)) |>
  filter(vsl_trip_int_pair > 1) |>
  nrow()
#' 0
#' Vessel/trip/interview combinations are unique 

lgb_join_i1__vsl_lgb_int_cnt__id_codes <-
  lgb_join_i1__vsl_lgb_int_cnt |>
  filter(!is.na(TRIP_ID)) |>
  select(id_code) |> 
  distinct()

nrow(lgb_join_i1__vsl_lgb_int_cnt__id_codes)

#' same id_code can have more than 1 lgb

#' 
#' 
lgb_join_i1__id_codes_w_lgb <- 
  lgb_join_i1 |>
  filter(!is.na(TRIP_ID)) |>
  select(id_code) |> 
  distinct()
dim(lgb_join_i1__id_codes_w_lgb)
# 1006

#' get id_codes with no lgb

lgb_join_i1__no_lgb_id_codes <- 
  lgb_join_i1 |>
  filter(is.na(TRIP_ID)) |>
  select(id_code) |> 
  distinct()

dim(lgb_join_i1__no_lgb_id_codes)
# 868 (should be 829)

intersect(lgb_join_i1__vsl_lgb_int_cnt__id_codes,
        lgb_join_i1__no_lgb_id_codes)
# 39

#' An example of false positive (fuzzy join by vessel id dist == 2)
lgb_join_i1 |> 
  filter(id_code == "1905120220529006") |> 
  glimpse()

# Multiple vessel ids for the same interview ----

lgb_join_i1__int_lgb <-
  lgb_join_i1 |>
  #' the same if grouped by `id_code` only
  group_by(id_code, survey_vessel_id) |>
  mutate(int_lgb = case_when(any(!is.na(TRIP_ID)) ~ "has_lgb",
                             .default = "no_lgb")) |>
  ungroup()

# lgb_join_i1__int_lgb |> 
  # filter(id_code == "1905120220529006") |> 
  # filter(int_lgb == "no_lgb") |> 
  # View()

lgb_join_i1__int_lgb__short <-
  lgb_join_i1__int_lgb |>
  select(
    VESSEL_OFFICIAL_NBR,
    trip_end_date_only,
    id_code,
    survey_vessel_id,
    st_pass,
    cnty_pass,
    name_pass,
    vsl_name_pass,
    int_lgb
  ) |> 
  distinct()


dim(lgb_join_i1__int_lgb__short)
# [1] 2294   9

