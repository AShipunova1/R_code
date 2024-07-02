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
    join_by(SERO_HOME_PORT_STATE == state, county_short)
    # ,
    # relationship = "many-to-many"
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

# 1834+2380 = 4214

#' check
# View(fuzzyjoin_vessel_ids__closest__clean_vsl_name)

## combine 3 filters ----

fuzzyjoin_vessel_ids__closest__clean_vsl_name__3_filt <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name |>
  filter(!!geo_filter |
           !!name_filter |
           !!vsl_name_filter)

fuzzyjoin_vessel_ids__closest__clean_vsl_name__3_filt |>
  filter(vessel_id_dist == 2) |>
  select(-c(id_code, interview_date, vsl_num, cnty, st, SERO_HOME_PORT_CITY)) |>
  distinct() |>
  head() |>
  glimpse()

n_distinct(fuzzyjoin_vessel_ids__closest__clean_vsl_name__3_filt$SERO_OFFICIAL_NUMBER)
# 258

n_distinct(fuzzyjoin_vessel_ids$SERO_OFFICIAL_NUMBER)
# 832

# 258*100/832
# 31% vessels pass 1 of 3 filters

#' fuzzy join and check again
diff_vessel_names <-
  fuzzyjoin_vessel_ids__closest__clean_vsl_name |>
  filter(!(!!vsl_name_filter)) |>
  select(vessel_name, VESSEL_NAME) |>
  distinct()

glimpse(diff_vessel_names)

