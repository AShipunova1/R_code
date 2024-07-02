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

## shorten survey data ----
survey_data_cols_to_keep <-
  c(
    "id_code",
    "vessel_name",
    "interviewee_f_name",
    "interviewee_l_name",
    "interviewee_m_name",
    "prefix1",
    "prefix2",
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
# [1] 1835   12

# unify county names ----
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
    "PERMIT",
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
# [1] 15854    12

## add fips codes to pims data ----

vessel_permit_owner_from_db_clean_vsl__cln_county__short__fips <-
  left_join(
    vessel_permit_owner_from_db_clean_vsl__cln_county__short,
    fips_code_to_use,
    join_by(SERO_HOME_PORT_STATE == state, county_short),
    relationship = "many-to-many"
  )

### check relationship = "many-to-many" ----
#' ℹ Row 13055 of `x` matches multiple rows in `y`.

vessel_permit_owner_from_db_clean_vsl__cln_county__short_13055 <-
  vessel_permit_owner_from_db_clean_vsl__cln_county__short[13055, ]

fips_code_to_use |>
  filter(
    county_short == vessel_permit_owner_from_db_clean_vsl__cln_county__short_13055$county_short &
      state == vessel_permit_owner_from_db_clean_vsl__cln_county__short_13055$SERO_HOME_PORT_STATE
  )
#      state state_code state_name county_code           county county_short
# 1196    md         24   maryland         005 baltimore county    baltimore
# 1217    md         24   maryland         510   baltimore city    baltimore
#' duplicates in FIPS county_short
#' Check manually?

#' ℹ Row 2360 of `y` matches multiple rows in `x`.

fips_code_to_use_2360 <-
  fips_code_to_use[2360, ]

vessel_permit_owner_from_db_clean_vsl__cln_county__short |>
  filter(
    county_short == fips_code_to_use_2360$county_short &
      SERO_HOME_PORT_STATE == fips_code_to_use_2360$state
  ) |> 
  glimpse()
#' Correct, many vessels have the same st & county

# Fuzzyjoin PIMS & survey by vessel_ids ----
fuzzyjoin_vessel_ids <-
  fuzzyjoin::stringdist_left_join(
    survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short,
    vessel_permit_owner_from_db_clean_vsl__cln_county__short__fips,
    by = c("survey_vessel_id" = "SERO_OFFICIAL_NUMBER"),
    distance_col = "vessel_id_dist"
  ) |> 
  distinct()

dim(fuzzyjoin_vessel_ids)
# [1] 32776    58

## keep each vessel in only one distance group ----

### change distance to words ----
#' for easier operations with column names 
#' 
fuzzyjoin_vessel_ids__dist_char <-
  fuzzyjoin_vessel_ids |>
  dplyr::select(survey_vessel_id, SERO_OFFICIAL_NUMBER, vessel_id_dist) |>
  dplyr::distinct() |>
  dplyr::mutate(vessel_id_dist = english::english(vessel_id_dist))

#' keep complete cases only 
fuzzyjoin_vessel_ids__dist_char__no_na <-
  fuzzyjoin_vessel_ids__dist_char |>
  dplyr::filter(stats::complete.cases(vessel_id_dist))

#' pivot wider to combine vessel ids by match distances
# Explanations:
# - `fuzzyjoin_vessel_ids__dist_char__no_na |>` starts the pipeline with the data frame `fuzzyjoin_vessel_ids__dist_char__no_na`.
# - `tidyr::pivot_wider(names_from = vessel_id_dist, values_from = permit_vessel_id, values_fn = list)` reshapes the data from long to wide format:
#   - `tidyr::pivot_wider()` is used to transform a data frame from long to wide format.
#   - `names_from = vessel_id_dist` specifies that the new column names in the wide format will come from the `vessel_id_dist` column.
#   - `values_from = permit_vessel_id` specifies that the values to fill the new columns will come from the `permit_vessel_id` column.
#   - `values_fn = list` specifies that if there are multiple values for a given `vessel_id_dist`, they should be combined into a list.
# 
# This code takes the `fuzzyjoin_vessel_ids__dist_char__no_na` data frame and pivots it to a wider format. In the wide format, new column names are derived from the unique values of `vessel_id_dist`, and the corresponding values from `permit_vessel_id` are placed in these new columns. If multiple `permit_vessel_id` values correspond to the same `vessel_id_dist`, they are combined into a list.

fuzzyjoin_vessel_ids__dist_grp <-
  fuzzyjoin_vessel_ids__dist_char__no_na |>
    tidyr::pivot_wider(names_from = vessel_id_dist,
                     values_from = SERO_OFFICIAL_NUMBER,
                     values_fn = list)

#' check
fuzzyjoin_vessel_ids__dist_grp |> 
  head() |> 
  glimpse()

fuzzyjoin_vessel_ids__dist_grp[2,] |> glimpse()

### keep a vessel in one distance group only ----
#' if there is a full match - no changes,
#' if no full match, but there is a match with a distance equal 1 - use it,
#' otherwise - all matches with the distacne equal 2
fuzzyjoin_vessel_ids__dist_grp__match <-
  fuzzyjoin_vessel_ids__dist_grp |>
  rowwise() |>
  mutate(
    grp0_len = length(zero),
    grp1_len = length(one),
    grp2_len = length(two)
  ) |>
  mutate(matching_vessel_ids =
           case_when(
             grp0_len > 0 ~ list(zero),
             (grp0_len == 0 & grp1_len > 0) ~ list(one),
             .default = list(two)
           )) |>
  ungroup()

#' check not perfect matches
fuzzyjoin_vessel_ids__dist_grp__match |>
  filter(grp0_len == 0 & !grp1_len == 0) |>
  select(survey_vessel_id, one, two, matching_vessel_ids) |> 
  head() |> 
  glimpse()

#' There is no more than one match in group 1 
fuzzyjoin_vessel_ids__dist_grp__match |>
  filter(grp0_len == 0) |>
  filter(grp1_len > 1) |>
  select(survey_vessel_id, one, two, matching_vessel_ids) |>
  nrow()
#' 0

#' check vessel ids with fuzzy match distance 2 
fuzzyjoin_vessel_ids__dist_grp__match_dist2 <-
  fuzzyjoin_vessel_ids__dist_grp__match |>
  dplyr::filter(grp0_len == 0 & grp1_len == 0) |>
  dplyr::select(survey_vessel_id, two, grp2_len) |>
  dplyr::distinct()

dplyr::n_distinct(fuzzyjoin_vessel_ids__dist_grp__match_dist2$survey_vessel_id)
# 62

#' write out the fuzy match result
#' fuzzyjoin_vessel_ids__dist_grp__match |>
#'   rowwise() |>
#'   mutate_if(is.list, ~ paste(unlist(.), collapse = ', ')) |>
#'   readr::write_csv(file.path(
#'     curr_proj_output_path,
#'     "fuzzyjoin_vessel_ids__dist_grp__match.csv"
#'   ))

# View(fuzzyjoin_vessel_ids__dist_grp__match)

## add all info back to the fuzzy match ----

#' keep the result only
fuzzyjoin_vessel_ids__dist_grp__match_solo <-
  fuzzyjoin_vessel_ids__dist_grp__match |>
  select(survey_vessel_id, matching_vessel_ids)

dim(fuzzyjoin_vessel_ids__dist_grp__match_solo)
# 354

n_distinct(fuzzyjoin_vessel_ids__dist_grp__match_solo)
# 354

### add back PIMS info to each fuzzy matching vessel id ----

fuzzyjoin_vessel_ids__dist_grp__match_solo__add_back_pims <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo |>
  rowwise() |>
  mutate(matching_vessel_id_regex =
           paste(matching_vessel_ids, collapse = "|")) |>
  fuzzyjoin::regex_left_join(
    vessel_permit_owner_from_db_clean_vsl__cln_county__short__fips,
    by = c("matching_vessel_id_regex" = "SERO_OFFICIAL_NUMBER")
  ) |>
  ungroup()

dim(fuzzyjoin_vessel_ids__dist_grp__match_solo__add_back_pims)
# [1] 2265   19
# [1] 20086    32

#' fuzzyjoin_vessel_ids__dist_grp__match_solo__add_back_pims
#' has PIMS info for each survey vessel id by fuzzy match,
#' one survey_vessel_id can have multiple matches with distance 2

### add back survey info ----
fuzzyjoin_vessel_ids__dist_grp__match_solo__add_back_pims_n_survey <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__add_back_pims |>
  inner_join(survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short,
             relationship = "many-to-many")
# Joining with `by = join_by(survey_vessel_id)`
#' relationship = "many-to-many": many id_codes (surveys) X many vessels in fuzzy match

# too many vessels for the same id_code ----
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
