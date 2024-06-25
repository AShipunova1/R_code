library(english)

# prepare data ----

# survey_data_l_2022 |> 
#   purrr::map(auxfunctions::print_df_names)

# $aga
# [1] "asg_num, intcd1, intcd2, state, interval, ano_int, anosite, site1, reason1, site2, reason2, start1, stop1, tsite1, start2, stop2, tsite2, start3, stop3, tsite3, start4, stop4, tsite4, year, month, day, wave, asg_code, sitehrs, int12_1, int12_2, int12, site1_comments, site2_comments, all_site_comments, control7, cluster_id, cnty, date1"
# 
# $i1
# [1] "id_code, time, hrsf, year, wave, sub_reg, intsite, vessel_name, num_typ2, num_typ3, status, for_hire_permit, la_charter_license, prefix1, prefix2, la_charter_permit_number, operating_type, srhs_vessel, interviewee_f_name, interviewee_l_name, interviewee_m_name, interviewee_suffix, interviewee_role, fishing_distance, people_fishing, no_harvested_selected, permit_number1, permit_number2, vsl_num, cnty, date1, st, comments"
# 
# $i2
# [1] "year, wave, sub_reg, id_code, tsn, num_fish, num_typ2, st, date1"
# 
# $i3
# [1] "year, wave, sub_reg, id_code, tsn, fshinsp, disp3, lngth, wgt, num_typ3, st, date1"
# 
# $ref
# [1] "id_code, time, hrsf, year, wave, st, sub_reg, intsite, vessel_name, num_typ2, num_typ3, status, comments, for_hire_permit, la_charter_license, prefix1, prefix2, la_charter_permit_number, operating_type, srhs_vessel, interviewee_f_name, interviewee_l_name, interviewee_m_name, interviewee_suffix, interviewee_role, fishing_distance, people_fishing, no_harvested_selected, permit_number1, permit_number2, vsl_num, cnty, date1"
# 

# aga asg code ----
survey_data_l_2022$aga |> 
  dplyr::select(year, month, day, asg_code) |> 
  dplyr::distinct() |> 
  head() |> 
  dplyr::glimpse()
# 
# sst <- strsplit(text, "")[[1]]
# out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])

survey_data_l_2022$aga |>
  dplyr::select(year, month, day, asg_code) |>
  dplyr::distinct() |>
  dplyr::mutate(asg_sps = stringr::str_replace(
    asg_code,
    "(\\d+)2022(\\d\\d)(\\d\\d)",
    stringr::str_c("\\1 \\2 \\3")
  )) |>
  dplyr::mutate(asg_dates = stringr::str_split(asg_sps, " ")) |>
  dplyr::rowwise() |>
  # filter(!asg_dates[[2]] == month) |>
  # 0
  dplyr::filter(!asg_dates[[3]] == day) |>
  # 0
  dplyr::glimpse()

#' asg_code is useless for us
#' 

# i1 ----
# View(survey_data_l_2022$ref)
# View(survey_data_l_2022$i1)

## ref and i1 id_code ----
setdiff(survey_data_l_2022$ref$id_code,
        survey_data_l_2022$i1$id_code) |> length()
# 19

setdiff(survey_data_l_2022$i1$id_code,
        survey_data_l_2022$ref$id_code) |> length()
# 1835

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
# 1835
  dplyr::ungroup() |>
  dim()
# 0

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

str(survey_data_l_2022_vsl_date_time)

## hours fishing ----
survey_data_l_2022_vsl_date_time_all <-
  survey_data_l_2022_vsl_date_time |>
  dplyr::mutate(minutes_fishing = hrsf * 60) |>
  dplyr::mutate(start_time = interview_date_time - lubridate::minutes(minutes_fishing))

# $ interview_date_time: POSIXct[1:1835], format: "2022-01-30 15:53:00" "2022-02-14 17:27:00" ...
 # $ minutes_fishing    : num [1:1835] 300 540 480 120 240 240 150 180 420 450 ...
 # $ start_time         : POSIXct[1:1835], format: "2022-01-30 10:53:00" "2022-02-14 08:27:00" ...

dim(survey_data_l_2022_vsl_date_time_all)
# [1] 1835    13

## add dates back to the full i1 ----

survey_data_l_2022_i1_w_dates <-
  dplyr::inner_join(survey_data_l_2022$i1,
                    survey_data_l_2022_vsl_date_time_all)
#' Joining with `by = join_by(id_code, time, hrsf, vsl_num)`

dim(survey_data_l_2022$i1)
# [1] 1835   33
dim(survey_data_l_2022_i1_w_dates)
# [1] 1835   42

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

## prepare geo data ----

#' The following is adopted from Dominique's code
gulf_states <- c("AL", "TX", "MS", "LA")

#' ## List of gulf county names in the same order as county codes above:
#' 
#' escambia, santa rosa, okaloosa, walton, bay, gulf, franklin, wakulla,
#' jefferson, taylor, dixie, levy, citrus, hernando, pasco, hillsborough,
#' pinellas, manatee, sarasota, charlotte, lee, collier
#' MONROE COUNTY = 87
florida_gulf_counties <- c(33,
                           113,
                           91,
                           131,
                           5,
                           45,
                           37,
                           129,
                           65,
                           123,
                           29,
                           75,
                           17,
                           53,
                           101,
                           57,
                           103,
                           81,
                           115,
                           15,
                           71,
                           21,
                           87)

#' ASSIGN RECORDS IN COUNTY=75 WITH NO STATE TO GULF (FL OR LA)
# is.na(st) & cnty == c(75) ~ ,

#' check st, cnty
# survey_data_l_2022_i1_w_dates |>
#   select(st, cnty) |> 
#   count(st, cnty) |> 
#   head()
  
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty <-
  survey_data_l_2022_i1_w_dates_clean_vsl |>
  dplyr::select(st, cnty) |>
  dplyr::distinct() |>
  dplyr::group_by(cnty) |>
  dplyr::mutate(st1 = 
                  dplyr::case_when(is.na(st) ~ "NA", .default = st)) |>
  dplyr::mutate(states_l_by_cnty = list(paste(unique(sort(
    st1
  ))))) |>
  dplyr::ungroup() |>
  dplyr::select(-st1) |>
  dplyr::distinct() |>
  dplyr::arrange(cnty)

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty |>
  select(states_l_by_cnty) |>
  distinct() |>
  glimpse()

## Restore state by PIMS county/state ----
### vessel_permit_owner_from_db tolower vessel ids ----
vessel_permit_owner_from_db_clean_vsl <-
  vessel_permit_owner_from_db |> 
  mutate(use_vessel_id = tolower(P_VESSEL_ID))
  
### change NA vsl_num from survey ----
survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num <- 
  survey_data_l_2022_i1_w_dates_clean_vsl |> 
  mutate(survey_vessel_id = tidyr::replace_na(vsl_num, ""))

### fuzzyjoin by vessel_ids ----
fuzzyjoin_vessel_ids <-
  fuzzyjoin::stringdist_left_join(
    survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num,
    vessel_permit_owner_from_db_clean_vsl,
    by = c("survey_vessel_id" = "use_vessel_id"),
    distance_col = "vessel_id_dist"
  )

### matched vsl ids -----
fuzzyjoin_vessel_ids_matched <-
  fuzzyjoin_vessel_ids |>
  filter(!is.na(use_vessel_id))
  
#' check 
fuzzyjoin_vessel_ids_matched |>
  select(survey_vessel_id, use_vessel_id, vessel_id_dist) |>
  distinct() |> 
  count(vessel_id_dist)
#   vessel_id_dist     n
#            <dbl> <int>
# 1              0   266
# 2              1    60
# 3              2   811

### if a vessel in more than in one distance group ----
#' change distance to words, to easier operations with column names
fuzzyjoin_vessel_ids__dist_grp <- 
  fuzzyjoin_vessel_ids_matched |>
  select(survey_vessel_id, use_vessel_id, vessel_id_dist) |>
  distinct() |> 
  dplyr::mutate(vessel_id_dist = english::english(vessel_id_dist)) |> 
  tidyr::pivot_wider(names_from = vessel_id_dist,
                     values_from = use_vessel_id,
                     values_fn = list)

fuzzyjoin_vessel_ids__dist_grp |> 
  head() |> 
  glimpse()

#' clean groups
#' 
# fuzzyjoin_vessel_ids__dist_grp[1,] |> glimpse()

### keep a vessel only in one group ----
fuzzyjoin_vessel_ids__dist_grp__match <-
  fuzzyjoin_vessel_ids__dist_grp |>
  rowwise() |>
  mutate(
    grp0_len = length(zero),
    grp1_len = length(one),
    grp2_len = length(two)
  ) |>
  mutate(matching_vessel_id =
           case_when(
             grp0_len > 0 ~ list(zero),
             (grp0_len == 0 & grp1_len > 0) ~ list(one),
             .default = list(two)
           )) |>
  ungroup()

#' check
fuzzyjoin_vessel_ids__dist_grp__match |>
  filter(grp0_len == 0 & !grp1_len == 0) |>
  select(survey_vessel_id, one, two, matching_vessel_id) |> 
  head() |> 
  glimpse()

### add back state and county ----
fuzzyjoin_vessel_ids__dist_grp__match_solo <- 
  fuzzyjoin_vessel_ids__dist_grp__match |> 
  select(survey_vessel_id, matching_vessel_id)

str(fuzzyjoin_vessel_ids__dist_grp__match_solo)
# 355

#### Add back vessel info from db ----
fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_db <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo |>
  # unnest_wider(matching_vessel_id, names_sep = "_") |>
  rowwise() |>
  mutate(matching_vessel_id_regex = 
           paste(matching_vessel_id, collapse = "|")) |>
  fuzzyjoin::regex_left_join(
    vessel_permit_owner_from_db_clean_vsl,
    by = c("matching_vessel_id_regex" = "use_vessel_id")
  ) |>
  ungroup()

# View(fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_db)

#' check 
fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_db |>
  select(
    survey_vessel_id,
    matching_vessel_id,
    matching_vessel_id_regex,
    use_vessel_id,
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE
    # SERO_OFFICIAL_NUMBER,
    # COAST_GUARD_NBR,
    # STATE_REG_NBR,
    # VESSEL_NAME,
    # VESSEL_ALT_NUM,
    # P_VESSEL_ID
  ) |>
  distinct() |> 
  head(20) |> 
  tail(10) |> 
  glimpse()

#### Add back vessel info from survey ----

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_db |>
  left_join(survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num,
            join_by(survey_vessel_id),
            relationship = "many-to-many")

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv |> 
  select(
  survey_vessel_id,
  matching_vessel_id,
  matching_vessel_id_regex,
  use_vessel_id,
  SERO_HOME_PORT_CITY,
  SERO_HOME_PORT_COUNTY,
  SERO_HOME_PORT_STATE,
  vsl_num,
  cnty,
  st
) |>
  distinct() |>
  head(20) |>
  # tail(10) |>
  glimpse()

### Restore state by vessel_id, cnty ----
#' For the same vessel same cnty, st is NA or the same
survey_data_l_2022_i1_w_dates_clean_vsl |> 
  filter(vsl_num == "fl9207st") |>
  select(cnty, st) |>
  distinct() |>
  glimpse()
# $ cnty <int> 17, 17
# $ st   <chr> NA, "12"

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v <- 
  survey_data_l_2022_i1_w_dates_clean_vsl |>
  dplyr::select(vsl_num, st, cnty) |>
  dplyr::distinct() |>
  dplyr::group_by(vsl_num, cnty) |>
  # dplyr::group_by(cnty) |>
  dplyr::mutate(st_with_char_na =
                  dplyr::case_when(is.na(st) ~ "NA", .default = st)) |>
  dplyr::mutate(states_l_by_cnty_v = list(paste(unique(
    sort(st_with_char_na)
  )))) |>
  dplyr::ungroup() |>
  dplyr::select(-st_with_char_na) |>
  dplyr::distinct() |>
  dplyr::arrange(vsl_num, cnty)

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v |>
  rowwise() |> 
  filter(length(states_l_by_cnty_v) > 2) |> 
  glimpse()
# 0
  
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored <- 
  survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v |>
  dplyr::rowwise() |>
  dplyr::mutate(temp_res =
                  case_when(is.na(st) ~
                              paste(unlist(states_l_by_cnty_v), collapse = ""), .default = st)) |>
  dplyr::mutate(restored_st =
                  stringr::str_extract(temp_res, "\\d+")) |>
  dplyr::select(-temp_res) |>
  dplyr::ungroup()

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored |> 
  # select(cnty, st, restored_st) |>
  distinct() |>
  head() |> 
  glimpse()

### format state and county codes ----
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

survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty_v__restored__fips |>
  head() |> 
  glimpse()

### convert cnty and state names to fips for restored from PIMS ----
get_fips <- function(my_df, field_name) {
  # browser()
  # state_both = "34"
  # cnty_3 = "315"
  res <-
    tidycensus::fips_codes |>
    dplyr::filter(state_code == state_both & county_code == cnty_3) |>
    dplyr::select(county) |>
    dplyr::mutate(county_short =
             stringr::str_replace_all(county, " County| Parish", "") |> 
             tolower())
  
  county_short <- res[["county_short"]]
  if (nrow(res) == 0) {
    county_short <- NA
  }
  
  return(county_short)
}
# get_county_name <- function(state_both, cnty_3) {
#   # browser()
#   # state_both = "34"
#   # cnty_3 = "315"
#   res <-
#     tidycensus::fips_codes |>
#     dplyr::filter(state_code == state_both & county_code == cnty_3) |>
#     dplyr::select(county) |>
#     dplyr::mutate(county_short =
#              stringr::str_replace_all(county, " County| Parish", "") |> 
#              tolower())
#   
#   county_short <- res[["county_short"]]
#   if (nrow(res) == 0) {
#     county_short <- NA
#   }
#   
#   return(county_short)
# }


fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips <- 
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv |> 
  format_state_and_county_codes("st")

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips |> 
  head() |> 
  glimpse()

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_short <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips |>
  select(
    survey_vessel_id,
    matching_vessel_id_regex,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    use_vessel_id,
    vsl_num,
    cnty,
    st,
    st_2,
    cnty_3,
    fips
  ) |> 
  distinct()

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_short |> 
  head(15) |> 
  tail() |> 
  glimpse()

### simplify county names ----
words_to_remove <- " county| parish| municipio| islands| island| municipality| district| city"

#### county names in my_df ----
#' check
grep(
  " ",
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_short$SERO_HOME_PORT_COUNTY,
  value = T
) |>
  unique() |>
  sort() |>
  print()

#' check in fips code
fips_codes_temp_1 <-
  tidycensus::fips_codes$county |>
  tolower() |> 
  stringr::str_replace_all(words_to_remove, "") |> 
  stringr::str_replace_all("st\\. ", "saint")

#' check
grep(" ", fips_codes_temp_1, value = T) |> 
  unique() |> 
  print()

grep(" ", fips_codes_temp_1, value = T) |> 
  unique() |> 
  stringr::str_extract(".*gulf.*") |> 
    unique()

unify_county_names <- function(my_df, county_col_name) {
  res_df <-
    my_df |>
    dplyr::mutate(dplyr::across(dplyr::everything(), tolower)) |> 
    dplyr::mutate(
      county_short =
        stringr::str_replace_all(!!dplyr::sym(county_col_name),
                                 words_to_remove, "") |>
        stringr::str_replace_all("st\\. ", "saint") |>
        stringr::str_squish()
    )
  
  return(res_df)  
}

#### unify_county_names in fips code ----
fips_code_to_use <-
  tidycensus::fips_codes |>
  unify_county_names("county")

glimpse(fips_code_to_use)

#' check 
fips_code_to_use |>
  select(county_short) |> 
  distinct() |> 
  head() |> 
  glimpse()

#### unify_county_names in my_df ----
fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_to_use <- 
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_short |>
  unify_county_names("SERO_HOME_PORT_COUNTY")

glimpse(fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_to_use)

### join state and county ----
vessel_ids_w_state_cnty_fips <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv_fips_to_use |>
  left_join(fips_code_to_use,
            join_by(SERO_HOME_PORT_STATE == state, county_short))

dim(vessel_ids_w_state_cnty_fips)
# 568

#' check
vessel_ids_w_state_cnty_fips |>
  filter(!cnty_3 == county_code &
           !st_2 == state_code) |>
  select(
    survey_vessel_id,
    use_vessel_id,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    cnty_3,
    st_2,
    county_short,
    county_code,
    state_code
  ) |>
  distinct() |>
  dim()
# 153

vessel_ids_w_state_cnty_fips__compare_counties_states <-
  vessel_ids_w_state_cnty_fips |>
  select(
    survey_vessel_id,
    use_vessel_id,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    cnty_3,
    st_2,
    county_short,
    county_code,
    state_code
  ) |>
  distinct()

vessel_ids_w_state_cnty_fips__compare_counties_states_rename <- 
  vessel_ids_w_state_cnty_fips__compare_counties_states |> 
  dplyr::rename(
 # = survey_vessel_id,
 # = use_vessel_id,
cnty_from_db = SERO_HOME_PORT_COUNTY,
state_from_db = SERO_HOME_PORT_STATE,
cnty_from_survey = cnty_3,
state_from_survey = st_2
# ,
 # = county_short,
 # = county_code,
 # = state_code,
  )

vessel_ids_w_state_cnty_fips__compare_counties_states_rename |> 
  head() |> 
  glimpse()

vessel_ids_w_state_cnty_fips__compare_counties_states_rename |>
  select(-contains("vessel")) |>
  distinct() |>
  head() |>
  glimpse()

vessel_ids_w_state_cnty_fips__compare_counties_states_rename |>
  select(-contains("vessel")) |>
  distinct() |>
  filter(!cnty_from_db == county_short) |> 
  glimpse()
# 15

### compare states restored by vessel and county with these restored from pims ----

# --- HERE ---


## add combined states back to i1 ----
survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty__all <-
  survey_data_l_2022_i1_w_dates_clean_vsl |>
  inner_join(survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty)
#' Joining with `by = join_by(cnty, st)`
#' 

# prepare logbooks ----
db_logbooks_2022_clean_vesl <-
  db_logbooks_2022 |>
  dplyr::mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR))

## shorten
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
  dplyr::mutate(trip_end_date_only = lubridate::date(TRIP_END_DATE))
  
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

str(db_logbooks_2022_short_date_time)

#' compare trips/vessels
#' tidyverse combine year, month and day into a date lubridate
#     str(db_logbooks_2022_short0)
# lubridate::date("2022-01-04 23:00:00")

# JOIN interview and logbooks by day and vessel ----
lgb_join_i1 <-
  dplyr::right_join(
    db_logbooks_2022_short_date_time,
    survey_data_l_2022_i1_w_dates_clean_vsl__states_by_cnty__all,
    dplyr::join_by(
      VESSEL_OFFICIAL_NBR == vsl_num,
      trip_end_date_only == interview_date
    ),
    relationship = "many-to-many"
  )
# ℹ Row 1391 of `x` matches multiple rows in `y`.
# ℹ Row 74 of `y` matches multiple rows in `x`.

lgb_join_i1 |> 
  head() |> 
  glimpse()

lgb_join_i1 |> 
  tail() |> 
  glimpse()

dim(lgb_join_i1)
# 2030 24

n_distinct(lgb_join_i1$VESSEL_OFFICIAL_NBR)
# 429

# Add interview and trip time difference ----
#' to align interviews with logbooks if there are more than one a day
#' 
lgb_join_i1__t_diff <-
  lgb_join_i1 |>
  dplyr::mutate(
    trip_end_interview_diff =
      trip_end_date_time - interview_date_time,
    trip_start_interview_diff =
      trip_start_date_time - start_time
  )

lgb_join_i1__t_diff_short <-
  lgb_join_i1__t_diff |>
  dplyr::select(
    id_code,
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    trip_start_date_time,
    start_time,
    trip_start_interview_diff,
    trip_end_date_time,
    interview_date_time,
    trip_end_interview_diff
  )

# View(lgb_join_i1__t_diff_short)

# Have logbooks ----
lgb_join_i1__t_diff_short_has_trip <-
  lgb_join_i1__t_diff_short |>
  dplyr::filter(!is.na(TRIP_ID))

dim(lgb_join_i1__t_diff_short_has_trip)
# 1197    

# Don't have logbooks ----
lgb_join_i1__t_diff_short_has_no_trip <-
  lgb_join_i1__t_diff_short |>
  dplyr::filter(is.na(TRIP_ID))

dim(lgb_join_i1__t_diff_short_has_no_trip)
# 833   

# n_distinct(lgb_join_i1__t_diff_short_has_trip$VESSEL_OFFICIAL_NBR)
# 228

# find duplicates ----

## duplicated vessel/trip_end ----
lgb_join_i1__t_diff_short %>%
  dplyr::group_by(VESSEL_OFFICIAL_NBR, 
                  lubridate::day(trip_end_date_time)) %>%
  dplyr::filter(n() > 1) |> 
  head() |> 
  dplyr::glimpse()

## duplicated id_code (2 trips - 1 interview) ----
lgb_join_i1__t_diff_short |> 
  dplyr::group_by(id_code) |> 
  dplyr::filter(n() > 1) |>
  dplyr::arrange(id_code, trip_end_date_time) |>
  head() |> 
  dplyr::glimpse()

## interval and big_diff_time ----
lgb_join_i1__t_diff_short__w_int_all <-
  lgb_join_i1__t_diff_short_has_trip |>
  dplyr::group_by(TRIP_ID) |>
  dplyr::mutate(
    trip_end_interval =
      lubridate::interval(
        start =
          trip_end_date_time - lubridate::minutes(30),
        end = trip_end_date_time + lubridate::minutes(90),
        tz = Sys.timezone()
      )
  ) |> 
  dplyr::mutate(big_diff_time = dplyr::case_when(
    !interview_date_time %within% trip_end_interval ~ "yes",
    .default = "no"
  )) |> 
  dplyr::ungroup()

## duplicated trip_id (2 trips - 2 interview) ----

lgb_join_i1__t_diff_short %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::filter(n() > 1) |>
  dplyr::arrange(TRIP_ID, interview_date_time, trip_end_date_time) |>
  head() |> 
  dplyr::glimpse()

lgb_join_i1__t_diff_short__w_int <-
  lgb_join_i1__t_diff_short |>
  dplyr::group_by(TRIP_ID) |>
  dplyr::filter(n() > 1) |>
  dplyr::mutate(
    trip_end_interval =
      lubridate::interval(
        start =
          trip_end_date_time - lubridate::minutes(30),
        end = trip_end_date_time + lubridate::minutes(90),
        tz = Sys.timezone()
      )
  ) |> 
  dplyr::mutate(big_diff_time = dplyr::case_when(
    !interview_date_time %within% trip_end_interval ~ "yes",
    .default = "no"
  )) |> 
  dplyr::ungroup()
  
lgb_join_i1__t_diff_short__w_int |>
  dplyr::arrange(VESSEL_OFFICIAL_NBR,
          id_code,
          TRIP_ID,
          trip_end_date_time,
          big_diff_time) |>
  dplyr::filter(!is.na(TRIP_ID)) |>
  dplyr::select(-c(contains("start"))) |>
  head() |> 
  dplyr::glimpse()

# a <- lubridate::ymd_hms("2022-06-01 07:09:00")
# a - lubridate::hours(1)

lgb_join_i1__t_diff_short__w_int_all_dup <-
  lgb_join_i1__t_diff_short__w_int_all |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::add_count(TRIP_ID, name = "dup_interviews") |>
  dplyr::ungroup()

dup_interviews <- 
  lgb_join_i1__t_diff_short__w_int_all_dup |> 
  dplyr::filter(dup_interviews > 1)

nrow(dup_interviews)
# 58

dup_interviews |> 
  dplyr::filter(big_diff_time == "yes") |> 
  head() |> 
  dplyr::glimpse()
# 31

## remove duplicated trip/interview ----
#' They are a result of full join on a day, e.g. 2 trips, 2 interviews
lgb_join_i1__t_diff_short__w_int_all_dup |>
  dplyr::filter(dup_interviews > 1) |>
  dplyr::select(-tidyselect::ends_with("_diff")) |>
  head() |> 
  dplyr::glimpse()

int_dups_only <- 
  lgb_join_i1__t_diff_short__w_int_all_dup |>
  dplyr::filter(dup_interviews > 1) |>
  dplyr::filter(big_diff_time == "yes") |>
  dplyr::select(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR) |> 
  dplyr::distinct()

dim(int_dups_only)
# 31

### remove duplicates ----
lgb_join_i1__t_diff_short__w_int_all_dup_rm <-
  lgb_join_i1__t_diff_short__w_int_all_dup |>
  dplyr::anti_join(int_dups_only)
#' Joining with `by = join_by(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR)`

nrow(lgb_join_i1__t_diff_short__w_int_all_dup) -
  nrow(lgb_join_i1__t_diff_short__w_int_all_dup_rm) ==
  nrow(int_dups_only)
# T
  
# check time difference ----
lgb_join_i1__t_diff_short__w_int_all_dup |>
  dplyr::filter(dup_interviews == 1) |>
  dplyr::filter(big_diff_time == "yes") |>
  # select(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR)
  dplyr::select(
    VESSEL_OFFICIAL_NBR,
    trip_end_date_time,
    interview_date_time,
    trip_end_interview_diff,
    trip_end_interval
  ) |>
  dplyr::arrange(
    VESSEL_OFFICIAL_NBR,
    trip_end_date_time,
    interview_date_time,
    trip_end_interview_diff,
    trip_end_interval
  ) |>
  head() |> 
  dplyr::glimpse()

# n_distinct(lgb_join_i1__t_diff_short__w_int_all_dup$TRIP_ID) ==
# nrow(lgb_join_i1__t_diff_short__w_int_all_dup)
# # F
# 
# lgb_join_i1__t_diff_short__w_int_all_dup |> 
#   group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
#   mutate(dups = c(unique(dup_interviews))) |> 
#   ungroup() |> 
#   # filter(!dups == 1) |>
#   # filter(!dups == 2) |> 
#   select(dups) |> 
#   distinct() |> 
# str()
#   View()

# remove duplicated interview counts (2 trips a day, 1 interview) ----
## Do that for the df with no 2 by 2 duplicates ----
### find interview duplicates ----
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup <-
 lgb_join_i1__t_diff_short__w_int_all_dup_rm |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, id_code) |>
  dplyr::add_count(id_code, name = "dup_id_codes") |>
  dplyr::ungroup()

#' duplicated interviews
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |> 
  dplyr::filter(dup_id_codes > 1) |> 
  dim()
  # 310

#' check diff time
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::filter(dup_id_codes == 2) |>
  dplyr::arrange(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR, trip_end_date_time) |>
  dplyr::filter(big_diff_time == "no") |>
  dim()
# [1] 147  13

## get interview duplicates only ----

trip_dups_only <- 
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::filter(dup_id_codes > 1) |>
  dplyr::filter(big_diff_time == "yes") |>
  dplyr::select(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR) |> 
  dplyr::distinct()

dim(trip_dups_only)
# 177

## remove duplicates 2 trips. 1 interview ----
#' Only keep logbooks with a correspondent interview
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm <-
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::anti_join(trip_dups_only)
#' Joining with `by = join_by(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR)`
#' 

## check if not loosing trips by removing ----
nrow(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup) -
  nrow(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm) ==
  nrow(trip_dups_only)
# T

fields_to_cnt <- c("TRIP_ID", "VESSEL_OFFICIAL_NBR", "id_code")

lgb_join_i1 |>
  select(all_of(fields_to_cnt)) |>
  auxfunctions::data_overview()
# TRIP_ID             1167
# VESSEL_OFFICIAL_NBR  429
# id_code              1835

lgb_join_i1__t_diff_short__w_int_all |>
  select(all_of(fields_to_cnt)) |>
  auxfunctions::data_overview()
# TRIP_ID             1166
# VESSEL_OFFICIAL_NBR  250
# id_code             1002

lgb_join_i1__t_diff_short__w_int_all_dup_rm |>
  select(all_of(fields_to_cnt)) |>
  auxfunctions::data_overview()
# TRIP_ID             1164
# VESSEL_OFFICIAL_NBR  250
# id_code              997

lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm |>
  select(all_of(fields_to_cnt)) |>
  auxfunctions::data_overview()
# TRIP_ID             987
# VESSEL_OFFICIAL_NBR 249
# id_code             986

# View(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm)

## shorten the df ----
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short <- lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm |>
  dplyr::select(-c(
    tidyselect::contains("start"),
    tidyselect::starts_with("dup_")
  )) |> 
  dplyr::arrange(VESSEL_OFFICIAL_NBR, trip_end_date_time)

lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |>
  head() |> 
  dplyr::glimpse()

# TODO: why id_code amount > TRIP_ID?

# TODO: check interview before trip end, joined a wrong trip?

# combine all catch info ----

lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_shorter <- 
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |>
  dplyr::select(id_code,
         TRIP_ID,
         VESSEL_OFFICIAL_NBR,
         trip_end_date_time,
         interview_date_time)

## shorten db logbooks ----

lgb_names_to_use <- c(
  "TRIP_ID",
"TRIP_TYPE_NAME",
"VESSEL_OFFICIAL_NBR",
"VESSEL_NAME",
"CAPT_NAME_FIRST",
"CAPT_NAME_LAST",
"STATE",
"STATE_NAME",
"END_PORT_NAME",
"END_PORT_COUNTY",
"END_PORT_STATE",
"NUM_ANGLERS",
"ACTIVITY_TYPE_NAME",
"DISTANCE_CODE_NAME",
"FISHING_HOURS",
"CATCH_SEQ",
"CATCH_SPECIES_ITIS",
"REPORTED_QUANTITY",
"ANYTHING_CAUGHT_FLAG",
"DISPOSITION_CODE",
"DISPOSITION_NAME"
)
# "UNIT_MEASURE",

db_logbooks_2022_short <-
  db_logbooks_2022_clean_vesl |>
  dplyr::select(tidyselect::all_of(lgb_names_to_use))

dim(db_logbooks_2022_short)

## add logbooks to the df joined by day/time ----
catch_info_lgb <- 
  dplyr::left_join(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_shorter,
            db_logbooks_2022_short)
#' Joining with `by = join_by(TRIP_ID, VESSEL_OFFICIAL_NBR)`

dim(catch_info_lgb)
# [1] 3799   24

## shorten survey_data_l_2022 ----

survey_fields_to_use <- 
    c(
  "id_code",
  "operating_type",
  "vsl_num",
  "vessel_name",
  "interviewee_f_name",
  "interviewee_l_name",
  "st",
  "cnty",
  "people_fishing",
  "no_harvested_selected",
  "fishing_distance",
  "hrsf",
  "tsn",
  "num_fish",
  "fshinsp",
  "num_typ2",
  "disp3",
  "lngth",
  "wgt",
  "num_typ3",
  "i2",
  "i3"
  )

survey_data_l_2022_short <- 
  survey_data_l_2022 |> 
  purrr::map(\(x) {x |> 
      dplyr::select(tidyselect::any_of(survey_fields_to_use))})

survey_data_l_2022 |> purrr::map(dim)
survey_data_l_2022_short |> purrr::map(dim)

survey_data_l_2022_short$i2 <-
  survey_data_l_2022_short$i2 |>
  tibble::add_column(i2 = "released")

survey_data_l_2022_short$i3 <-
  survey_data_l_2022_short$i3 |>
  tibble::add_column(i3 = "harvested")

survey_data_l_2022_short$i3 |> glimpse()

# survey_data_l_2022_short |> purrr::map(dim)

catch_info_lgb_i1 <-
  left_join(catch_info_lgb,
            survey_data_l_2022_short$i1,
            suffix = c(".lgb", ".i1"))
#' Joining with `by = join_by(id_code)`

dim(catch_info_lgb_i1)
# [1] 3502   37

catch_info_lgb_i1_i2 <- 
  left_join(catch_info_lgb_i1,
            survey_data_l_2022_short$i2,
            relationship = "many-to-many",
            suffix = c(".i1", ".releas"),
            join_by(id_code))
#' ℹ Row 5 of `x` matches multiple rows in `y`.
#' 
#' ℹ Row 1127 of `y` matches multiple rows in `x`.
#' 
#' default
#' 
#' Joining with `by = join_by(id_code, st, num_typ2)`
#' 
#' Error in `left_join()`:
#' 
#' ! Can't join `x$st` with `y$st` due to incompatible types.

dim(catch_info_lgb_i1_i2)
# [1] 9172   42

catch_info_lgb_i1_i2_i3 <-
  left_join(catch_info_lgb_i1_i2,
            survey_data_l_2022_short$i3,
            relationship = "many-to-many",
            join_by(id_code),
            suffix = c(".releas", ".harv")
)
#' ℹ Row 1 of `x` matches multiple rows in `y`.
#' 
#' ℹ Row 1427 of `y` matches multiple rows in `x`.
#' 
#' default
#' 
#' Joining with `by = join_by(id_code, num_typ3, tsn)`

dim(catch_info_lgb_i1_i2_i3)

# join all survey info ----

lubridate::intersect(names(survey_data_l_2022_short$i1),
                     names(survey_data_l_2022_short$i2))

#' unify classes
survey_data_l_2022_short <-
  survey_data_l_2022_short |>
  purrr::map(\(one_df) {
    one_df |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(c("st")), ~ as.integer(.x)))
  })

#' joins
#' 

## i1 and i2 ----
survey_data_l_2022_short |> 
  purrr::map(~n_distinct(.x$id_code))

# TODO: left_join or full_join?
survey_i1_i2_released <-
  full_join(survey_data_l_2022_short$i1,
            survey_data_l_2022_short$i2,
            by = join_by(id_code, st),
            suffix = c(".i1", ".release"))
#' Joining with `by = join_by(id_code, st, num_typ2)`

dim(survey_i1_i2_released)
# 3218  left join
# 3683  full join

# TODO: check how it is joining if "st" is NA

n_distinct(survey_i1_i2_released$id_code)
# 1835

### add dates to i1_i2 and clean vsl ----
survey_i1_i2_released_dates <-
  get_date_from_id_code_survey(survey_i1_i2_released) |> 
  clean_up_survey_vessel_ids()

glimpse(survey_i1_i2_released_dates)

## i1 and i3 ----
survey_i1_i3_harvested <-
  full_join(survey_data_l_2022_short$i1,
            survey_data_l_2022_short$i3,
            by = join_by(id_code, st),
            suffix = c(".i1", ".harv")
)

n_distinct(survey_i1_i3_harvested$id_code)
# 1835

dim(survey_i1_i3_harvested)
# [1] 11794    21
# View(survey_i1_i3_harvested)

### add dates to i1_i3 and clean vessel ids ----
survey_i1_i3_harvested_dates <- 
  get_date_from_id_code_survey(survey_i1_i3_harvested) |> 
  clean_up_survey_vessel_ids()

# glimpse(survey_i1_i3_harvested_dates)

# Prepared data result names ----
data_names <-
  c("lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short",
    "db_logbooks_2022_short",
    "catch_info_lgb_i1_i2_i3",
    "survey_i1_i2_released",
    "survey_i1_i3_harvested")

auxfunctions::pretty_print(my_title = "Processed Data are in:", 
                           my_text = data_names)

