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

# asg_code is useless for us

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

# get dates from survey's id_code
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

# prepare i1 dates ----
survey_data_l_2022_vsl_date <-
  survey_data_l_2022$i1 |>
  dplyr::select(vsl_num, id_code, time, hrsf) |>
  dplyr::distinct() |>
  get_date_from_id_code_survey()

# check if correct split
survey_data_l_2022_vsl_date |>
  dplyr::mutate(date_paste = paste0(int_year, int_month, int_day)) |>
  dplyr::rowwise() |>
  # filter(!grepl(date_paste, id_code)) |>
  # 0
  dplyr::filter(grepl(date_paste, id_code)) |>
# 1835
  dplyr::ungroup() |>
  dim()

# check if correct interview_date
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

# str(survey_data_l_2022_vsl_date_time)

## hours fishing ----
survey_data_l_2022_vsl_date_time_all <-
  survey_data_l_2022_vsl_date_time |>
  dplyr::mutate(minutes_fishing = hrsf * 60) |>
  dplyr::mutate(start_time = interview_date_time - lubridate::minutes(minutes_fishing))

# $ interview_date_time: POSIXct[1:1835], format: "2022-01-30 15:53:00" "2022-02-14 17:27:00" ...
 # $ minutes_fishing    : num [1:1835] 300 540 480 120 240 240 150 180 420 450 ...
 # $ start_time         : POSIXct[1:1835], format: "2022-01-30 10:53:00" "2022-02-14 08:27:00" ...

dim(survey_data_l_2022_vsl_date)
# [1] 1835    10

n_distinct(survey_data_l_2022_vsl_date$vsl_num)
# vessels 476

n_distinct(db_logbooks_2022$VESSEL_OFFICIAL_NBR)
# 1892

survey_data_l_2022_vsl_date |> auxfunctions::print_df_names()

grep("date", names(db_logbooks_2022), ignore.case = T, value = T)

# View(db_logbooks_2022)

## add dates back to the full i1 ----

survey_data_l_2022_i1_w_dates <-
  dplyr::inner_join(survey_data_l_2022$i1, survey_data_l_2022_vsl_date)
# Joining with `by = join_by(id_code, time, hrsf, vsl_num)`

dim(survey_data_l_2022$i1)
# [1] 1835   33
dim(survey_data_l_2022_i1_w_dates)
# [1] 1835   37

# prepare geo data ----

# The following is adopted from Dominique's code
gulf_states <- c("AL", "TX", "MS", "LA")

## LIST OF GULF COUNTY NAMES IN THE SAME ORDER AS COUNTY CODES ABOVE
# escambia, santa rosa, okaloosa, walton, bay, gulf, franklin, wakulla,
# jefferson, taylor, dixie, levy, citrus, hernando, pasco, hillsborough,
# pinellas, manatee, sarasota, charlotte, lee, collier
# MONROE COUNTY = 87
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

# ASSIGN RECORDS IN COUNTY=75 WITH NO STATE TO GULF (FL OR LA)
# is.na(st) & cnty == c(75) ~ ,

# check st, cnty
# survey_data_l_2022_i1_w_dates |>
#   select(st, cnty) |> 
#   count(st, cnty) |> 
#   head()
  
survey_data_l_2022_i1_w_dates__states_by_cnty <-
  survey_data_l_2022_i1_w_dates |>
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

# survey_data_l_2022_i1_w_dates__states_by_cnty |>
#   select(states_l_by_cnty) |>
#   distinct() |>
#   glimpse()
  
# prepare logbooks ----
db_logbooks_2022_short0 <-
  db_logbooks_2022 |>
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

  # mutate(dplyr::across(tidyselect::all_of(time_col_names),
  #               ~ sprintf("%04d", .x)))

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

# compare trips/vessels
# tidyverse combine year, month and day into a date lubridate
#     str(db_logbooks_2022_short0)
# lubridate::date("2022-01-04 23:00:00")

lgb_join_i1 <-
  dplyr::right_join(
    db_logbooks_2022_short_date_time,
    survey_data_l_2022_vsl_date_time_all,
    dplyr::join_by(
      VESSEL_OFFICIAL_NBR == vsl_num,
      trip_end_date_only == interview_date
    ),
    relationship = "many-to-many"
  )
# ℹ Row 5799 of `x` matches multiple rows in `y`.
# ℹ Row 944 of `y` matches multiple rows in `x`.

dim(lgb_join_i1)
# 2015 23

n_distinct(lgb_join_i1$VESSEL_OFFICIAL_NBR)
# 476

# str(lgb_join_i1)

# interview and trip time difference ----
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

# has logbooks ----
lgb_join_i1__t_diff_short_has_trip <-
  lgb_join_i1__t_diff_short |>
  dplyr::filter(!is.na(TRIP_ID))

dim(lgb_join_i1__t_diff_short_has_trip)

lgb_join_i1__t_diff_short_has_no_trip <-
  lgb_join_i1__t_diff_short |>
  dplyr::filter(is.na(TRIP_ID))

dim(lgb_join_i1__t_diff_short_has_no_trip)

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
# They are a result of full join on a day, e.g. 2 trips, 2 interviews
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

## remove duplicates ----
lgb_join_i1__t_diff_short__w_int_all_dup_rm <-
  lgb_join_i1__t_diff_short__w_int_all_dup |>
  dplyr::anti_join(int_dups_only)
# Joining with `by = join_by(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR)`

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

# duplicated interview
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |> 
  dplyr::filter(dup_id_codes > 1) |> 
  dim()
  # 310

# check diff time
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::filter(dup_id_codes == 2) |>
  dplyr::arrange(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR, trip_end_date_time) |>
  dplyr::filter(big_diff_time == "no") |>
  dim()
# [1] 146  13

## get interview duplicates only ----

trip_dups_only <- 
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::filter(dup_id_codes > 1) |>
  dplyr::filter(big_diff_time == "yes") |>
  dplyr::select(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR) |> 
  dplyr::distinct()

dim(trip_dups_only)
# 164

## remove duplicates 2 trips. 1 interview ----
# Only keep logbooks with a correspondent interview
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm <-
  lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup |>
  dplyr::anti_join(trip_dups_only)
# Joining with `by = join_by(id_code, TRIP_ID, VESSEL_OFFICIAL_NBR)`

## check if not loosing trips by removing ----
nrow(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup) -
  nrow(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm) ==
  nrow(trip_dups_only)
# T

auxfunctions::data_overview(lgb_join_i1)
# TRIP_ID              1054
# VESSEL_OFFICIAL_NBR   476
# id_code              1835

auxfunctions::data_overview(lgb_join_i1__t_diff_short__w_int_all)
# id_code                    901
# TRIP_ID                   1053
# VESSEL_OFFICIAL_NBR        228

auxfunctions::data_overview(lgb_join_i1__t_diff_short__w_int_all_dup_rm)
# id_code                    896
# TRIP_ID                   1051
# VESSEL_OFFICIAL_NBR        228

auxfunctions::data_overview(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm)
# id_code                   886
# TRIP_ID                   887
# VESSEL_OFFICIAL_NBR       227

# View(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm)

## shorten the df ----
lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short <- lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm |>
  dplyr::select(-c(
    # tidyselect::ends_with("_diff"),
    # tidyselect::ends_with("_interval"),
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
  db_logbooks_2022 |>
  dplyr::select(tidyselect::all_of(lgb_names_to_use))

# dim(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_shorter)

## add logbooks to the df joined by day/time ----
catch_info_lgb <- 
  dplyr::left_join(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_shorter,
            db_logbooks_2022_short)
# Joining with `by = join_by(TRIP_ID, VESSEL_OFFICIAL_NBR)`

dim(catch_info_lgb)
# [1] 3502   24

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
# Joining with `by = join_by(id_code)`

dim(catch_info_lgb_i1)
# [1] 3502   37

catch_info_lgb_i1_i2 <- 
  left_join(catch_info_lgb_i1,
            survey_data_l_2022_short$i2,
            relationship = "many-to-many",
            suffix = c(".i1", ".releas"),
            join_by(id_code))
# ℹ Row 5 of `x` matches multiple rows in `y`.
# ℹ Row 1127 of `y` matches multiple rows in `x`.
# default
# Joining with `by = join_by(id_code, st, num_typ2)`
# Error in `left_join()`:
# ! Can't join `x$st` with `y$st` due to incompatible types.

dim(catch_info_lgb_i1_i2)
# [1] 8418   42

catch_info_lgb_i1_i2_i3 <-
  left_join(catch_info_lgb_i1_i2,
            survey_data_l_2022_short$i3,
            relationship = "many-to-many",
            join_by(id_code),
            suffix = c(".releas", ".harv")
)
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 1427 of `y` matches multiple rows in `x`.
# default
# Joining with `by = join_by(id_code, num_typ3, tsn)`

dim(catch_info_lgb_i1_i2_i3)
# [1] 89466    50

# join all survey info ----

lubridate::intersect(names(survey_data_l_2022_short$i1),
                     names(survey_data_l_2022_short$i2))

# unify classes
survey_data_l_2022_short <-
  survey_data_l_2022_short |>
  purrr::map(\(one_df) {
    one_df |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(c("st")), ~ as.integer(.x)))
  })

# joins
## i1 and i2 ----
survey_data_l_2022_short |> 
  purrr::map(~n_distinct(.x$id_code))

# TODO: left_join or full_join?
survey_i1_i2_released <-
  full_join(survey_data_l_2022_short$i1,
            survey_data_l_2022_short$i2,
            by = join_by(id_code, st),
            suffix = c(".i1", ".release"))
# Joining with `by = join_by(id_code, st, num_typ2)`

dim(survey_i1_i2_released)
# 3218  left join
# 3683  full join

n_distinct(survey_i1_i2_released$id_code)
# 1835

### add dates to i1_i2 ----
survey_i1_i2_released_dates <-
  get_date_from_id_code_survey(survey_i1_i2_released)

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

### add dates to i1_i3 ----
survey_i1_i3_harvested_dates <- 
  get_date_from_id_code_survey(survey_i1_i3_harvested)

# glimpse(survey_i1_i3_harvested_dates)

# result names:
data_names <-
  c("lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short",
    "db_logbooks_2022_short",
    "catch_info_lgb_i1_i2_i3",
    "survey_i1_i2_released",
    "survey_i1_i3_harvested")

auxfunctions::pretty_print(my_title = "Processed Data are in:", 
                           my_text = data_names)

