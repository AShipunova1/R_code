# Set up ----

# install.packages("devtools")
library(devtools)
devtools::install_github("AShipunova1/R_code/auxfunctions@development")
                         # , 
                         # force = T)
library(auxfunctions)
library(lubridate)

Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

my_paths <- auxfunctions::set_work_dir()

# get this project name
current_project_dir_name <- this.path::this.dir()

# find its base name
current_project_name <-
  basename(current_project_dir_name)

# use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
get_data_file_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_file_path)

source(get_data_file_path)

# Data are in:
# survey_data_l_2022
# processed_logbooks_2022
# db_logbooks_2022

survey_data_l_2022 |> 
  purrr::map(auxfunctions::print_df_names)

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
  dplyr::glimpse()
# 
# sst <- strsplit(text, "")[[1]]
# out <- paste0(sst[c(TRUE, FALSE)], sst[c(FALSE, TRUE)])

survey_data_l_2022$aga |>
  dplyr::select(year, month, day, asg_code) |>
  dplyr::distinct() |>
  dplyr::mutate(asg_sp = stringr::str_replace(
    asg_code,
    "(\\d+)2022(\\d\\d)(\\d\\d)",
    stringr::str_c("\\1 \\2 \\3")
  )) |>
  dplyr::mutate(asg_dates = stringr::str_split(asg_sp, " ")) |>
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

# prepare i1 ----
survey_data_l_2022_vsl_date <-
  survey_data_l_2022$i1 |>
  select(vsl_num, id_code, time, hrsf) |>
  distinct() |>
  mutate(
    id_sp = stringr::str_replace(
      id_code,
      "(\\d{5})(2022)(\\d{2})(\\d{2})(\\d{3})",
      stringr::str_c("\\1 \\2 \\3 \\4 \\5")
    )
  ) |>
  tidyr::separate_wider_delim(
    id_sp,
    delim = " ",
    names = c(
      "assignment_num_sampler_id",
      "int_year",
      "int_month",
      "int_day",
      "intercept_num"
    )
  ) |>
  select(-c(assignment_num_sampler_id, intercept_num)) |> 
  mutate(interview_date =
           lubridate::make_date(int_year, int_month, int_day))

survey_data_l_2022_vsl_date_time <-
  survey_data_l_2022_vsl_date |>
  mutate(hour_sec =
           stringr::str_replace(time, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(hour_sec,
                              delim = " ",
                              names = c("int_hour", "int_sec")) |>
  mutate(across(starts_with("int_"), ~ as.numeric(.x))) |>
  mutate(
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

# prepare logbooks ----
db_logbooks_2022_short <-
  db_logbooks_2022 |>
  select(
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    TRIP_START_DATE,
    TRIP_START_TIME,
    TRIP_END_DATE,
    TRIP_END_TIME
  ) |>
  distinct() |> 
  mutate(trip_end_date_only = lubridate::date(TRIP_END_DATE))

  # mutate(across(all_of(time_col_names),
  #               ~ sprintf("%04d", .x)))

db_logbooks_2022_short_date_time <-
  db_logbooks_2022_short |>
  mutate(start_hour_sec =
           stringr::str_replace(TRIP_START_TIME, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(
    start_hour_sec,
    delim = " ",
    names = c("trip_start_hour", "trip_start_sec")
  ) |>
  mutate(end_hour_sec =
           stringr::str_replace(TRIP_END_TIME, "(\\d+)(\\d{2})", "\\1 \\2")) |>
  tidyr::separate_wider_delim(end_hour_sec,
                              delim = " ",
                              names = c("trip_end_hour", "trip_end_sec")) |>
  mutate(across(
    c(
      "trip_start_hour",
      "trip_start_sec",
      "trip_end_hour",
      "trip_end_sec"
    ),
    ~ as.numeric(.x)
  )) |>
  mutate(
    trip_start_date_time = lubridate::make_datetime(
      lubridate::year(TRIP_START_DATE),
      lubridate::month(TRIP_START_DATE),
      lubridate::day(TRIP_START_DATE),
      as.numeric(trip_start_hour),
      as.numeric(trip_start_sec),
      tz = Sys.timezone()
    )
  ) |>
  mutate(
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
#     str(db_logbooks_2022_short)
# lubridate::date("2022-01-04 23:00:00")

lgb_join_i1 <-
  right_join(
    db_logbooks_2022_short_date_time,
    survey_data_l_2022_vsl_date_time_all,
    join_by(
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

# intervie and trip time difference ----
lgb_join_i1__t_diff <-
  lgb_join_i1 |>
  mutate(
    interview_trip_end_diff =
      trip_end_date_time - interview_date_time,
    interview_trip_start_diff =
      trip_start_date_time - start_time
  )

lgb_join_i1__t_diff_short <-
  lgb_join_i1__t_diff |>
  select(
    id_code,
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    trip_start_date_time,
    start_time,
    interview_trip_start_diff,
    trip_end_date_time,
    interview_date_time,
    interview_trip_end_diff
  )

# View(lgb_join_i1__t_diff_short)

# has logbooks ----
lgb_join_i1__t_diff_short_has_trip <-
  lgb_join_i1__t_diff_short |>
  filter(!is.na(TRIP_ID))

# n_distinct(lgb_join_i1__t_diff_short_has_trip$VESSEL_OFFICIAL_NBR)
# 228


# find duplicates ----

## duplicated vessel/trip_end ----
lgb_join_i1__t_diff_short %>%
  dplyr::group_by(VESSEL_OFFICIAL_NBR, lubridate::day(trip_end_date_time)) %>%
  filter(n() > 1) |> 
  View()


## duplicated id_code (2 trips - 1 interview) ----
lgb_join_i1__t_diff_short |> 
  dplyr::group_by(id_code) |> 
  dplyr::filter(n() > 1) |>
  dplyr::arrange(id_code, trip_end_date_time) |>
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
  mutate(big_diff_time = dplyr::case_when(
    !interview_date_time %within% trip_end_interval ~ "yes",
    .default = "no"
  )) |> 
  ungroup()

## duplicated trip_id (2 trips - 2 interview) ----

lgb_join_i1__t_diff_short %>%
  dplyr::group_by(TRIP_ID) %>%
  dplyr::filter(n() > 1) |>
  dplyr::arrange(TRIP_ID, interview_date_time, trip_end_date_time) |>
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
  mutate(big_diff_time = dplyr::case_when(
    !interview_date_time %within% trip_end_interval ~ "yes",
    .default = "no"
  )) |> 
  ungroup()
  
lgb_join_i1__t_diff_short__w_int |>
  arrange(VESSEL_OFFICIAL_NBR,
          id_code,
          TRIP_ID,
          trip_end_date_time,
          big_diff_time) |>
  filter(!is.na(TRIP_ID)) |>
  select(-c(contains("start"))) |>
  glimpse()

# a <- lubridate::ymd_hms("2022-06-01 07:09:00")
# a - lubridate::hours(1)

# lgb_join_i1__t_diff_short__w_int_all__dup <- 
lgb_join_i1__t_diff_short__w_int_all |>
  group_by(VESSEL_OFFICIAL_NBR, id_code) |>
  add_count(TRIP_ID, name = "dup_trip_ids") |>
  ungroup() |>
  filter(dup_trip_ids > 1) |>
  dim()
  # 0

lgb_join_i1__t_diff_short__w_int_all_dup <-
  lgb_join_i1__t_diff_short__w_int_all |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  add_count(TRIP_ID, name = "dup_interviews") |>
  ungroup()

lgb_join_i1__t_diff_short__w_int_all_dup |> 
  filter(dup_interviews > 1) |>
# 58
  filter(big_diff_time == "yes") |> 
  glimpse()
# 31

# check time difference ----
# lgb_join_i1__t_diff_short__w_int_all