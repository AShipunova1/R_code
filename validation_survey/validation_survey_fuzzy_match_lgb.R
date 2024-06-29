#' matched vsl ids
fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv |>
  glimpse()

# survey_vessel_id, matching_vessel_id, matching_vessel_id_regex, SERO_HOME_PORT_CITY, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, SERO_OFFICIAL_NUMBER, COAST_GUARD_NBR, EVENT_ID, HULL_ID_NBR, OWNER_ID, STATE_REG_NBR, V_STATUS, SUPPLIER_VESSEL_ID, UE, V_VESSEL_ID, VESSEL_NAME, EFFECTIVE_DATE, END_DATE, ENTITY_ID, EXPIRATION_DATE, NEW_OWNER, PERMIT, PERMIT_STATUS, PRIOR_OWNER, VESSEL_ALT_NUM, P_VESSEL_ID, FIRST_NAME, MIDDLE_NAME, LAST_NAME, NAME_SUFFIX, ADDRESS_1, ADDRESS_2, STATE, POSTAL_CODE, PHONE_NBR, EMAIL, LICENSE_NBR, PARTICIPANT_ID, PERMIT_ID, F_P_STATUS, permit_vessel_id, id_code, time, hrsf, year, wave, sub_reg, intsite, vessel_name, num_typ2, num_typ3, status, for_hire_permit, la_charter_license, prefix1, prefix2, la_charter_permit_number, operating_type, srhs_vessel, interviewee_f_name, interviewee_l_name, interviewee_m_name, interviewee_suffix, interviewee_role, fishing_distance, people_fishing, no_harvested_selected, permit_number1, permit_number2, vsl_num, cnty, date1, st, comments, int_year, int_month, int_day, interview_date, int_hour, int_sec, interview_date_time, minutes_fishing, start_time


# JOIN interview and logbooks by day and fuzzy-matched vessel ----

db_logbooks_2022_short_date_time__short <-
  db_logbooks_2022_short_date_time |>
  select(VESSEL_OFFICIAL_NBR, trip_end_date_only, TRIP_ID) |> 
  distinct()

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv |>
  select(SERO_OFFICIAL_NUMBER, interview_date, id_code) |>
  distinct()

lgb_join_i1_fuzzy_matched  <-
  dplyr::right_join(
    db_logbooks_2022_short_date_time__short,
    fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short,
    dplyr::join_by(
      VESSEL_OFFICIAL_NBR == SERO_OFFICIAL_NUMBER,
      trip_end_date_only == interview_date
    ),
    relationship = "many-to-many"
  )

n_distinct(lgb_join_i1_fuzzy_matched$VESSEL_OFFICIAL_NBR)
# 378

## many-to-many relationship ----
#' ℹ Row 1391 of `x` matches multiple rows in `y`.

db_logbooks_2022_short_date_time__short_1391 <-
  db_logbooks_2022_short_date_time__short[1391, ]

glimpse(db_logbooks_2022_short_date_time__short_1391)

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short |>
  filter(SERO_OFFICIAL_NUMBER == db_logbooks_2022_short_date_time__short_1391$VESSEL_OFFICIAL_NBR &
           interview_date == db_logbooks_2022_short_date_time__short_1391$trip_end_date_only)  |> 
  glimpse()
# 2 id_codes in a day

# ℹ Row 574 of `y` matches multiple rows in `x`.

fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short_574 <-
  fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short[574, ]

# glimpse(fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short_574)

db_logbooks_2022_short_date_time__short |>
  filter(VESSEL_OFFICIAL_NBR == fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short_574$SERO_OFFICIAL_NUMBER &
           trip_end_date_only == fuzzyjoin_vessel_ids__dist_grp__match_solo__join_back_surv__short_574$interview_date)  |> 
  glimpse()
# 2 trips in a day


lgb_join_i1__t_diff_short_has_trip <-
  lgb_join_i1__t_diff_short |>
  dplyr::filter(!is.na(TRIP_ID))

dim(lgb_join_i1__t_diff_short_has_trip)
# 1197    

# Don't have logbooks ----
lgb_join_i1_fuzzy_matched_has_no_trip <-
  lgb_join_i1_fuzzy_matched |>
  dplyr::filter(is.na(TRIP_ID))

dim(lgb_join_i1_fuzzy_matched)
# [1] 2309    4
dim(lgb_join_i1_fuzzy_matched_has_no_trip)
# 1322

