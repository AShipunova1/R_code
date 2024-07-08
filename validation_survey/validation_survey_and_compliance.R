# compare list of vessels from survey with no lgb
# with
# fhier compliance monthly and annually

# Prepare compliance data ----
## complaince after ovrridden ----
tictoc::tic("compl_overr")
db_compliance_2022__comp_after_overr <-
  db_compliance_2022 |>
  auxfunctions::add_compliant_after_override(overridden_col_name = "IS_COMP_OVERRIDE",
                                             compliance_col_name = "IS_COMP")
tictoc::toc()
# compl_overr: 28.73 sec elapsed
dim(db_compliance_2022__comp_after_overr)
# [1] 126105     22

# shorten db_compliance_2022__comp_after_overr

db_compliance_2022__comp_after_overr__short <-
  db_compliance_2022__comp_after_overr |>
  select(
    -c(
      SRH_VESSEL_COMP_ID,
      SAFIS_VESSEL_ID,
      PERMIT_GROUP,
      PRM_GRP_EXP_DATE,
      COMP_YEAR,
      IS_CREATED_PERIOD,
      IS_COMP,
      IS_COMP_OVERRIDE,
      COMP_OVERRIDE_DT,
      COMP_OVERRIDE_USER_ID,
      SRFH_FOR_HIRE_TYPE_ID,
      CREATED_DT,
      CREATED_USER_ID,
      LU_DT,
      LU_USER_ID,
      COMP_OVERRIDE_CMT,
      IS_PMT_ON_HOLD
    )
  ) |>
  distinct()

dim(db_compliance_2022__comp_after_overr__short)
# [1] 125845      5

## add a column for month  ----

db_compliance_2022__comp_after_overr__short_m <-
  db_compliance_2022__comp_after_overr__short |>
  dplyr::mutate(year_month = zoo::as.yearmon(COMP_WEEK_START_DT))

dim(lgb_join_i1)
# [1] 4722   35

lgb_join_i1__no_lgb <-
  lgb_join_i1 |>
  filter(is.na(TRIP_ID))

dim(lgb_join_i1__no_lgb)
# [1] 2241   35
  
lgb_join_i1__no_lgb__short <- 
  lgb_join_i1__no_lgb |> 
  select(-c(TRIP_ID, TRIP_START_DATE, TRIP_START_TIME, TRIP_END_DATE, TRIP_END_TIME, trip_start_hour, trip_start_sec, trip_end_hour, trip_end_sec, trip_start_date_time, trip_end_date_time, id_code, vessel_name, interviewee_f_name, interviewee_l_name, survey_vessel_id, st_2, cnty_3, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, VESSEL_NAME, FIRST_NAME, LAST_NAME, county_short, state_code, state_name, county_code, vessel_id_dist, vsl_names_dissim, st_pass, cnty_pass, name_pass, vsl_name_pass)) |> 
  distinct() |> 
  rename("interview_date" = trip_end_date_only) |> 
  filter(!is.na(VESSEL_OFFICIAL_NBR))

dim(lgb_join_i1__no_lgb__short)
# [1] 1082    2# get compliance information for vessels from survey no lgb ----
db_compliance_2022__comp_after_overr__short_m__interv <-
  db_compliance_2022__comp_after_overr__short_m |>
  filter(VESSEL_OFFICIAL_NBR %in% lgb_join_i1__no_lgb__short$VESSEL_OFFICIAL_NBR)

View(db_compliance_2022__comp_after_overr__short_m__interv)
# 5607    
