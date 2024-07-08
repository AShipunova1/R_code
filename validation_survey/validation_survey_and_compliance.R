library(zoo)
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

## shorten db_compliance_2022__comp_after_overr ----

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

# View(db_compliance_2022__comp_after_overr__short_m)

# check compliance for interviews w no logbooks ----
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
# [1] 1082    2

# get compliance information for vessels from survey no lgb ----
db_compliance_2022__comp_after_overr__short_m__interv <-
  db_compliance_2022__comp_after_overr__short_m |>
  filter(tolower(VESSEL_OFFICIAL_NBR) %in% tolower(lgb_join_i1__no_lgb__short$VESSEL_OFFICIAL_NBR))

dim(db_compliance_2022__comp_after_overr__short_m__interv)
# [1] 7600    6

n_distinct(db_compliance_2022__comp_after_overr__short_m__interv$VESSEL_OFFICIAL_NBR)
# 192

# get compl, no compl, or both per year ----
db_compliance_2022__comp_after_overr__short_m__interv__wide <-
  db_compliance_2022__comp_after_overr__short_m__interv |> 
  # remove weeks and month
  select(-starts_with("COMP_WEEK"), -year_month) |> 
  # unique compliance per vessel
  distinct() |> 
  # more columns, a column per vessel
  tidyr::pivot_wider(
    names_from = VESSEL_OFFICIAL_NBR,
    values_from = compliant_after_override,
    # make it "NO_YES" if both
    values_fn = ~ paste0(sort(.x), collapse = "_")
  ) |>
  dplyr::ungroup()

# glimpse(db_compliance_2022__comp_after_overr__short_m__interv__wide)

## turn back year compliance to the long format ----
db_compliance_2022__comp_after_overr__short_m__interv__wide_long <-
  db_compliance_2022__comp_after_overr__short_m__interv__wide |> 
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "vessel_official_number") |> 
  rename("is_compl_or_both_year" = V1)

# View(db_compliance_2022__comp_after_overr__short_m__interv__wide_long)
# check
db_compliance_2022__comp_after_overr__short_m__interv__wide_long |> 
    count(is_compl_or_both_year)
#  is_compl_or_both_year   n
#                     no   2
#                 no_yes   3
#                    yes 187

## get compliance per month ----
db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider <-
    db_compliance_2022__comp_after_overr__short_m__interv |>
    select(-starts_with("COMP_WEEK")) |> 
    tidyr::pivot_wider(
    names_from = VESSEL_OFFICIAL_NBR,
    values_from = compliant_after_override,
    # make it "NO_YES" if both
    values_fn = ~ paste0(unique(sort(.x)), collapse = "_")
  ) |>
  dplyr::ungroup()

# View(db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider)
# 
#   
#   # count grouped by other columns
#   dplyr::add_count(year_month, is_compl_or_both,
#             name = "compl_or_not_cnt") |>
#   unique() |>
#   dplyr::ungroup()
# 
# # View(db_compliance_2022__comp_after_overr__short_m__interv__wide_long_cnt)
# is_compl_or_both_month  n
#   <chr>            <int>
# 1 no                  57
# 2 yes               5550
# 3 NA                1725
