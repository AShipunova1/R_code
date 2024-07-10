library(zoo)
# compare list of vessels from survey with no lgb
# with
# fhier compliance monthly and annually

# Prepare compliance data ----
## compliance from db ----
### db: complaince after ovrridden ----
tictoc::tic("compl_overr")
db_compliance_2022__comp_after_overr <-
  db_compliance_2022 |>
  auxfunctions::add_compliant_after_override(overridden_col_name = "IS_COMP_OVERRIDE",
                                             compliance_col_name = "IS_COMP")
tictoc::toc()
# compl_overr: 28.73 sec elapsed
dim(db_compliance_2022__comp_after_overr)
# [1] 126105     22

### db: shorten db_compliance_2022__comp_after_overr ----

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

# VESSEL_OFFICIAL_NBR, COMP_WEEK, COMP_WEEK_START_DT, COMP_WEEK_END_DT, compliant_after_override

dim(db_compliance_2022__comp_after_overr__short)
# [1] 125845      5

### db: add a column for month  ----

db_compliance_2022__comp_after_overr__short_m <-
  db_compliance_2022__comp_after_overr__short |>
  dplyr::mutate(year_month = zoo::as.yearmon(COMP_WEEK_START_DT))

# View(db_compliance_2022__comp_after_overr__short_m)

# check compliance for interviews w no logbooks ----
dim(lgb_join_i1__int_lgb__short)
# [1] 2294    9

lgb_join_i1__no_lgb <- 
  lgb_join_i1__int_lgb__short |> 
  filter(int_lgb == "no_lgb")

dim(lgb_join_i1__no_lgb)
# [1] 1113    9

#' Vessels in survey list with no lgb

vessels_in_survey_no_lgb <- 
  unique(lgb_join_i1__no_lgb$VESSEL_OFFICIAL_NBR)

n_distinct(lgb_join_i1__no_lgb$VESSEL_OFFICIAL_NBR)
# 220

#' Should we use this instead?
#' If at least on of the filters is passed the PIMS vessel id is more likely to be the same as survey vessel id. Didn't use the State filter pass, too few states, the possibility of a random match is too high.

good_match_in_survey_no_lgb <-
  lgb_join_i1__no_lgb |>
  filter(!(is.na(cnty_pass) &
             is.na(name_pass) &
             is.na(vsl_name_pass)))

n_distinct(good_match_in_survey_no_lgb$VESSEL_OFFICIAL_NBR)
# 115

#' For now use all 220

# lgb_join_i1__no_lgb__short <-
#   lgb_join_i1__no_lgb |>
#   select(
#     -c(
#       TRIP_ID,
#       TRIP_START_DATE,
#       TRIP_START_TIME,
#       TRIP_END_DATE,
#       TRIP_END_TIME,
#       trip_start_hour,
#       trip_start_sec,
#       trip_end_hour,
#       trip_end_sec,
#       trip_start_date_time,
#       trip_end_date_time,
#       id_code,
#       vessel_name,
#       interviewee_f_name,
#       interviewee_l_name,
#       survey_vessel_id,
#       st_2,
#       cnty_3,
#       SERO_HOME_PORT_COUNTY,
#       SERO_HOME_PORT_STATE,
#       VESSEL_NAME,
#       FIRST_NAME,
#       LAST_NAME,
#       county_short,
#       state_code,
#       state_name,
#       county_code,
#       vessel_id_dist,
#       vsl_names_dissim,
#       st_pass,
#       cnty_pass,
#       name_pass,
#       vsl_name_pass
#     )
#   ) |>
#   distinct() |>
#   rename("interview_date" = trip_end_date_only) |>
#   filter(!is.na(VESSEL_OFFICIAL_NBR))
# 
# dim(lgb_join_i1__no_lgb__short)
# # [1] 1082    2

## get compliance information for vessels from survey no lgb ----
db_compliance_2022__comp_after_overr__short_m__interv <-
  db_compliance_2022__comp_after_overr__short_m |>
  filter(tolower(VESSEL_OFFICIAL_NBR) %in% tolower(vessels_in_survey_no_lgb))

dim(db_compliance_2022__comp_after_overr__short_m__interv)
# [1] 7305    6

vessels_in_survey_no_lgb__n__compl <- 
  unique(db_compliance_2022__comp_after_overr__short_m__interv$VESSEL_OFFICIAL_NBR)

glimpse(vessels_in_survey_no_lgb__n__compl)

n_distinct(db_compliance_2022__comp_after_overr__short_m__interv$VESSEL_OFFICIAL_NBR)
# 184

#' Why there are vessels in survey_no_lgb with no compliance info?
setdiff(vessels_in_survey_no_lgb,
        vessels_in_survey_no_lgb__n__compl)
# 86

## get compl, no compl, or both per year ----
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

dim(db_compliance_2022__comp_after_overr__short_m__interv__wide)
# 1 184

### turn back year compliance to the long format ----
db_compliance_2022__comp_after_overr__short_m__interv__wide_long <-
  db_compliance_2022__comp_after_overr__short_m__interv__wide |> 
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "vessel_official_number") |> 
  rename("is_compl_or_both_year" = V1)

dim(db_compliance_2022__comp_after_overr__short_m__interv__wide_long)
# [1] 184   2

# check
db_compliance_2022__comp_after_overr__short_m__interv__wide_long |> 
    count(is_compl_or_both_year)
#  is_compl_or_both_year   n
#                     no   2
#                 no_yes   3
#                    yes 179

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

dim(db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider)
# [1]  12 185

db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider |> 
  arrange(year_month) |> 
  glimpse()

### compliance per month in longer format ----
db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long <- 
  db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them
    cols = -year_month,
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

dim(db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long)
# [1] 2208    3

n_distinct(db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long$vessel_official_number)
# [1] 184, ok, as above

### count compliance per month ----
db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long_cnts <-
  db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long |>
  select(-vessel_official_number) |>
  dplyr::add_count(year_month, is_compl_or_both,
                   name = "compl_or_not_cnt_m") |> 
  unique()
  
db_compliance_2022__comp_after_overr__short_m__interv__compl_m_wider__long_cnts |> 
  filter(!is.na(is_compl_or_both)) |> 
  arrange(year_month, desc(is_compl_or_both)) |> 
  glimpse()

# Result: these vessels are mostly compliant.

# Repeat the compliance check for good_match_in_survey_no_lgb ----

## get compliance information for vessels in good_match_in_survey_no_lgb ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match <-
  db_compliance_2022__comp_after_overr__short_m |>
  filter(tolower(VESSEL_OFFICIAL_NBR) %in% tolower(good_match_in_survey_no_lgb$VESSEL_OFFICIAL_NBR))

dim(db_compliance_2022__comp_after_overr__short_m__interv__good_match)
# [1] 4314    6

vessels_in_survey_no_lgb__n__compl__good_match <- 
  unique(db_compliance_2022__comp_after_overr__short_m__interv__good_match$VESSEL_OFFICIAL_NBR)

glimpse(vessels_in_survey_no_lgb__n__compl__good_match)

n_distinct(db_compliance_2022__comp_after_overr__short_m__interv__good_match$VESSEL_OFFICIAL_NBR)
# 108


# get compl, no compl, or both per year ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide <-
  db_compliance_2022__comp_after_overr__short_m__interv__good_match |> 
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

dim(db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide)
# 1 108

## turn back year compliance to the long format ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide_long <-
  db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide |> 
  t() |> 
  as.data.frame() |> 
  tibble::rownames_to_column(var = "vessel_official_number") |> 
  rename("is_compl_or_both_year" = V1)

dim(db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide_long)
# [1] 108   2

# check
db_compliance_2022__comp_after_overr__short_m__interv__good_match__wide_long |> 
    count(is_compl_or_both_year)
#  is_compl_or_both_year   n
#                     no   2
#                    yes 106

# get compliance per month ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider <-
    db_compliance_2022__comp_after_overr__short_m__interv__good_match |>
    select(-starts_with("COMP_WEEK")) |> 
    tidyr::pivot_wider(
    names_from = VESSEL_OFFICIAL_NBR,
    values_from = compliant_after_override,
    # make it "NO_YES" if both
    values_fn = ~ paste0(unique(sort(.x)), collapse = "_")
  ) |>
  dplyr::ungroup()

dim(db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider)
# [1]  12 109

db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider |> 
  arrange(year_month) |> 
  glimpse()

## compliance per month in longer format ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long <- 
  db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider |> 
  tidyr::pivot_longer(
    # all other columns are vessel ids, use them
    cols = -year_month,
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

dim(db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long)
# [1] 1296    3

n_distinct(db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long$vessel_official_number)
# [1] 108, ok, as above

## count compliance per month ----
db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long_cnts <-
  db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long |>
  select(-vessel_official_number) |>
  dplyr::add_count(year_month, is_compl_or_both,
                   name = "compl_or_not_cnt_m") |> 
  unique()
  
db_compliance_2022__comp_after_overr__short_m__interv__good_match__compl_m_wider__long_cnts |> 
  filter(!is.na(is_compl_or_both)) |> 
  arrange(year_month, desc(is_compl_or_both)) |> 
  glimpse()

# Still mostly compliant

# Look at days in survey and compliance?