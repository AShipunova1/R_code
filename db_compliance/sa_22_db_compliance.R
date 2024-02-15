# run from db_compliance.R
# SA 2022 compliance ----
# # There should be at least one logbook or one DNFs filed for any given week except the last one (can submit the following Tuesday).
# # DNFs should not be submitted more than 30 days in advance
#
# # all weeks of 2022 * all vessels
# # SA: each can have:
# # 1) a permit
# # 2) a trip
# # 3) a negative report
# # 1 only
# # 1,2
# # 1,3
# # 2 only
# # 3 only
# # 2,3?

# SA compliance by year ----
## get sa only vsls ----
v_p__t__tne_d_weeks_sa <-
  v_p__t__tne_d_weeks |>
  dplyr::filter(permit_sa_gom_dual == "sa_only")
dim(v_p__t__tne_d_weeks_sa)
# [1] 90766    15
# [1] 194697     92
# [1] 194051     93 excl. srhs

## reports_exists dplyr::filter ----
reports_exists_filter <- rlang::quo(
  !(is.na(rep_type.t) & is.na(rep_type.tne))
)

## mark weekly compliance ----
tic("v_p__t__tne_d_weeks_sa_compl")
v_p__t__tne_d_weeks_sa_compl_w <-
  v_p__t__tne_d_weeks_sa |>
  dplyr::group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m,
           YEAR) |>
  # not compliant if both reports (trips and t negative) are absent
  dplyr::mutate(sa_compl_week = dplyr::case_when(!!reports_exists_filter ~
                                "yes",
                              .default = "no")) |>
  dplyr::ungroup()
toc()
# v_p__t__tne_d_weeks_sa_compl: 28.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl: 22.39 sec elapsed
# v_p__t__tne_d_weeks_sa_compl_w: 33.11 sec elapsed

dim(v_p__t__tne_d_weeks_sa_compl_w)
# [1] 90766    16
# [1] 194697     93
# [1] 194051     94 excl. SRHS

## count compl weeks ----
# Do not group by year, the last week of 2021 should be counted together with 2022

v_p__t__tne_d_weeks_sa_compl_cnt_w <-
  v_p__t__tne_d_weeks_sa_compl_w |>
  dplyr::group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID) |>
  dplyr::mutate(compl_w_cnt = n_distinct(WEEK_OF_YEAR)) |>
  dplyr::ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w)
# [1] 90766    16
# [1] 194697     94
# excl. SRHS
# [1] 194051     95

### check compl week count ----
v_p__t__tne_d_weeks_sa_compl_cnt_w |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL4430NN") |>
  dplyr::select(WEEK_OF_YEAR, date_y_m, all_of(starts_with("rep_type")), compl_w_cnt) |>
    dplyr::distinct() |>
    dim()
# 14 distinct weeks
# 17 rows bc some weeks are in 2 month, e.g. 48 in Nov 2022 and Dec 2022

# fewer columns ----
rm_columns <-
  c(
    "SUPPLIER_TRIP_ID",
    "TRIP_NBR",
    "SPLIT_TRIP",
    "FORM_VERSION",
    "DAYS_AT_SEA",
    "NBR_OF_CREW",
    "DE.t",
    "DC.t",
    "UC.t",
    "CF_PERMIT_ID.t",
    "PORT",
    "STATE",
    "TRIP_END_TIME",
    "PARTNER_VTR",
    "SUBMITTED_BY_PARTICIPANT",
    "APPROVED_BY",
    "APPROVAL_DATE",
    "TRIP_START_TIME",
    "NUM_ANGLERS",
    "DEA_PERMIT_ID",
    "SUBMIT_METHOD.t",
    "TICKET_TYPE",
    "EVENT_ID.t",
    "SUB_TRIP_TYPE",
    "REPORTING_SOURCE",
    "TRANSMISSION_DATE",
    "APP_VERSION",
    "CONFIRMATION_SIGNATURE",
    "END_PORT",
    "START_PORT",
    "CAPT_NAME_LAST",
    "CAPT_NAME_FIRST",
    "OWNER_ABOARD",
    "NBR_PAYING_PASSENGERS",
    "TRIP_FEE",
    "FUEL_GAS_GALLONS",
    "FUEL_GAS_GALLON_PRICE",
    "FUEL_DIESEL_GALLONS",
    "FUEL_DIESEL_GALLON_PRICE",
    "COST_BAIT",
    "COST_ICE",
    "COST_FOOD",
    "COST_LIGHT",
    "COST_MISC",
    "COST_IFQ",
    "REVENUE_TOTAL",
    "PAY_TO_CAPT_CREW",
    "PAY_PERCENT_TO_CAPT",
    "PAY_PERCENT_TO_CREW",
    "ADDDITIONAL_FISHERMEN",
    "FUEL_GALLONS",
    "FUEL_GALLON_PRICE",
    "CF_ISS_AGENCY",
    "VENDOR_APP_NAME",
    "VENDOR_PLATFORM",
    "VALIDATING_AGENCY",
    "CONFIRMED_VALIDATING_AGENCY",
    "VTR_NUMBER",
    "SEA_TIME",
    "BAIT_WEIGHT",
    "ICE_MAKER",
    "PAY_PERCENT_TO_OWNER",
    "TRIP_END_week_num",
    "TRIP_END_y",
    "TRIP_END_m",
    "CF_PERMIT_ID.tne",
    "CF_ID",
    "UC.tne",
    "DC.tne",
    "DE.tne",
    "EVENT_ID.tne",
    "STATUS",
    "SUBMIT_METHOD.tne"
  )

v_p__t__tne_d_weeks_sa_compl_cnt_w_short <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w |> 
  dplyr::select(-any_of(rm_columns)) |> 
  dplyr::distinct()
dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_short)
# [1] 194697     23
# [1] 189214     23 distinct
# today()
# [1] "2023-08-22"
# [1] 114478     22
# excl. SRHS
# [1] 113909     22

## compliance per year ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22 <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_short |>
  dplyr::mutate(compl_2022 =
           dplyr::case_when(
    !!reports_exists_filter &
      compl_w_cnt >= permit_weeks_amnt_22 ~ "yes",
           .default = "no")
  ) |>
  dplyr::ungroup()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22)
# [1] 90766    17
# [1] 194697     95
# [1] 189214     24
# [1] 114478     23
# [1] 113909     23 (excl. SRHS)

v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22 |> 
  dplyr::count(ACTIVITY_TYPE)
# 1             0  64007
# 2             2      7
# 3             8     22
# 4            80    229
# 5            NA 130432
# distinct fewer cols
# 1             0  59945
# 2             2      6
# 3             8     22
# 4            80    215
# 5            NA 129026
# [1] "2023-08-22"
# 1             0 44594
# 2             2     2
# 3             8    15
# 4            80   115
# 5            NA 69752
# excl srhs
# 1             0 44269
# 2             2     2
# 3             8    15
# 4            80   114
# 5            NA 69509

# v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22 |>
#   # dplyr::select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, all_of(starts_with("UE"))) |>
#   dplyr::select(PERMIT_VESSEL_ID, ACTIVITY_TYPE, UE.t) |>
#   dplyr::distinct() |>
#   dplyr::filter(ACTIVITY_TYPE %in% c("2", "8")) |>
#   head(10)

### fewer columns ----
v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22 |>
  dplyr::select(
    PERMIT_VESSEL_ID,
    permit_2022_int,
    permit_weeks_amnt_22,
    YEAR,
    compl_w_cnt,
    compl_2022,
    rep_type.t,
    rep_type.tne
  ) |>
  dplyr::distinct()

dim(v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short)
# [1] 5275    6
# [1] 6627    8
# [1] 4934    8 (metrics vsls)
# [1] 4858    8

v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short |>
  dplyr::filter(compl_2022 == "yes") |>
  head() |>
  dplyr::glimpse()
# $ PERMIT_VESSEL_ID     <chr> "FL4430NN", "FL2698TE", "FL2698TE", "FL2720R…
# $ permit_2022_int      <Interval> 2022-10-11 00:00:00 EDT--2022-12-30 19:…
# $ permit_weeks_amnt_22 <dbl> 12, 32, 32, 52, 52, 52
# $ YEAR                 <dbl> 2022, 2022, 2021, 2022, 2021, 2022
# $ compl_w_cnt          <int> 14, 53, 53, 53, 53, 52
# $ compl_2022           <chr> "yes", "yes", "yes", "yes", "yes", "yes"
# $ rep_type.t           <chr> NA, NA, NA, NA, NA, "trips"
# $ rep_type.tne         <chr> "trips_neg", "trips_neg", "trips_neg", "trip…

v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short |>
filter(PERMIT_VESSEL_ID == "FL2698TE") |>
  dplyr::glimpse()
# $ permit_weeks_amnt_22 <dbl> 32, 32
# $ YEAR                 <dbl> 2022, 2021
# $ compl_w_cnt          <int> 53, 1
# $ compl_2022           <chr> "yes", "no"
# w/o join_by YEAR:
# $ compl_w_cnt          <int> 53, 53
# $ compl_2022           <chr> "yes", "yes"

# TODO: check year = NA

## plot SA year ----
length(unique(v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     3956
# 2302 (from metrics)
# [1] 2239 excl. SRHS

sa_compl_cnts <-
  v_p__t__tne_d_weeks_sa_compl_cnt_w_short_compl22_short |>
  dplyr::select(PERMIT_VESSEL_ID,
         compl_2022) |>
  dplyr::distinct() |>
  dplyr::add_count(compl_2022, name = "total_compl_y")

sa_compl_cnts |>
  dplyr::select(compl_2022, total_compl_y) |>
  dplyr::distinct()
# 2 no                  2700
# 1 yes                 1257
#   compl_2022 total_compl_y
#   <chr>              <int>
# 1 yes                 1234
# 2 no                  1069
# excl. srhs
# 1 yes                 1229
# 2 no                  1011


# compl
# 1262 * 100 / (3956)
# 32%
# 1257 * 100 / (3956)
# 31.77452
1234 * 100 / (1234 + 1069)
# [1] 53.58228
# escl. srhs
1229 * 100 / (1229 + 1011)
# [1] 54.86607

# no

# 2695 * 100 / (3956)
# 68%
# 2700 * 100 / (3956)
# [1] 68.25076
1069 * 100 / (1234 + 1069)
# [1] 46.41772
1011 * 100 / (1229 + 1011)
# [1] 45.13393

# Total 2240

sa_compl_cnts_perc <-
  sa_compl_cnts |>
  dplyr::mutate(total_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  dplyr::select(-PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  dplyr::group_by(compl_2022) |>
  dplyr::mutate(compl_perc =
           total_compl_y * 100 / (total_vsls)) |>
  dplyr::ungroup()

# (was 41% yes vs. 59% no from 2178 vessels)
# print_df_names(sa_compl_cnts_perc)
sa22_title = "SA Only Permitted Vessels (Total permitted: {sa_compl_cnts_perc$total_vsls})"

compl_2022_ord <- factor(sa_compl_cnts_perc$compl_2022,
                         levels = c("yes", "no"))
year_plot_sa <-
  sa_compl_cnts_perc %>%
  ggplot(aes(x = compl_2022_ord,
             y = compl_perc,
             fill = compl_2022)) +
  # geom_col(position = "dodge") +
  geom_col() +
  ylim(0, 100) +
  labs(title = str_glue(sa22_title),
       x = "",
       # x = "Compliant",
       y = "") +
  geom_text(aes(label = paste0(round(compl_perc, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(
    values =
      c("yes" = "turquoise1",
        "no" = "yellow"),
    name = "Is compliant?",
    labels = c("no", "yes")
  )

# year_plot_sa

# SA compliance by month ----
v_p__t__tne_d_weeks_sa_compl_w_short <-
  v_p__t__tne_d_weeks_sa_compl_w |> 
  dplyr::select(-any_of(rm_columns)) |> 
  dplyr::distinct()

dim(v_p__t__tne_d_weeks_sa_compl_w_short)
# [1] 194697     22
# [1] 189214     22 (distinct)
# [1] 111468     20
# [1] 113909     21

## a month is compliant if all weeks are compliant ----

# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
### add beginning and end of permit as a date ----
v_p__t__tne_d_weeks_sa_compl_w_short_p_dates0 <-
  v_p__t__tne_d_weeks_sa_compl_w_short |>
  dplyr::mutate(
    permit_start = int_start(permit_2022_int),
    permit_end = int_end(permit_2022_int)
  )

# For each value in x, this finds everywhere that value falls between ⁠[y_lower, y_upper]⁠. Equivalent to ⁠x >= y_lower, x <= y_upper⁠ by default.
# 

permit_dates_by <-
  join_by(between(x$COMPLETE_DATE,
                  y$permit_start,
                  y$permit_end))

tic("v_p__t__tne_d_weeks_sa_compl_w_short_p_dates")
v_p__t__tne_d_weeks_sa_compl_w_short_p_dates <-
  right_join(dates_2022_yw,
            v_p__t__tne_d_weeks_sa_compl_w_short_p_dates0,
            permit_dates_by,
            suffix = c(".d", ".t"))
toc()
# v_p__t__tne_d_weeks_sa_compl_w_short_p_dates: 25.16 sec elapsed

# the month is not compliant if at least one week is not compliant
tic("v_p__t__tne_d_weeks_sa_compl_w_short_m")
v_p__t__tne_d_weeks_sa_compl_w_short_p_dates_m <-
  v_p__t__tne_d_weeks_sa_compl_w_short_p_dates |>
  dplyr::group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           date_y_m.d) |>
  dplyr::mutate(v_compliant_m =
           dplyr::case_when(any(tolower(sa_compl_week) == "no") ~ "no",
                     .default = "yes")) |>
  dplyr::ungroup()
toc()
# v_p__t__tne_d_weeks_sa_compl_w_short_m: 35.45 sec elapsed

v_p__t__tne_d_weeks_sa_compl_w_short_p_dates_m |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL2702KR") |> View()

v_p__t__tne_d_weeks_sa_compl_w_short_m_cnt <-
  v_p__t__tne_d_weeks_sa_compl_w_short |>
  dplyr::group_by(PERMIT_VESSEL_ID,
           VESSEL_VESSEL_ID,
           date_y_m) |>
  dplyr::mutate(compl_w_cnt_m = n_distinct(WEEK_OF_YEAR)) |>
  dplyr::ungroup()

# View(v_p__t__tne_d_weeks_sa_compl_w_short_m_cnt)
# [1] 113909     22

# non compliant only ----
## add counts of weeks per vessel by month, compl ----
count_weeks_per_vsl_permit_year_compl_month <-
  v_p__t__tne_d_weeks_sa_compl_w_short_m_cnt %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  ungroup %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_month %>% 
    # dplyr::select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  # unique() %>%
  dplyr::filter(year_month == "Dec 2022") %>%
  dplyr::glimpse()
# Rows: 11,031
# $ compliant_                         <chr> "YES", "NO", "YES", "YES",…
# $ total_vsl_m                        <int> 1657, 1657, 1657, 1657, 16…
# $ perm_exp_m                         <chr> "active", "active", "activ…
# $ exp_m_tot_cnt                      <int> 1656, 1656, 1656, 1656, 16…
# $ cnt_vsl_m_compl                    <int> 1282, 434, 1282, 1282, 434…
# $ weeks_per_vessel_per_compl_m       <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…

# test
count_weeks_per_vsl_permit_year_compl_month %>%
filter(year_permit == "2022 sa_only" &
           compliant_ == "NO") %>%
  dplyr::select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  dplyr::glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

## 1) Month: percent compl weeks per vsl per month ----

count_weeks_per_vsl_permit_year_compl_m_p <-
  count_weeks_per_vsl_permit_year_compl_month %>%
  dplyr::mutate(percent_compl_m =
           weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_per_compl_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_p %>%
  dplyr::filter(year_month == "Dec 2022") %>% 
  dplyr::filter(vessel_official_number == "NJ8126HN") %>%
  dplyr::select(
    vessel_official_number,
    year_month,
    compliant_,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m
  ) %>%
  unique() %>%
  dplyr::arrange(year_month) %>%
  dplyr::glimpse()
# $ compliant_                         <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl_m       <int> 1, 3
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 25, 75

## 2a) Month: Only non-compl and fewer cols ----
# View(count_weeks_per_vsl_permit_year_compl_m_p)
count_weeks_per_vsl_permit_year_compl_m_p_nc <-
  count_weeks_per_vsl_permit_year_compl_m_p %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_permit,
    year_month,
    vessel_official_number,
    perm_exp_m,
    exp_m_tot_cnt,
    cnt_vsl_m_compl,
    total_vsl_m,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

## 2b) Month: get percentage "buckets" ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b <-
  # Use F2 to see the function definition
  get_p_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

### check 2, by month ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  dplyr::filter(percent_n_compl_rank == '75<= & <=100%') %>%
  dplyr::filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA9236AV") %>%
  dplyr::add_count(percent_compl_m, year_permit,
        name = "amount_of_occurences") %>%
  # sort in the descending order
  dplyr::arrange(desc(percent_compl_m)) %>%
  # sum
  dplyr::add_count(wt = amount_of_occurences) %>%
  dplyr::glimpse()
# $ amount_of_occurences         <int> 12, 12, 12, 12, 12, 12, 12, 12…
# $ n                            <int> 144, 144, 144, 144, 144, 144, …

## 3) Month: count how many in each bucket ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
    dplyr::add_count(year_permit,
              year_month,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

# check by counting in a different way
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  dplyr::group_by(year_permit,
           year_month,
           percent_n_compl_rank) %>%
  dplyr::mutate(cnt_v_in_bucket1 =
           dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::filter(!(cnt_v_in_bucket == cnt_v_in_bucket1)) %>%
  dim()
# 0 - correct, no difference

### tests 3, by month ----
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot)

test_compare_with <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::filter(year_month == "Jan 2022") %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>% dim()
# total 703 nc vsls in "Jan 2022 sa_only"
# tot 1635 in Jan 2022
# 
# 45 nc vsls in "Jan 2022 gom_dual"
# 45 * 100 / 1192 = 3.8%

# still true?
test_res <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::filter(year_month == "Jan 2022") %>%
  dplyr::select(vessel_official_number) %>%
  unique() %>% dim()
#  703 1

test_compare_with[1] == test_res[1]
# TRUE

## 4) Month: cnt percents of (3) ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4, by month ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::filter(year_month == "Dec 2022") %>%
  dplyr::select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
# Dec 2022
# 1 25<= & <50%                         2.30
# 2 50<= & <75%                         4.15
# 3 75<= & <=100%                      93.5 

# Jan 2022
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0<= & <25%                          4.69
# 2 25<= & <50%                         4.13
# 3 50<= & <75%                         4.13
# 4 75<= & <=100%                      87.1

# 612*100/703 == 87.05548

## 5) Month plots ----

## 5a) prepare the df for plotting ----
### keep only fields needed to plot ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  dplyr::select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  unique()
  
### add column with Month name only (for plotting) ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short %>% 
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

# check
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
# [1] 11766    15
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)
# [1] 107  12
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)

### split the df by year_permit into a list ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short$year_permit))

### get used year_permits ----
sorted_year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2022 sa_only"  "2023 sa_dual"

### make titles ---- 
get_year_permit_titles <- function(permit, year) {
      paste0("% of non-compliant ",
             permit,
             " Permitted vessels by month",
             " (", year, ")"
             ) %>%
    return()
}

year_permit_titles <-
  data.frame(
    super_title_gom = get_year_permit_titles("Gulf + Dual", "2022"),
    super_title_sa = get_year_permit_titles("South Atlantic Only", "2022"),
    super_title_2023 = get_year_permit_titles("South Atlantic + Dual", "2023")
  )

names(year_permit_titles) <- sorted_year_permits

### additional functions for Month plots ----
# TODO: simplify
# returns 0 or number of expired permits
get_expired_permit_numbers <- function(curr_data) {
  # browser()

  exp_filt <- curr_data %>%
    dplyr::filter(perm_exp_m == "expired") %>% 
    unique()
  
  res = exp_filt$exp_m_tot_cnt
  
  # if dplyr::filter(perm_exp_m == "expired") returned nothing
  if (dim(exp_filt)[1] == 0)
  {
      res = 0
  }
  
  return(res)
}

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      dplyr::filter(year_month == curr_year_month)
    
    curr_month_name <- unique(curr_data$month_only)
    
    curr_year_permit <- unique(curr_data$year_permit)
    
    curr_tot_v_per_m_y_r <- unique(curr_data$cnt_vsl_m_compl)
    
    curr_m_tot_active <- curr_data %>%
      dplyr::filter(perm_exp_m == "active") %>%
      dplyr::select(exp_m_tot_cnt) %>%
      unique()

    # see function definition F2
    cnt_expired <- get_expired_permit_numbers(curr_data) 

    curr_title <- paste0(
      curr_month_name,
      " (",
      curr_tot_v_per_m_y_r,
      " vsls; ",
      curr_m_tot_active$exp_m_tot_cnt,
      " act. p.; ",
      cnt_expired,
      " exp. p.)"
    )
    
    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           # no labels for axes
           x = "",
           y = "") +
      # text on each bar
      geom_text(aes(label = perc_labels),
                # posintion - middle
                position = position_stack(vjust = 0.5)) +
      # Y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title = 
              element_text(size = 10))
    
    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_permits %>%
  purrr::map(
    # for each year and permit pull a df from the list
    function(current_year_permit) {
      # browser()
      curr_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[[current_year_permit]]
      
      curr_year_months <-
        curr_df %>%
        dplyr::select(year_month) %>%
        unique() %>%
        as.data.frame()
      
      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # run the function for each month
        # see the function definition F2
        purrr::map( ~ get_one_plot_by_month(curr_df,
                                            curr_year_month = .))
      
      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)
      
      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    })

# check
# test_df <-
#   count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[["2022 sa_only"]]
# 
# get_one_plot_by_month(test_df,
#                       curr_year_month = "Aug 2022")

# gg_month_nc_perc[[1]][[2]]

footnote_text <- "In parenthesis are 1) # of non compliant vessels per month; 2) total active permits per month; 3) total expired permits per month;"

# footnote <- textGrob(
#   footnote_text,
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -0.5,
#   # just = "right",
#   x = 0.01,
#   y = 0.99,
#   vjust = 1
# )

### common axes for Months ----
y_left <- textGrob("% per 'bucket'",
                   # angle
                   rot = 90,
                   gp = gpar(fontsize = 10))

x_bottom <- textGrob("'buckets' - distibution of % of non compliant weeks per vessel", 
                  gp = gpar(fontsize = 10))

all_plots_w_titles_list <-
  gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    # get a name
    curr_year_permit <- curr_year_reg_list[[1]]
    
    # get a title by the name
    curr_super_title <- year_permit_titles[[curr_year_permit]]
    
    # add a subtitle
    whole_title <-
      paste0(curr_super_title,
             # new line
             "\n",
             footnote_text)
    
    all_plots_per_year_region <-
      gridExtra::arrangeGrob(
        grobs =
          curr_year_reg_list[[2]],
        top = whole_title,
        left = y_left,
        bottom = x_bottom,
        ncol = 3
      )
    
    # combine the current year_permit and the plots in a list
    res <- list(curr_year_permit,
                all_plots_per_year_region)
    
    return(res)
  })

# warnings()
# ...
# 22: Removed 1 rows containing missing values (`geom_text()`).
# 23: Removed 1 rows containing missing values (`geom_col()`).

# draw one plot to test
gridExtra::grid.arrange(all_plots_w_titles_list[[2]][[2]])

# View(all_plots_w_titles_list)

## all plots per month to files ----
# saves to PNG, PDF etc. depending on an extension in "file_full_name"
save_plots_list_to_files <-
  function(file_full_name,
           plots_list) {
    ggplot2::ggsave(
      file_full_name,
      plots_list,
      width = 30,
      height = 20,
      units = "cm"
    )
  }

all_plots_w_titles_list %>%
  # repeat for each element of the list
  purrr::map(function(curr_plot_list) {
    file_name_base <- paste0(curr_plot_list[[1]],
                             "_percent_distribution_per_month",
                             ".png")
    
    file_path <-
      r"(quantify_compliance\jun_21_2023\per_month)"
    
    # file.path adds the correct concatenation
    file_full_name <- file.path(my_paths$outputs,
                                file_path,
                                file_name_base)
    
    # see the function definition F2
    save_plots_list_to_files(file_full_name,
                             # plots
                             curr_plot_list[[2]])
  })

# [[1]]
# [1] "~/R_files_local/my_outputs/quantify_compliance\\jun_21_2023\\per_month/2022 gom_dual_percent_distribution_per_month.png"...

