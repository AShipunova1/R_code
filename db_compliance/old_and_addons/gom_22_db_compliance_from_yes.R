# GOM + dual compl by year ----
# GOM must have both (1) a logbook for any fishing intended declaration, and (2) an intended fishing declaration for any logbook

# There should be a declaration for every logbook.
# There should be a logbook for every declaration of a charter or headboat intending to fish.
# Noncompliant + overridden are compliant

## get gom and dual vsls ----
v_p__t__tn_d_weeks_gom <-
  v_p__t__tn_d_weeks |>
  dplyr::filter(permit_sa_gom_dual %in% c("gom_only", "dual"))

dim(v_p__t__tn_d_weeks_gom)
# [1] 75524    91
# [1] 75403    92

## rm extra cols ----
### find empty columns ----
names(v_p__t__tn_d_weeks_gom) |>
  length()
# 92

empty_cols <-
  v_p__t__tn_d_weeks_gom |>
  map_df(function(x) {
    if (length(unique(x)) == 1) {
      return(unique(x))
    }
  })
# dim(empty_cols)[2]
# 32

t_names_to_rm <-
  c("ADDDITIONAL_FISHERMEN",
    "APP_VERSION",
    "APPROVAL_DATE",
    "APPROVED_BY",
    "BAIT_WEIGHT",
    "CANCEL_FLAG",
    "CAPT_NAME_FIRST",
    "CAPT_NAME_LAST",
    "CF_ID",
    "CF_ISS_AGENCY",
    "CF_PERMIT_ID",
    "CONFIRMATION_SIGNATURE",
    "CONFIRMED_VALIDATING_AGENCY",
    "COST_BAIT",
    "COST_FOOD",
    "COST_ICE",
    "COST_IFQ",
    "COST_LIGHT",
    "COST_MISC",
    "DAYS_AT_SEA",
    "DC",
    "DE",
    "DEA_PERMIT_ID",
    "END_PORT",
    "FORM_VERSION",
    "FUEL_DIESEL_GALLON_PRICE",
    "FUEL_DIESEL_GALLONS",
    "FUEL_GALLON_PRICE",
    "FUEL_GALLONS",
    "FUEL_GAS_GALLON_PRICE",
    "FUEL_GAS_GALLONS",
    "ICE_MAKER",
    "NBR_OF_CREW",
    "NBR_PAYING_PASSENGERS",
    "NUM_ANGLERS",
    "OWNER_ABOARD",
    "PARTNER_VTR",
    "PAY_PERCENT_TO_CAPT",
    "PAY_PERCENT_TO_CREW",
    "PAY_PERCENT_TO_OWNER",
    "PAY_TO_CAPT_CREW",
    "PORT",
    "REPORTING_SOURCE",
    "REVENUE_TOTAL",
    "SEA_TIME",
    "SPLIT_TRIP",
    "START_PORT",
    "STATE",
    "SUB_TRIP_TYPE",
    "SUBMITTED_BY_PARTICIPANT",
    "SUPPLIER_TRIP_ID",
    "TICKET_TYPE",
    "TRANSMISSION_DATE",
    "TRIP_FEE",
    "TRIP_NBR",
    "TRIP_START_DATE_TIME",
    "UC",
    "UE",
    "VALIDATING_AGENCY",
    "VENDOR_APP_NAME",
    "VENDOR_PLATFORM",
    "VTR_NUMBER"
  )

# do not rm:
# "ACTIVITY_TYPE",
# "EVENT_ID",
# "STATUS",
# "SUBMIT_METHOD",
# "TRIP_END_TIME",
# "TRIP_START_TIME",

v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom |>
  dplyr::select(-any_of(t_names_to_rm)) |>
  dplyr::distinct()

dim(v_p__t__tn_d_weeks_gom)
# [1] 75524    91
# [1] 75403    92

dim(v_p__t__tn_d_weeks_gom_short)
# [1] 75524    35
# [1] 75403    36

data_overview(v_p__t__tn_d_weeks_gom_short) |>
  head(2)
# VESSEL_VESSEL_ID      1351
# PERMIT_VESSEL_ID      1351

v_p__t__tn_d_weeks_gom_short |>
  dplyr::count(permit_sa_gom_dual)
# 1 dual               15875
# 2 gom_only           59649
# 1 dual               15853
# 2 gom_only           59550

v_p__t__tn_d_weeks_gom_short |>
  dplyr::count(TRIP_TYPE)
#   TRIP_TYPE     n
#   <chr>     <int>
# 1 A         63121
# 2 H         11935
# 3 NA          468
# 1 A         63009
# 2 H         11926
# 3 NA          468
# Activity type NA - declaration only

v_p__t__tn_d_weeks_gom_short |>
  dplyr::count(INTENDED_FISHING_FLAG)

# by <- join_by(unique_all_vessel_ids,
# #               overlaps(x$EFFECTIVE_DATE,
# #                        x$my_end_date,
# #                        y$EFFECTIVE_DATE,
# #                        y$my_end_date,
# #                        bounds = "[)"))

length(unique(v_p__t__tn_d_weeks_gom$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     1351

# TODO: check if permit_id in tn mean the same as in p_v

## wrong TRIP_START_TIME.tn fix ----
# ℹ In argument: `TRIP_START_TIME_tn_hm = parse_date_time(TRIP_START_TIME.tn,
#   "HM")`.
# Caused by warning:
# !  2 failed to parse.

v_p__t__tn_d_weeks_gom_short |>
  dplyr::select(TRIP_START_TIME.tn) |>
  dplyr::distinct() |>
  dplyr::arrange(desc(TRIP_START_TIME.tn)) |>
  rowwise() |>
  head(3) |>
  # count number of digits, should be 4
  # dplyr::mutate(l = floor(log10(as.numeric(TRIP_START_TIME.tn))) + 1)
  dplyr::mutate(l = nchar(TRIP_START_TIME.tn)) |>
  dplyr::ungroup()
#   <chr>              <int>
# 1 601                    3
# 2 600                    3
# 3 2359                   4

v_p__t__tn_d_weeks_gom_short |>
  dplyr::filter(!(nchar(TRIP_START_TIME.tn) == 4) |
           !(nchar(TRIP_START_TIME.t) == 4)
         ) |>
  dplyr::glimpse()
# $ TRIP_START_TIME.t    <chr> "0700", "0601", NA
# $ TRIP_START_TIME.tn   <chr> "601", "601", "600"

# assume the missing leading zero, can't be 6010 etc.
#### restore ----
v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom_short |>
  dplyr::mutate(TRIP_START_TIME.tn =
           case_when(
           nchar(TRIP_START_TIME.tn) < 4 ~
           paste0("0", TRIP_START_TIME.tn),
         .default = TRIP_START_TIME.tn
           )
         )

## 1) match logbooks and declarations ----

# There should be a logbook for every declaration of a charter or headboat intending to fish.
# decl trip start < or > 1h logbooks trip start

### at least one pair of matching declarations per week ----
# non compl if there is a logbook w/o a decl

filter_logb_for_decl_fish <- rlang::quo(
  rep_type.tn == "trips_notif" &
  INTENDED_FISHING_FLAG == 'Y'
  &
  rep_type.t == "trips"
)

# v_p__t__tn_d_weeks_gom_short |>
#   dplyr::filter(!!filter_logb_for_decl_fish) |>
#   dim()
# [1] 43802    35
# [1] 43767    36

#### decl trip start < or > 1h logbooks trip start ----

# already matched by day
tic("v_p__t__tn_d_weeks_gom_short_matched")
v_p__t__tn_d_weeks_gom_short_matched <-
  v_p__t__tn_d_weeks_gom_short |>
  dplyr::group_by(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |>
  # convert time to a Date format
  dplyr::mutate(
    TRIP_START_TIME_t_hm =
      parse_date_time(TRIP_START_TIME.t, "HM"),
    TRIP_START_TIME_tn_hm =
      parse_date_time(TRIP_START_TIME.tn, "HM")
  ) |>
  # count the difference between start times t and tn
  dplyr::mutate(time_diff1 = abs(TRIP_START_TIME_t_hm - (TRIP_START_TIME_tn_hm))) |>
  # less than an hour difference between trip and trip notif start time
  dplyr::mutate(matched_reports =
           case_when(time_diff1 < 3600 ~ "matched",
                     .default = "not_matched"
           )) |>
  dplyr::distinct() |>
  dplyr::ungroup()
toc()

dim(v_p__t__tn_d_weeks_gom_short_matched)
# [1] 35662    41
# [1] 35664    38 dplyr::filter with restored time
# [1] 44312    39 add matched col
# [1] 75524    39 without dplyr::filter out not matched
# [1] 75403    40

v_p__t__tn_d_weeks_gom_short_matched |>
  dplyr::select(PERMIT_VESSEL_ID, matched_reports) |>
  dplyr::distinct() |>
  dplyr::count(matched_reports)
# 1 matched           595
# 2 not_matched      1300

length(unique(v_p__t__tn_d_weeks_gom_short_matched$PERMIT_VESSEL_ID))
# [1] 1351

## strict compl vessels per week ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w")
v_p__t__tn_d_weeks_gom_short_matched_compl_w <-
  v_p__t__tn_d_weeks_gom_short_matched |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(matched_compl =
           # all logbooks and decl should match for any given week
           case_when(all(matched_reports == "matched") ~
                       "yes",
                     .default = "no")) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w: 5.39 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::count(INTENDED_FISHING_FLAG)
# 1 N                      4072
# 2 Y                     68177
# 3 NA                     3275

  # INTENDED_FISHING_FLAG matched_compl     n
# 1 N                     no              2739
# 2 N                     yes             1333
# 3 Y                     no             23597
# 4 Y                     yes            44580
# 5 NA                    no              2917
# 6 NA                    yes              358

# check compliant_fishing_month
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::count(
    # VESSEL_VESSEL_ID,
    #     PERMIT_VESSEL_ID,
    # WEEK_OF_YEAR,
    date_y_m,
    INTENDED_FISHING_FLAG,
    matched_compl) |>
  dplyr::glimpse()
  # write_csv("compliant_fishing_month.csv")

v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::select(PERMIT_VESSEL_ID,
         date_y_m,
         matched_reports,
         INTENDED_FISHING_FLAG,
         matched_compl) |>
  dplyr::distinct() |>
  dplyr::group_by(date_y_m,
           matched_reports,
           INTENDED_FISHING_FLAG,
           matched_compl) |>
  dplyr::mutate(cnt_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  dplyr::select(-PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  dplyr::glimpse()
# disregard "not_matched & yes", that means there are more than 1 decl

### a good example of 2 decl per week with compliant ----
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::filter(date_y_m == "Feb 2022") |>
  dplyr::filter(PERMIT_VESSEL_ID == "1093374") |>
  dplyr::select(
    VESSEL_VESSEL_ID,
    PERMIT_VESSEL_ID,
    WEEK_OF_YEAR,
    date_y_m,
    TRIP_TYPE,
    TRIP_START_DATE,
    TRIP_START_TIME.t,
    ACTIVITY_TYPE,
    SERO_VESSEL_PERMIT,
    trip_int,
    TRIP_START_TIME.tn,
    INTENDED_FISHING_FLAG,
    time_diff1,
    matched_reports,
    matched_compl
  ) |>
  dplyr::distinct() |>
  View()

### count vessel per month by matched_compl ----
v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         matched_compl) |>
  dplyr::distinct() |>
  dplyr::group_by(date_y_m,
           WEEK_OF_YEAR,
           matched_compl) |>
  dplyr::mutate(cnt_vsls =
           case_when(
             any(matched_compl == "yes") ~
               n_distinct(PERMIT_VESSEL_ID),
             .default = 0
           )) |>
  dplyr::select(-PERMIT_VESSEL_ID) |>
  dplyr::distinct()
# disregard "not_matched & yes", that means there are more than 1 decl

glimpse(v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w)

# check vessel cnt by matched compl in Mar
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         matched_compl) |>
  dplyr::distinct() |>
  dplyr::group_by(date_y_m,
           WEEK_OF_YEAR,
           matched_compl) |>
  dplyr::filter(date_y_m == 'Mar 2022') |>
  subset(PERMIT_VESSEL_ID %in% PERMIT_VESSEL_ID[matched_compl == 'yes']) |>
  dplyr::mutate(cnt_c1 = n_distinct(PERMIT_VESSEL_ID)) |>
  dplyr::ungroup() |>
  dplyr::select(-PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  View()

## no matched declarations, but compliant? ----
# There should be a logbook for every declaration of a charter or a headboat intending to fish.

## 2) There are only a not matched not fishing intended declarations per week ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_2")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(not_fish_compl =
           case_when(
             matched_compl == "no" &
               # all reports are not fishing declarations
               all(INTENDED_FISHING_FLAG == "N" &
                     !is.na(rep_type.tn)) ~ "yes",
             .default = "no"
           )) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_2: 8.93 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  dplyr::count(not_fish_compl)
# 1 no             73009
# 2 yes             2394

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  # dplyr::filter(not_fish_compl == "yes") |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL4459PW") |>
  dplyr::arrange(WEEK_OF_YEAR) |>
  dplyr::glimpse()

## 3) a week with no reports of any kind (compl) ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_3")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(
    no_rep_compl =
      case_when(
        matched_compl == "no" &
          not_fish_compl == "no" &
          # no reports
          all(is.na(rep_type.t)) &
          all(is.na(rep_type.tn)) ~ "yes",
        .default = "no"
      )
  ) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3: 7.41 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  dplyr::count(no_rep_compl)
# 1 no           74935
# 2 yes            468

#   dplyr::filter(PERMIT_VESSEL_ID == "FL3627NN") |>
  # View()
#
# > v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   dplyr::filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > v_p__t__tn_d_weeks |>
#   dplyr::filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > trips_info_2022 |>
#   dplyr::filter(VESSEL_ID == "330659") |>
#   View()
# 0
# > trips_notifications_2022 |>
#   dplyr::filter(VESSEL_ID == "330659") |>
#   View()
# 0

## 4) a week with a logb and no decl (err, is non-compliant) ----
# TODO: check in FHIER

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   dplyr::filter(VESSEL_VESSEL_ID == 72359,
#          PERMIT_VESSEL_ID == "933533")

tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_4")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(
    no_decl_compl_0 =
      case_when(
        matched_compl == "no" &
          not_fish_compl == "no" &
          no_rep_compl == "no" &
          # no decl for a lgb
          !is.na(rep_type.t) &
          is.na(rep_type.tn)
        # cnt_t > cnt_tn
        ~ "yes",
        .default = "no"
      )
  ) |>
  dplyr::mutate(no_decl_compl =
           case_when(all(no_decl_compl_0 == "yes") ~ "yes",
                     .default = "no")) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4: 11.72 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  dplyr::count(no_decl_compl_0)
# 1 no              73738
# 2 yes              1665
  # dplyr::count(no_decl_compl)
# 1 no            73812
# 2 yes            1591

# v_yes <-
#   list(
#     v_p__t__tn_d_weeks_gom_short_matched_compl_w_4,
#     v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a
#   ) |>
#   map(function(curr_df) {
#     # browser()
#     curr_df |>
#       dplyr::filter(no_decl_compl == "yes") |>
#       dplyr::select(PERMIT_VESSEL_ID) |>
#       dplyr::distinct() %>%
#       return()
#   })

# str(v_yes)
 # $ : tibble [82 × 1] (S3: tbl_df/tbl/data.frame)
 #  ..$ PERMIT_VESSEL_ID: chr [1:82] "FL4482RC" "1162015" "1162266" "FL6786PB" ...
 # $ : tibble [115 × 1] (S3: tbl_df/tbl/data.frame)
 #  ..$ PERMIT_VESSEL_ID: chr [1:115] "FL4482RC" "FL3326MC" "1271163" "1271879" ...

# in4 <-
#   setdiff(v_yes[[1]]$PERMIT_VESSEL_ID,
#           v_yes[[2]]$PERMIT_VESSEL_ID)
# length(in4)
# 0

# in4a <-
#   setdiff(v_yes[[2]]$PERMIT_VESSEL_ID,
#           v_yes[[1]]$PERMIT_VESSEL_ID)
#
# length(in4a)
# 34
#  [1] "FL3326MC" "1271163"  "1271879"  "1174344"  "1211890"  "1174689"
#  [7] "1109181"  "1087799"  "1217823"  "1264525"  "FL6017NC" "LA2117GM"
# [13] "FL8511RT" "FL9207ST" "FL8845ML" "FL1640RJ" "FL0957RW" "AL0364RL"
# [19] "AL1172RG" "1076615"  "FL1383SN" "1023529"  "1310120"  "1313218"
# [25] "1299573"  "980844"   "FL2564MC" "FL1751SN" "FL2305NU" "FL1909RG"
# [31] "573252"   "555530"   "592015"

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |>
#   dplyr::filter(PERMIT_VESSEL_ID %in%
#            v_yes[[2]]$PERMIT_VESSEL_ID) |>
#   View()

# FL3326MC_4 <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
#   dplyr::filter(PERMIT_VESSEL_ID == "FL3326MC") |>
#   dplyr::select(-no_decl_compl_0)
#
# FL3326MC_4a <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |>
#   dplyr::filter(PERMIT_VESSEL_ID == "FL3326MC")
#
# library(diffdf)
# diffdf(FL3326MC_4, FL3326MC_4a)
# All rows are shown in table below

  #    VARIABLE     ..ROWNUMBER..  BASE  COMPARE
  #  no_decl_compl       37         no     yes

# dplyr::glimpse(FL4482RC_4[37,])
# dplyr::glimpse(FL4482RC_4a[37,])

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  dplyr::filter(WEEK_OF_YEAR == 26,
         PERMIT_VESSEL_ID == "FL3326MC") |>
  dplyr::glimpse()
# $ cnt_t                 <int> 1, 1
# $ cnt_tn                <int> 1, 1
# 2 entries, 1 has t, another has a tn! should be not compl

## 5) no logbook, but a decl is a "no fish" - compl ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_5")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(
    no_lgb_compl =
      case_when(
        matched_compl == "no" &
          not_fish_compl == "no" &
          no_rep_compl == "no" &
          INTENDED_FISHING_FLAG == "N" &
          # no lgb for a not fish decl
          is.na(rep_type.t) &
          !is.na(rep_type.tn)
        # HERE
        # there is a decl
        !is.na(rep_type.tn)
        ~ "yes",
        # TODO: check the same week with other steps
        .default = "no"
      )
  ) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 13.05 sec elapsed
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 3032.68 sec elapsed rowwise
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 7.04 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  dplyr::count(no_lgb_compl)
# 1 no           75060
# 2 yes            343

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  dplyr::arrange(desc(no_lgb_compl)) |>
  dplyr::glimpse()

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  dplyr::arrange(desc(no_lgb_compl)) |>
  dplyr::filter(no_decl_compl == "yes" & no_lgb_compl == "yes") |>
  # dplyr::count(n_distinct(PERMIT_VESSEL_ID))
  dplyr::glimpse()
# 0

## 6) not compliant but overridden ----
# everything that was overridden is compliant (the whole week)
compl_err_db_data_short <-
  compl_err_db_data |>
  dplyr::select(
    comp_error_type_cd,
    comp_override_cmt,
    # comp_override_dt,
    # comp_override_user_id,
    comp_week,
    # comp_week_end_dt,
    # comp_week_start_dt,
    comp_year,
    is_comp,
    is_comp_override,
    is_override,
    safis_vessel_id,
    vessel_official_nbr
  ) |>
  dplyr::distinct()

override_join_by =
  join_by(
    VESSEL_VESSEL_ID == safis_vessel_id,
    PERMIT_VESSEL_ID == vessel_official_nbr,
    WEEK_OF_YEAR == comp_week,
    YEAR == comp_year
  )

compl_err_db_data_short_overr <-
  compl_err_db_data_short |>
  dplyr::filter(is_comp_override == 1)

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr <-
  left_join(
    v_p__t__tn_d_weeks_gom_short_matched_compl_w_5,
    compl_err_db_data_short_overr,
    override_join_by,
    relationship = "many-to-many",
    suffix = c(".o", ".c_w")
  )

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5)
# [1] 75403    46

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr)
# [1] 97679    55 all overr
# [1] 77748    51


# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr |>
#     dplyr::select(comp_error_type_cd) |>
#     dplyr::distinct()
#     comp_error_type_cd
  # 1         DECL_NO_TRIP
# 2 SUBMIT_AFTER_ARRIVAL
# 3         TRIP_NO_DECL
# 4   VAL_ERROR_TRIP_GOM
  # 5     VMS_DECL_NO_TRIP
# 6     TRIP_BEFORE_DECL
# 7        NO_TRIP_FOUND
# 8                 <NA>

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr |>
#   dplyr::filter(comp_error_type_cd %in% c("DECL_NO_TRIP",
#                                    "NO_TRIP_FOUND",
#                                    "TRIP_NO_DECL")) |>
#   View()

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr |>
#   dplyr::select(srfh_for_hire_type_id) |>
#   dplyr::distinct()
#   srfh_for_hire_type_id
# 1                     2
# 2                    NA

# HERE:

all_not_compl_filter <-
  rlang::quo(
    # any is a no - out of compliance
    # matched_compl == "no" &
    #   not_fish_compl == "no" &
    #   no_rep_compl == "no" &
    #   no_decl_compl == "no" &
    #   no_lgb_compl == "no"
  )

tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr |>
  dplyr::group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  dplyr::mutate(compl_w =
           case_when(!!all_not_compl_filter ~ "no",
                     .default = "yes")) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1: 7.61 sec elapsed

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1)
# [1] 77748    52

### fewer fields ----
rm_fields <-
  c(
    "ACTIVITY_TYPE",
    "SERO_VESSEL_PERMIT",
    "comp_override_dt",
    "comp_override_user_id",
    "comp_week_end_dt",
    "comp_week_start_dt",
    "DC.t",
    "DC.tn",
    "DE.t",
    "DE.tn",
    "EVENT_ID",
    "INTENDED_FISHING_FLAG",
    # "is_comp_override", #(all 1)
    "is_override", #(all 0)
    "matched_compl",
    "matched_reports",
    "no_decl_compl",
    "no_decl_compl_0",
    "no_lgb_compl",
    "no_rep_compl",
    "not_fish_compl",
    "NOTIFICATION_TYPE_IDs",
    "rep_type.t",
    "rep_type.tn",
    "SUBMIT_METHOD",
    "time_diff1",
    "TRIP_END_DATE",
    "TRIP_END_m",
    "TRIP_END_TIME.t",
    "TRIP_END_TIME.tn",
    "TRIP_END_week_num",
    "TRIP_END_y",
    "trip_int",
    "TRIP_START_DATE",
    "TRIP_START_TIME_t_hm",
    "TRIP_START_TIME_tn_hm",
    "TRIP_START_TIME.t",
    "TRIP_START_TIME.tn",
    "TRIP_TYPE",
    "UC.t",
    "UC.tn",
    "UE.t",
    "UE.tn"
  )

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1 |>
  dplyr::select(-any_of(rm_fields)) |>
  dplyr::distinct()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short)
# [1] 77748    14
# [1] 22072    14

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short |>
#   dplyr::filter(compl_w == "no") |>
#   dplyr::filter(comp_error_type_cd == "SUBMIT_AFTER_ARRIVAL") |>
#   View()
  # dplyr::select(comp_error_type_cd) |>
  # dplyr::distinct()
# 1 DECL_NO_TRIP
# 2 SUBMIT_AFTER_ARRIVAL
# 3 TRIP_NO_DECL
# 4 VAL_ERROR_TRIP_GOM
# 5 VMS_DECL_NO_TRIP
# 6 TRIP_BEFORE_DECL
# 7 NO_TRIP_FOUND


# total gom compl ----
# not compliant and overridden = compliant
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short |>
  dplyr::mutate(compl_w_total =
           case_when(
             compl_w == "no" &
               (!is.na(is_comp_override) &
                  is_comp_override == 1) ~ "yes",
             .default = compl_w
           ))

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w)
# [1] 22072    15

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w |>
  dplyr::select(-c(
    comp_error_type_cd,
    comp_override_cmt,
    is_comp,
    is_comp_override,
    compl_w
  )) |>
  dplyr::distinct()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short)
# [1] 21551    10

## compliant per month ----
tic(
  "v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m"
)
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
  dplyr::group_by(PERMIT_VESSEL_ID, date_y_m) |>
  dplyr::mutate(compl_per_m_w = list(na.omit(unique(compl_w_total))),
         compl_m_len = lengths(compl_per_m_w)) |>
  dplyr::mutate(compl_m = case_when(compl_m_len > 1 ~ "no",
                             compl_per_m_w == "no" ~ "no",
                             .default = "yes")) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m: 2.47 sec elapsed

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m)
# [1] 21551    13 distinct

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m |>
  dplyr::filter(PERMIT_VESSEL_ID == "FL4463MX" &
           date_y_m == "Jun 2022") |>
  dplyr::arrange(WEEK_OF_YEAR) |>
  dplyr::distinct() |>
  dplyr::glimpse()
# $ WEEK_OF_YEAR     <dbl> 22, 23, 24, 25, 26
# $ compl_w          <chr> "no", "no", "yes", "yes", "yes"
# $ compl_m          <chr> "no", "no", "no", "no", "no"

## GOM total compliance per year ----
tic(
  "v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y"
)
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m |>
  dplyr::group_by(PERMIT_VESSEL_ID) |>
  dplyr::mutate(compl_per_y_w = list(na.omit(unique(compl_w_total))),
         compl_y_len = lengths(compl_per_y_w)) |>
  dplyr::mutate(compl_y = case_when(compl_y_len > 1 ~ "no",
                             compl_per_y_w == "no" ~ "no",
                             .default = "yes")) |>
  dplyr::ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y: 0.72 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y |>
  dplyr::count(compl_y)
# 1 no      11645
# 2 yes      9906

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y |>
  dplyr::select(PERMIT_VESSEL_ID, compl_y) |>
  dplyr::distinct() |>
  dplyr::count(compl_y)
# 1 no        476
# 2 yes       875
# 476 + 875
# 1351

476 * 100 / (476 + 875)
# 35.23316 % no

875 * 100 / (476 + 875)
# 64.76684 % yes
# (the same as "new rule")
# was
# 20% no
# 80% yes

# w. overridden
# 1 no        453
# 2 yes       898
# 898 + 453 = 1351

453 * 100 / (453 + 898)
# 33.53072 % no

898 * 100 / (453 + 898)
# 66.46928 % yes

# FHIER
# 12/31/2021
# Total Dual (SA & GOM) Permitted Vessels
# 308
# Total Vessels With GOM Permit Only	
# 1021
# 1021 + 308
# 1329

# Total Vessels	SA Only Non-compliant	SA Only Compliant	GOM Only Non-compliant	GOM Only Compliant	Dual (SA & GOM) Non-compliant	Dual (SA & GOM) Compliant
# 3,629	932	1,368	1	1,020	22	286

# 23 - not compl
# 1020 + 286 = 1306 compl

