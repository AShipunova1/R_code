# Only not in Headboat survey
# By default all are compliant if there are no reports.
# By default all are compliant if there are no reports.
# If they have submitted anything - should be a declaration and a logbook

# GOM + dual compl by year ----

## get gom and dual vsls ----
v_p__t__tn_d_weeks_gom <-
  v_p__t__tn_d_weeks |>
  filter(permit_sa_gom_dual %in% c("gom_only", "dual"))

dim(v_p__t__tn_d_weeks_gom)
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
dim(empty_cols)[2]
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
  select(-any_of(t_names_to_rm)) |>
  distinct()

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
  count(permit_sa_gom_dual)
# 1 dual               15853
# 2 gom_only           59550

v_p__t__tn_d_weeks_gom_short |>
  count(TRIP_TYPE)
#   TRIP_TYPE     n
# 1 A         63009
# 2 H         11926
# 3 NA          468
# Activity type NA - declaration only

v_p__t__tn_d_weeks_gom_short |>
  count(INTENDED_FISHING_FLAG)

length(unique(v_p__t__tn_d_weeks_gom$PERMIT_VESSEL_ID))
# PERMIT_VESSEL_ID     1351

# TODO: check if permit_id in tn mean the same as in p_v

## wrong TRIP_START_TIME.tn fix ----
# From error msg
# ℹ In argument: `TRIP_START_TIME_tn_hm = parse_date_time(TRIP_START_TIME.tn,
#   "HM")`.
# Caused by warning:
# !  2 failed to parse.

v_p__t__tn_d_weeks_gom_short |>
  select(TRIP_START_TIME.tn) |>
  distinct() |>
  arrange(desc(TRIP_START_TIME.tn)) |>
  rowwise() |>
  head(3) |>
  # count number of digits, should be 4
  # mutate(l = floor(log10(as.numeric(TRIP_START_TIME.tn))) + 1)
  mutate(l = nchar(TRIP_START_TIME.tn)) |>
  ungroup()
#   <chr>              <int>
# 1 601                    3
# 2 600                    3
# 3 2359                   4

# check if the wrong time was from VMS:
v_p__t__tn_d_weeks_gom |>
  filter(!(nchar(TRIP_START_TIME.tn) == 4) |
           !(nchar(TRIP_START_TIME.t) == 4)
         ) |>
  glimpse()
# $ PERMIT_VESSEL_ID            <chr> "FL8373MZ", "FL8373MZ", "698897"
# $ TRIP_START_TIME.tn          <chr> "601", "601", "600"
# $ UE.tn                       <chr> "VESL", "VESL", "VMS"

v_p__t__tn_d_weeks_gom_short |>
  filter(!(nchar(TRIP_START_TIME.tn) == 4) |
           !(nchar(TRIP_START_TIME.t) == 4)
         ) |>
  glimpse()
# $ TRIP_START_TIME.t    <chr> "0700", "0601", NA
# $ TRIP_START_TIME.tn   <chr> "601", "601", "600"

# assume the missing leading zero, can't be 6010 etc.
#### restore ----
v_p__t__tn_d_weeks_gom_short <-
  v_p__t__tn_d_weeks_gom_short |>
  mutate(TRIP_START_TIME.tn =
           case_when(
           nchar(TRIP_START_TIME.tn) < 4 ~
           paste0("0", TRIP_START_TIME.tn),
         .default = TRIP_START_TIME.tn
           )
         )

# create date time columns ----
grep("start", names(v_p__t__tn_d_weeks_gom_short),
     ignore.case = T, value = T)
# [1] "TRIP_START_DATE"    "TRIP_START_TIME.t"  "TRIP_START_TIME.tn"

v_p__t__tn_d_weeks_gom_short_dt <-
  v_p__t__tn_d_weeks_gom_short |>
  # date only
  mutate(trip_start_date_only =
           lubridate::date(TRIP_START_DATE)) |>
  # combine start date and time for t & tn
  mutate(
    # declarations
    trip_start_date_time_tn =
      # convert to char
      paste(trip_start_date_only,
            TRIP_START_TIME.tn) |>
      # convert to date_time
      parse_date_time("ymd HM")
  ) |>
  mutate(
    # logbooks
    trip_start_date_time_t =
      # convert to char
      paste(trip_start_date_only,
            TRIP_START_TIME.t) |>
      # convert to date_time
      parse_date_time("ymd HM")
  )

# str(v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts)

# 1: There was 1 warning in `mutate()`.
# ℹ In argument: `trip_start_date_time_tn = parse_date_time(...)`.
# Caused by warning:
# !  2491 failed to parse. 
# 2: There was 1 warning in `mutate()`.
# ℹ In argument: `trip_start_date_time_t = parse_date_time(...)`.
# Caused by warning:
# !  29103 failed to parse. 

# v_p__t__tn_d_weeks_gom_short |>
#   mutate(trip_start_date_only =
#            lubridate::date(TRIP_START_DATE)) |>
#   filter(
#     as.integer(trip_start_date_only) %in% c(2491, 29103) |
#       as.character(TRIP_START_TIME.t) %in% c("2491", "29103") |
#       as.character(TRIP_START_TIME.tn) %in% c("2491", "29103")
#   ) |>
#   dim()
# 0

# v_p__t__tn_d_weeks_gom_short_dt |> 
#   head() |> 
#   select(contains("start")) |> 
#   glimpse()

# 1) all compliant if there are no reports ----
v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts <-
  v_p__t__tn_d_weeks_gom_short_dt |>
  group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID) |>
  mutate(compl_no_reps_y =
           case_when(is.na(rep_type.t) &
                       is.na(rep_type.tn) ~ "yes",
                     .default = "no")) |>
  ungroup()

# str(v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts)

# check
dim(v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts)
# [1] 75403    40

v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts |> 
  select(PERMIT_VESSEL_ID, compl_no_reps_y) |> 
  distinct() |> 
  count(compl_no_reps_y)
#   compl_no_reps_y     n
#   <chr>          <int>
# 1 no               883
# 2 yes              468
# 883 + 468 = 1351 ok

# if all are compliant - the whole year is compl
# View(v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts)

## same by week ----
v_p__t__tn_d_weeks_gom_short_dt |> 
  filter(is.na(date_y_m)) |> 
  dim()
# [1] 468  36
  # filter(is.na(WEEK_OF_YEAR)) |> 
  # dim()
# [1] 468  36

# check
v_p__t__tn_d_weeks_gom_short_compl_y_no_rprts |>
  filter(is.na(WEEK_OF_YEAR)) |>
  select(PERMIT_VESSEL_ID, compl_no_reps_y) |>
  distinct() |>
  count(compl_no_reps_y)
# [1] 468  36
# 1 yes              468 All are a yes, no reports

tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts <-
  v_p__t__tn_d_weeks_gom_short_dt |>
  group_by(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID,
           date_y_m,
           WEEK_OF_YEAR) |>
  mutate(compl_no_reps_w =
           case_when(is.na(rep_type.t) &
                       is.na(rep_type.tn) ~ "yes",
                     .default = "no")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts: 6.16 sec elapsed

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts |> 
  select(PERMIT_VESSEL_ID, VESSEL_VESSEL_ID,
           date_y_m,
           WEEK_OF_YEAR,
         compl_no_reps_w) |>
  distinct() |>
  count(date_y_m,
        compl_no_reps_w) |> 
  arrange(desc(compl_no_reps_w)) |> 
  # head()
# 1 NA        yes               468
# 2 Sep 1970  no                  1
# 3 Oct 2020  no                  1
  tail()
# 1 Oct 2022  no               1755
# 2 Nov 2022  no               1010
# 3 Dec 2022  no               1031

# 2) if all declarations for a vessel have a matched logbook - compliant ----

# By week
## a) match logbooks and declarations ----

# There should be a logbook for every declaration of a charter or headboat intending to fish.
# decl trip start < or > 1h logbooks trip start

# at least one pair of matching declarations per week
# non compl if there is a logbook w/o a decl
## decl trip start < or > 1h logbooks trip start ----

# already matched by day, check the time, by vessel
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched")
# v_p__t__tn_d_weeks_gom_short_matched <-
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts |>
  group_by(VESSEL_VESSEL_ID, PERMIT_VESSEL_ID) |>
  # convert time to a Date format
  mutate(
    TRIP_START_TIME_t_hm =
      parse_date_time(TRIP_START_TIME.t, "HM"),
    TRIP_START_TIME_tn_hm =
      parse_date_time(TRIP_START_TIME.tn, "HM")
  ) |>
  # count the difference between start times t and tn
  mutate(time_diff1 = abs(TRIP_START_TIME_t_hm - (TRIP_START_TIME_tn_hm))) |>
  # less than an hour difference between trip and trip notif start time
  mutate(matched_reports =
           case_when(time_diff1 < 3600 ~ "matched",
                     .default = "not_matched"
           )) |>
  distinct() |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched: 5.42 sec elapsed

dim(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched)
# [1] 75403    44

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched |>
  select(PERMIT_VESSEL_ID, matched_reports) |>
  distinct() |>
  count(matched_reports)
# 1 matched           595
# 2 not_matched      1300

length(unique(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched$PERMIT_VESSEL_ID))
# [1] 1351

# View(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched)

# no grouping?, even one not matched will make the vessel not compliant

## strict compl vessels per week ----
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(matched_compl =
           # all logbooks and decl should match for any given week
           case_when(all(matched_reports == "matched") ~
                       "yes",
                     .default = "no")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w: 5.89 sec elapsed

View(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w)

# check compliant_fishing_month
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w |>
  count(
    # VESSEL_VESSEL_ID,
    #     PERMIT_VESSEL_ID,
    # WEEK_OF_YEAR,
    date_y_m,
    INTENDED_FISHING_FLAG,
    matched_compl) |>
  dim()
# [1] 68  4

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         matched_reports,
         compl_no_reps_w,
         matched_compl) |>
  count(date_y_m,
        compl_no_reps_w,
        
        matched_compl) |>
  arrange(desc(matched_compl)) |> 
  # head()
#   date_y_m  compl_no_reps_w matched_compl     n
#   <yearmon> <chr>           <chr>         <int>
# 1 Jan 2022  no              yes             341
# 2 Feb 2022  no              yes             598
  tail()
# 2 Dec 2022  no              no             2025
# 3 Feb 2023  no              no                1
# 4 Jun 2023  no              no                1
# 5 Nov 2023  no              no                1
# 6 NA        yes             no              468

# strictly compliant by week ----
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(compl_w =
           case_when(
             compl_no_reps_w == "yes" |
               matched_compl == "yes" ~ "yes",
             .default = "no"
           )) |> 
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict: 5.91 sec elapsed

dim(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict)
# [1] 75403    46

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict |> 
  select(PERMIT_VESSEL_ID, compl_w) |> 
  distinct() |> 
  count(compl_w)
# 1 no        832
# 2 yes      1039

## same by year ----
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID) |>
  mutate(compl_y =
           case_when(
             compl_no_reps_w == "yes" |
               matched_compl == "yes" ~ "yes",
             .default = "no"
           )) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict: 0.53 sec elapsed

# View(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict)

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |> 
  select(PERMIT_VESSEL_ID,
         compl_y) |> 
  distinct() |> 
  count(compl_y)
# 1 no        832
# 2 yes      1039
# TODO: change, more than 1351 total vsls
# TODO: why the same as week?

# Not strictly, but are compliant ----
# 1) There are only not fishing intended declarations per week
# 2) a duplicate declaration for the same trip, one has a logbook
# 3) a logb and no decl (err, but is compliant in FHIER?)
# TODO: check in FHIER
# 4) not compliant but overridden
# everything that was overridden is compliant (the whole week)

# 1) There are only not fishing intended declarations per week
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y__no_fish")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w__no_fish <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w_strict |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(not_fish_compl =
           case_when(
             # not strictly compliant
             matched_compl == "no" &
               # all reports are not fishing declarations in the week
               all(INTENDED_FISHING_FLAG == "N" &
                     !is.na(rep_type.tn)
                   ) ~ "yes",
             .default = "no"
           )) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w__no_fish: 5.41 sec elapsed

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w__no_fish |> 
  select(PERMIT_VESSEL_ID,
         date_y_m,
         matched_reports,
         compl_no_reps_w,
         matched_compl,
         not_fish_compl) |>
  distinct() |> 
  count(not_fish_compl)
# no              9754
# yes              409


# print_df_names(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w__no_fish)
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w__no_fish |>
  filter(PERMIT_VESSEL_ID == "FL4459PW" &
           WEEK_OF_YEAR == "13" &
           date_y_m == "Mar 2022") |>
#     not_fish_compl INTENDED_FISHING_FLAG     n
# 1 no             N                        16
# 2 no             Y                       102
# 3 no             NA                        4
# 4 yes            N                         2
  select(
    matched_reports,
    matched_compl,
    compl_no_reps_w,
    not_fish_compl,
    INTENDED_FISHING_FLAG
  ) |>
  add_count(not_fish_compl,
        INTENDED_FISHING_FLAG) |> 
  glimpse()
# $ matched_reports       <chr> "matched", "not_matched", "not_matched"
# $ matched_compl         <chr> "no", "no", "no"
# $ compl_y               <chr> "no", "no", "no"
# $ not_fish_compl        <chr> "no", "no", "no"
# $ INTENDED_FISHING_FLAG <chr> "Y", "N", "N"
# $ n                     <int> 1, 2, 2
  
#   not_fish_compl INTENDED_FISHING_FLAG     n
# 1 no             N                        16
# 2 no             Y                       102
# 3 no             NA                        4
# 4 yes            N                         2

## 2) a duplicate declaration for the same trip, one has a logbook ----

### a) add before and after 1 hour intervals ----

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |>
  mutate(
    after_interval =
      lubridate::as.interval(3600, trip_start_date_time_tn),
    # flip interval to get lower date first in the interval
    before_interval = int_flip(as.interval(-3600, trip_start_date_time_tn))
  )

str(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont)

tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont_tn_dup")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont_tn_dup <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont |>
  # group by tn
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           NOTIFICATION_TYPE_IDs,
           # not_fish_compl
           rep_type.tn) |>
  mutate(
    decl_dup =
      all(trip_start_date_time_tn %within% before_interval  |
          trip_start_date_time_tn %within% after_interval)
  )
toc()
  
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_ont_tn_dup |>
  # filter(date_y_m == "Feb 2022") |>
  # filter(PERMIT_VESSEL_ID == "1093374") |>
  View()

# HERE
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |>
  # select(-c(UE.tn,
  #           DE.tn,
  #           UC.tn,
  #           DC.tn)) |>
  # group by vessel and tn date and time (<>1), having cnt(decl) > 1
  # TRIP_START_TIME_tn_hm <> 1h
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           # TRIP_START_TIME.tn,
           # TRIP_END_TIME.tn,
           WEEK_OF_YEAR,
           date_y_m,
           NOTIFICATION_TYPE_IDs,
           # not_fish_compl
           rep_type.tn) |>
  mutate(
    st_times = paste(unique(TRIP_START_TIME.tn),
                     collapse = ", "),
    end_times = paste(unique(TRIP_END_TIME.tn),
                      collapse = ", ")
  ) |> 
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d: 1.51 sec elapsed

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d |>
  filter(PERMIT_VESSEL_ID == "FL4459PW" &
           WEEK_OF_YEAR == "13" &
           date_y_m == "Mar 2022") |>
  View()
 
print_df_names(v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict)
 
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |>
  select(TRIP_START_DATE, TRIP_START_TIME.tn) |>
  distinct() |>
  mutate(date_time = 
           # ymd_hms(
           paste0(TRIP_START_DATE, TRIP_START_TIME.tn)
         # )
         ) |>
  head()

## count amount of declarations ----
tic("v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_cnt_d")
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_cnt_d <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |>
  select(-c(UE.tn,
            DE.tn,
            UC.tn,
            DC.tn)) |>
  glimpse()
  # group by vessel and tn date and time (<>1),
  # having cnt(decl) > 1
  # TRIP_START_TIME_tn_hm <> 1h
  mutate(hour_interval_plus =
           ymd_hm(paste0(TRIP_START_DATE, TRIP_START_TIME.tn))
  ) |> View()
# 
#            lubridate::minute(TRIP_START_TIME_tn_hm) + 60*60,
#          hour_interval_min =
#            lubridate::minute(TRIP_START_TIME_tn_hm) - 60*60,
#          hour_interval =
#            hour_interval_plus + hour_interval_min) |>
  group_by(PERMIT_VESSEL_ID,
           TRIP_START_DATE,
           TRIP_START_TIME.tn,
           hour_interval) |>
  mutate(cnts = n()) |>
  ungroup()
toc()

v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_cnt_d |>
  filter(PERMIT_VESSEL_ID == "546932" &
           date_y_m == "Feb 2022") |>
  View()

# v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_cnt_d |> 
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d_1 <-
  v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict_dup_d |> 
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           TRIP_START_TIME.tn,
           TRIP_END_TIME.tn,
           WEEK_OF_YEAR,
           date_y_m,
           NOTIFICATION_TYPE_IDs,
           # not_fish_compl
           rep_type.tn) |>
  mutate(
    st_times = paste(unique(TRIP_START_TIME.tn),
                     collapse = ", "),
    end_times = paste(unique(TRIP_END_TIME.tn),
                      collapse = ", ")
  ) |> 
  ungroup()
toc()


# old part unchanged ----
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_w |>
  count(INTENDED_FISHING_FLAG)
# INTENDED_FISHING_FLAG     n
# 1 N                      4070
# 2 Y                     68118
# 3 NA                     3215
         
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         matched_reports,
         INTENDED_FISHING_FLAG,
         matched_compl) |>
  distinct() |>
  group_by(date_y_m,
           matched_reports,
           INTENDED_FISHING_FLAG,
           matched_compl) |>
  mutate(cnt_vsls = n_distinct(PERMIT_VESSEL_ID)) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  glimpse()
# disregard "not_matched & yes", that means there are more than 1 decl

### a good example of 2 decl per week with compliant ----
v_p__t__tn_d_weeks_gom_short_compl_w_no_rprts_matched_y_strict |>
  filter(date_y_m == "Feb 2022") |>
  filter(PERMIT_VESSEL_ID == "1093374") |>
  select(
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
  distinct() |>
  View()

### count vessel per month by matched_compl ----
v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         matched_compl) |>
  distinct() |>
  group_by(date_y_m,
           WEEK_OF_YEAR,
           matched_compl) |>
  mutate(cnt_vsls =
           case_when(
             any(matched_compl == "yes") ~
               n_distinct(PERMIT_VESSEL_ID),
             .default = 0
           )) |>
  select(-PERMIT_VESSEL_ID) |>
  distinct()
# disregard "not_matched & yes", that means there are more than 1 decl

glimpse(v_p__t__tn_d_weeks_gom_short_matched_compl_w_cnt_vsls_w)

# check vessel cnt by matched compl in Mar
v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  select(PERMIT_VESSEL_ID,
         date_y_m,
         WEEK_OF_YEAR,
         matched_compl) |>
  distinct() |>
  group_by(date_y_m,
           WEEK_OF_YEAR,
           matched_compl) |>
  filter(date_y_m == 'Mar 2022') |>
  subset(PERMIT_VESSEL_ID %in% PERMIT_VESSEL_ID[matched_compl == 'yes']) |>
  mutate(cnt_c1 = n_distinct(PERMIT_VESSEL_ID)) |>
  ungroup() |>
  select(-PERMIT_VESSEL_ID) |>
  distinct() |>
  View()

## no matched declarations, but compliant? ----
# There should be a logbook for every declaration of a charter or a headboat intending to fish.

## 2) There are only a not matched not fishing intended declarations per week ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_2")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(not_fish_compl =
           case_when(
             matched_compl == "no" &
               # all reports are not fishing declarations
               all(INTENDED_FISHING_FLAG == "N" &
                     !is.na(rep_type.tn)) ~ "yes",
             .default = "no"
           )) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_2: 8.93 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  count(not_fish_compl)
# 1 no             73009
# 2 yes             2394

v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  # filter(not_fish_compl == "yes") |>
  filter(PERMIT_VESSEL_ID == "FL4459PW") |>
  arrange(WEEK_OF_YEAR) |>
  glimpse()

## 3) a week with no reports of any kind (compl) ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_3")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_2 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
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
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3: 7.41 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  count(no_rep_compl)
# 1 no           74935
# 2 yes            468

#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
  # View()
#
# > v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > v_p__t__tn_d_weeks |>
#   filter(PERMIT_VESSEL_ID == "FL3627NN") |>
#   View()
# > trips_info_2022 |>
#   filter(VESSEL_ID == "330659") |>
#   View()
# 0
# > trips_notifications_2022 |>
#   filter(VESSEL_ID == "330659") |>
#   View()
# 0

## 4) a week with a logb and no decl (err, is non-compliant) ----
# TODO: check in FHIER

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
#   filter(VESSEL_VESSEL_ID == 72359,
#          PERMIT_VESSEL_ID == "933533")

tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_4")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_3 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
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
  mutate(no_decl_compl =
           case_when(all(no_decl_compl_0 == "yes") ~ "yes",
                     .default = "no")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_4: 11.72 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  count(no_decl_compl_0)
# 1 no              73738
# 2 yes              1665
  # count(no_decl_compl)
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
#       filter(no_decl_compl == "yes") |>
#       select(PERMIT_VESSEL_ID) |>
#       distinct() %>%
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
#   filter(PERMIT_VESSEL_ID %in%
#            v_yes[[2]]$PERMIT_VESSEL_ID) |>
#   View()

# FL3326MC_4 <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
#   filter(PERMIT_VESSEL_ID == "FL3326MC") |>
#   select(-no_decl_compl_0)
#
# FL3326MC_4a <-
#   v_p__t__tn_d_weeks_gom_short_matched_compl_w_4a |>
#   filter(PERMIT_VESSEL_ID == "FL3326MC")
#
# library(diffdf)
# diffdf(FL3326MC_4, FL3326MC_4a)
# All rows are shown in table below

  #    VARIABLE     ..ROWNUMBER..  BASE  COMPARE
  #  no_decl_compl       37         no     yes

# glimpse(FL4482RC_4[37,])
# glimpse(FL4482RC_4a[37,])

v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  filter(WEEK_OF_YEAR == 26,
         PERMIT_VESSEL_ID == "FL3326MC") |>
  glimpse()
# $ cnt_t                 <int> 1, 1
# $ cnt_tn                <int> 1, 1
# 2 entries, 1 has t, another has a tn! should be not compl

## 5) no logbook, but a decl is a "no fish" - compl ----
tic("v_p__t__tn_d_weeks_gom_short_matched_compl_w_5")
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_4 |>
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(
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
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 13.05 sec elapsed
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 3032.68 sec elapsed rowwise
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5: 7.04 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  count(no_lgb_compl)
# 1 no           75060
# 2 yes            343

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  arrange(desc(no_lgb_compl)) |>
  glimpse()

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5 |>
  arrange(desc(no_lgb_compl)) |>
  filter(no_decl_compl == "yes" & no_lgb_compl == "yes") |>
  # count(n_distinct(PERMIT_VESSEL_ID))
  glimpse()
# 0

## 6) not compliant but overridden ----
# everything that was overridden is compliant (the whole week)
compl_err_db_data_short <-
  compl_err_db_data |>
  select(
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
  distinct()

override_join_by =
  join_by(
    VESSEL_VESSEL_ID == safis_vessel_id,
    PERMIT_VESSEL_ID == vessel_official_nbr,
    WEEK_OF_YEAR == comp_week,
    YEAR == comp_year
  )

compl_err_db_data_short_overr <-
  compl_err_db_data_short |>
  filter(is_comp_override == 1)

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
#     select(comp_error_type_cd) |>
#     distinct()
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
#   filter(comp_error_type_cd %in% c("DECL_NO_TRIP",
#                                    "NO_TRIP_FOUND",
#                                    "TRIP_NO_DECL")) |>
#   View()

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr |>
#   select(srfh_for_hire_type_id) |>
#   distinct()
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
  group_by(VESSEL_VESSEL_ID,
           PERMIT_VESSEL_ID,
           WEEK_OF_YEAR,
           date_y_m) |>
  mutate(compl_w =
           case_when(!!all_not_compl_filter ~ "no",
                     .default = "yes")) |>
  ungroup()
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
  select(-any_of(rm_fields)) |>
  distinct()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short)
# [1] 77748    14
# [1] 22072    14

# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short |>
#   filter(compl_w == "no") |>
#   filter(comp_error_type_cd == "SUBMIT_AFTER_ARRIVAL") |>
#   View()
  # select(comp_error_type_cd) |>
  # distinct()
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
  mutate(compl_w_total =
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
  select(-c(
    comp_error_type_cd,
    comp_override_cmt,
    is_comp,
    is_comp_override,
    compl_w
  )) |>
  distinct()

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short)
# [1] 21551    10

## compliant per month ----
tic(
  "v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m"
)
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short |>
  group_by(PERMIT_VESSEL_ID, date_y_m) |>
  mutate(compl_per_m_w = list(na.omit(unique(compl_w_total))),
         compl_m_len = lengths(compl_per_m_w)) |>
  mutate(compl_m = case_when(compl_m_len > 1 ~ "no",
                             compl_per_m_w == "no" ~ "no",
                             .default = "yes")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m: 2.47 sec elapsed

dim(v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m)
# [1] 21551    13 distinct

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m |>
  filter(PERMIT_VESSEL_ID == "FL4463MX" &
           date_y_m == "Jun 2022") |>
  arrange(WEEK_OF_YEAR) |>
  distinct() |>
  glimpse()
# $ WEEK_OF_YEAR     <dbl> 22, 23, 24, 25, 26
# $ compl_w          <chr> "no", "no", "yes", "yes", "yes"
# $ compl_m          <chr> "no", "no", "no", "no", "no"

## GOM total compliance per year ----
tic(
  "v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y"
)
v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y <-
  v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_m |>
  group_by(PERMIT_VESSEL_ID) |>
  mutate(compl_per_y_w = list(na.omit(unique(compl_w_total))),
         compl_y_len = lengths(compl_per_y_w)) |>
  mutate(compl_y = case_when(compl_y_len > 1 ~ "no",
                             compl_per_y_w == "no" ~ "no",
                             .default = "yes")) |>
  ungroup()
toc()
# v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y: 0.72 sec elapsed

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y |>
  count(compl_y)
# 1 no      11645
# 2 yes      9906

v_p__t__tn_d_weeks_gom_short_matched_compl_w_5_overr_total_comp1_short_compl_w_short_y |>
  select(PERMIT_VESSEL_ID, compl_y) |>
  distinct() |>
  count(compl_y)
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

