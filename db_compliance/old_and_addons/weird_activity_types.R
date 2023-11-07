## check activity type ----
v_p__t__tn_d_weeks_gom |>
  count(ACTIVITY_TYPE, INTENDED_FISHING_FLAG)
#   ACTIVITY_TYPE     n
#           <dbl> <int>
# 1             0 45834
# 2             3     1
# 3            80   488
# 4            81     2
# 5            NA 29199
# 0, 'TRIP WITH EFFORT',
# 80, 'TRIP UNABLE TO FISH',
# 81, 'TRIP NO INTENTION OF FISHING'

#    ACTIVITY_TYPE INTENDED_FISHING_FLAG     n
#            <dbl> <chr>                 <int>
#  1             0 N                       488
#  2             0 Y                     43375
#  3             0 NA                     1971
#  4             3 Y                         1
#  5            80 N                        21
#  6            80 Y                       425
#  7            80 NA                       42
#  8            81 N                         1
#  9            81 Y                         1
# 10            NA N                      3562
# 11            NA Y                     24375
# 12            NA NA                     1262

v_p__t__tn_d_weeks_gom |>
  dplyr::filter(ACTIVITY_TYPE == "3") |>
  # head(2) |>
  # dplyr::select(all_of(starts_with("UE"))) |>
  glimpse()
# $ UE.t  <chr> "KCSPORTFISHING                "
# $ UE.tn <chr> "KCSPORTFISHING"
# $ VESSEL_VESSEL_ID            <dbl> 328032
# $ PERMIT_VESSEL_ID            <chr> "FL9452SM"

v_p__t__tn_d_weeks_gom |>
  dplyr::filter(ACTIVITY_TYPE == "81") |>
  dplyr::select(PERMIT_VESSEL_ID,
         ACTIVITY_TYPE,
         INTENDED_FISHING_FLAG,
         all_of(starts_with("rep_type"))) |>
  glimpse()
# $ PERMIT_VESSEL_ID      <chr> "1114447", "FL6430PK"
# $ ACTIVITY_TYPE         <dbl> 81, 81
# $ INTENDED_FISHING_FLAG <chr> "Y", "N"
# $ rep_type.t            <chr> "trips", "trips"
# $ rep_type.tn           <chr> "trips_notif", "trips_notif"

# TODO: check for INTENDED_FISHING_FLAG again
# check vessels_with_weird_activity ----
vessels_with_weird_activity <-
  c("FL9452SM",
    "NJ2232GM",
    "1199007",
    "1036367",
    "1067284",
    "1021417",
    "926746")

length(vessels_with_weird_activity)
# 7

vessels_permits_2022_r_act <-
  vessels_permits_2022_r |>
  dplyr::filter(PERMIT_VESSEL_ID %in% vessels_with_weird_activity)

# vessels_permits_2022_r_act

trips_act <-
  t_d_w |>
  dplyr::filter(
    VESSEL_ID %in% vessels_permits_2022_r_act$VESSEL_VESSEL_ID,
    ACTIVITY_TYPE %in% c(8, 2, 3)
  ) |>
  dplyr::select(-any_of(t_names_to_rm))

dim(trips_act)
# [1] 18 21

dim(vessels_permits_2022_r_act)
# [1] 42 52

weird_activity_types_info_by1 <-
  join_by(
    VESSEL_VESSEL_ID == VESSEL_ID,
    overlaps(
      x$EFFECTIVE_DATE,
      x$END_DATE,
      y$TRIP_START_DATE,
      y$TRIP_END_DATE,
      bounds = "[)"
    ))
    
weird_activity_types_info_by2 <-
  join_by(
    VESSEL_VESSEL_ID == VESSEL_ID,
    overlaps(
      x$EFFECTIVE_DATE,
      x$EXPIRATION_DATE,
      y$TRIP_START_DATE,
      y$TRIP_END_DATE,
      bounds = "[)"
    )
  )

weird_activity_types_info1 <-
  inner_join(
  vessels_permits_2022_r_act,
  trips_act,
  weird_activity_types_info_by1,
  relationship = "many-to-many",
  suffix = c(".v_p", ".t")
) |> 
  dplyr::distinct() |> 
  dplyr::arrange(PERMIT_VESSEL_ID)

dim(weird_activity_types_info1)
# [1] 10 72

weird_activity_types_info2 <-
  inner_join(
  vessels_permits_2022_r_act,
  trips_act,
  weird_activity_types_info_by2,
  relationship = "many-to-many",
  suffix = c(".v_p", ".t")
) |> 
  dplyr::distinct() |> 
  dplyr::arrange(PERMIT_VESSEL_ID)

dim(weird_activity_types_info2)
# [1] 22 72

weird_activity_types_info12 <-
  rbind(weird_activity_types_info1,
        weird_activity_types_info2)

dim(weird_activity_types_info12)
# [1] 32 72

# dim(weird_activity_types_info)
# 34 72
# 10 - inner join

# weird_activity_types_info |> 
weird_activity_types_info12 |> 
  dplyr::select(PERMIT_VESSEL_ID) |> 
  dplyr::distinct() |> 
  head(10)
# 8 (7 + NA)
# 5
#   PERMIT_VESSEL_ID
# 1          1021417
# 2          1036367
# 3          1067284
# 4          1199007
# 5           926746
# 6         FL9452SM
# 7         NJ2232GM
# 8             <NA>

# 926746 Permit
# 127190 vessel_id
trips_info_2022_int_dur |>
  dplyr::filter(VESSEL_ID == "127190") |>
  dplyr::select(TRIP_START_DATE,
         TRIP_END_DATE) |>
  dplyr::distinct() |>
  dplyr::arrange(TRIP_START_DATE) |> 
  View()

vessels_permits_2022_r_end_date_uid_short_mm_w_y_interv |> 
  dplyr::filter(PERMIT_VESSEL_ID == "926746") |> 
  View()
# 2021-12-31 19:00:00 EST--2022-01-30 23:00:00 EST

rm_cols <-
  c("TM_ORDER",
    "TM_TOP_ORDER",
    "GRP_PRIOR_OWNER",
    "APPLICATION_ID",
    "VESSEL_ALT_NUM",
    "TOP_NAME",
    "COUNTY_CODE",
    "STATE_CODE",
    "ENTRY_DATE",
    "SUPPLIER_VESSEL_ID",
    "PORT_CODE",
    "HULL_ID_NBR",
    "COAST_GUARD_NBR",
    "STATE_REG_NBR",
    "PASSENGER_CAPACITY",
    "VESSEL_TYPE",
    "YEAR_BUILT",
    "UPDATE_DATE",
    "PRIMARY_GEAR",
    "OWNER_ID",
    "EVENT_ID.v_p",
    "UPDATED_FLAG",
    "SERO_OFFICIAL_NUMBER",
    "EVENT_ID.t",
    "TRIP_TIME_ZONE",
    "trip_int",
    "TRIP_END_TIME",
    "TRIP_START_TIME",
    "TRIP_END_week_num",
    "TRIP_END_y",
    "TRIP_END_m"
)

weird_activity_types_info_short12 <-
  weird_activity_types_info12 |> 
  dplyr::select(-any_of(rm_cols)) |> 
  dplyr::distinct()

dim(weird_activity_types_info_short12)
  # dim()
  # 34 41
  # dplyr::filter(!is.na(TRIP_ID))
  # 23 41
  # 22

# weird_activity_types_info_short_act <-
#   weird_activity_types_info_short12 |> 
#   dplyr::filter(!is.na(ACTIVITY_TYPE))

# dim(weird_activity_types_info_short_act)
# 22

# all.equal(weird_activity_types_info_short_act,
#           weird_activity_types_info_short12)
# T

# weird_activity_types_info_short_no_trips <-
#   weird_activity_types_info_short |> 
#   dplyr::filter(PERMIT_VESSEL_ID %in% c("1199007",
#                                  "926746"))

# dim(weird_activity_types_info_short)
# [1] 34 41
# dim(weird_activity_types_info_short_no_trips)
# [1] 23 41
# dim(weird_activity_types_info_short_act)
# [1] 23 41

# weird_activity_types_info_short |> 
# weird_activity_types_info_short_act |> 
weird_activity_types_info_short12 |> 
  dplyr::select(PERMIT_VESSEL_ID) |>
  dplyr::distinct() |>
  head(10)
# 6

write_csv(
  weird_activity_types_info_short12,
  file.path(
    my_paths$outputs,
    current_project_name,
    "weird_activity_types_info.csv"
  )
)

weird_activity_types_info_short12 |>
  dplyr::select(PERMIT_VESSEL_ID,
         EFFECTIVE_DATE,
         EXPIRATION_DATE,
         TRIP_START_DATE,
         TRIP_END_DATE) |>
  dplyr::distinct() |>
  write_csv(
    file.path(
      my_paths$outputs,
      current_project_name,
      "weird_activity_types_info_short_intervals.csv"
    )
  )

    
