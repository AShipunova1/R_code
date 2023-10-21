# fishing_effort_location_by_permit_and_end_port

# effort_vsl_cropped_cnt_l |> View()
# coord_data_2022_short_good_sf_crop_big_df_in_metricks_list |> View()

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  select(ACTIVITY_TYPE_NAME) |>
  distinct()
# 1    TRIP WITH EFFORT
# 2                <NA>
# 3 TRIP UNABLE TO FISH

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  select(NOTIF_LANDING_LOCATION_STATE) |>
  distinct()
# <NA>
# AL
# FL
# TX
# LA

sa_end_port <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  select(
    TRIP_ID,
    VESSEL_ID,
    VESSEL_OFFICIAL_NBR,
    END_PORT_STATE,
    NOTIF_LANDING_LOCATION_STATE
  ) |>
  distinct()

dim(sa_end_port)
# [1] 44009     5

sa_end_port_cnt_vessels <-
  sa_end_port |>
  select(VESSEL_OFFICIAL_NBR, END_PORT_STATE) |>
  distinct() |>
  filter(END_PORT_STATE %in% sa_state_abb$state_abb) |>
  count(END_PORT_STATE)
#   END_PORT_STATE   n
# 1             FL 565
# 2             GA  25
# 3             NC 217
# 4             SC 142

sa_end_port |>
  select(VESSEL_OFFICIAL_NBR, NOTIF_LANDING_LOCATION_STATE) |>
  filter(NOTIF_LANDING_LOCATION_STATE %in% sa_state_abb$state_abb) |>
  count(NOTIF_LANDING_LOCATION_STATE)
# 1                           FL 688

sa_end_port_cnt_trips <-
  sa_end_port |>
  select(TRIP_ID, END_PORT_STATE) |>
  distinct() |>
  filter(END_PORT_STATE %in% sa_state_abb$state_abb) |>
  count(END_PORT_STATE)

#   END_PORT_STATE     n
# FL 27332
# GA   351
# NC  7503
# SC  5128
