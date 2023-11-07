# fishing_effort_location_by_permit_and_end_port

# effort_vsl_cropped_cnt_l |> View()
# coord_data_2022_short_good_sf_crop_big_df_in_metricks_list |> View()

# Split the data frame into multiple sub-data frames based on the 'permit_region' column.

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list <-
  split(
    # Data frame to be split
    coord_data_2022_short_good_sf_crop_big_df_in_metricks,

    # Split based on the 'permit_region' column
    as.factor(
      coord_data_2022_short_good_sf_crop_big_df_in_metricks$permit_region
    )
  )

# Use the 'map' function to apply the 'dim' function to each element in the list.
purrr::map(
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list,
  dim
)

# $gom_and_dual
# [1] 46772    73
#
# $sa_only
# [1] 44066    73

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  dplyr::select(activity_type_name) |>
  dplyr::distinct()
# 1    TRIP WITH EFFORT
# 2                <NA>
# 3 TRIP UNABLE TO FISH

coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  dplyr::select(notif_landing_location_state) |>
  dplyr::distinct()
# <NA>
# AL
# FL
# TX
# LA

sa_end_port <-
  coord_data_2022_short_good_sf_crop_big_df_in_metricks_list$sa_only |>
  dplyr::select(
    trip_id,
    vessel_id,
    vessel_official_nbr,
    end_port_state,
    notif_landing_location_state
  ) |>
  dplyr::distinct()

dim(sa_end_port)
# [1] 44009     5

sa_end_port_cnt_vessels <-
  sa_end_port |>
  dplyr::select(vessel_official_nbr, end_port_state) |>
  dplyr::distinct() |>
  filter(end_port_state %in% sa_state_abb$state_abb) |>
  count(end_port_state)
#   END_PORT_STATE   n
# 1             FL 565
# 2             GA  25
# 3             NC 217
# 4             SC 142

# This code snippet performs operations on the `sa_end_port` data frame to count
# the occurrences of unique values in the 'notif_landing_location_state' column
# that match the values in the 'state_abb' column of the 'sa_state_abb' data frame.

# The `sa_end_port` data frame is piped into the 'select' function to keep only
# the 'vessel_official_nbr' and 'notif_landing_location_state' columns.
sa_end_port |>
  dplyr::select(vessel_official_nbr, notif_landing_location_state) |>

# The 'filter' function is used to retain only the rows where
# 'notif_landing_location_state' values are present in the 'state_abb' column of
# the 'sa_state_abb' data frame.
  filter(notif_landing_location_state %in% sa_state_abb$state_abb) |>

# The 'count' function calculates the number of occurrences of each unique
# 'notif_landing_location_state' value in the resulting data frame, returning the
# count of occurrences for each state.
  count(notif_landing_location_state)
# 1                           FL 688

# This code snippet calculates the count of unique end_port_state values in the
# sa_end_port data frame that match the values in the 'state_abb' column of the
# 'sa_state_abb' data frame.

# The `sa_end_port` data frame is piped into the 'select' function to retain only
# the 'trip_id' and 'end_port_state' columns.
sa_end_port_cnt_trips <- sa_end_port |>
  dplyr::select(trip_id, end_port_state) |>

# The 'distinct' function is used to retain unique rows based on the combination
# of 'trip_id' and 'end_port_state'.
  dplyr::distinct() |>

# The 'filter' function is applied to keep only rows where 'end_port_state' is
# present in the 'state_abb' column of the 'sa_state_abb' data frame.
  filter(end_port_state %in% sa_state_abb$state_abb) |>

# Finally, the 'count' function calculates the number of occurrences of each
# unique 'end_port_state' value in the resulting data frame, returning the count
# of trips for each state.
  count(end_port_state)

#   END_PORT_STATE     n
# FL 27332
# GA   351
# NC  7503
# SC  5128
