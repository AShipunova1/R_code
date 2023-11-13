# prepare data for plotting
# restore maximum information

# from arcGIS Pro ----
# from tidygeo ----

## add missing coords to input_data_raw_nominatim_converted ----
no_addded_coords_filter <-
  rlang::quo(is.na(X) | is.na(Y))

input_data_raw_nominatim_converted_no_coord1 <- 
  input_data_raw_nominatim_converted |> 
  filter(!!no_addded_coords_filter)

all.equal(input_data_raw_nominatim_converted_no_coord,
          input_data_raw_nominatim_converted_no_coord1)
# T
# dim(input_data_raw_nominatim_converted_no_coord)
# Rows: 1,307
# Columns: 29

input_data_raw_nominatim_converted_no_coord |> 
  select(-USER_NYEAR) |> 
  remove_empty_cols() |> 
  distinct() |> 
  dim()
# [1] 1062   15

input_data_raw_nominatim_converted_no_coord |> 
  select(USER_FK_LANDING_LOCATION_ID) |> 
  distinct() |> 
  dim()
# [1] 210   1

input_data_raw_nominatim_converted_no_coord[1,]$USER_FK_LANDING_LOCATION_ID 
   # int 1604

arcgisdata_coord_only <- 
  input_data |>
  select(-c(USER_NYEAR, OID_, USER_UseCount)) |>
  remove_empty_cols() |>
  distinct()
# dim(arcgisdata_coord_only)
# [1] 585  61

# arcgisdata_coord_only |>
#   filter(USER_FK_LANDING_LOCATION_ID == 1604) |>
#   remove_empty_cols() |>
#   distinct() |>
#   dim()
# [1]  1 56

join_nominatim_n_arcgis <-
  left_join(
    input_data_raw_nominatim_converted_no_coord,
    arcgisdata_coord_only,
    join_by(USER_FK_LANDING_LOCATION_ID),
    suffix = c("_osm", "_arc")
  )

dim(join_nominatim_n_arcgis)
# [1] 1307   89

join_nominatim_n_arcgis <- 
  join_nominatim_n_arcgis |>
  remove_empty_cols() |>
  distinct()
# [1] 1307   76

print_df_names(join_nominatim_n_arcgis)

join_nominatim_n_arcgis_add_coord <- 
  join_nominatim_n_arcgis |> 
  mutate(
    X = case_when(X_arc == 0 ~ NA,
    .default = X_arc),
    Y = case_when(Y_arc == 0 ~ NA,
    .default = Y_arc
  )
  ) |>
  mutate(
    use_lat =
      dplyr::coalesce(corrected_lat,
                      Y,
                      converted_dms_lat),
    use_lon =
      dplyr::coalesce(corrected_long,
                      X,
                      -abs(converted_dms_lon)),
    # can't use coalesce, bc corrected_addr can be ""
    use_addr =
      case_when(
        !is.na(corrected_addr) &
          !corrected_addr == "" ~ corrected_addr,
        .default = Place_addr
      )
  )

View(join_nominatim_n_arcgis_add_coord)


### merge back ----
input_data_raw_nominatim_converted |> 
  filter()
join_nominatim_n_arcgis_add_coord