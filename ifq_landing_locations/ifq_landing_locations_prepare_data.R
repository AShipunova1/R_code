# prepare data for plotting
# restore maximum information

# from arcGIS Pro ----
# clean ARCgis df
# In summary, the code takes the 'input_data_convert_dms_short' data frame, makes several modifications to it using the 'mutate' function, and creates three new columns: 'use_lat', 'use_lon', and 'use_addr'. These new columns are generated based on specific conditions or by choosing the first non-NA value from a set of columns using 'coalesce'. The resulting data frame is stored in 'input_data_convert_dms_short_clean'.

input_data_convert_dms_short_clean <-
  input_data_convert_dms_short |>
  mutate(
    X = case_when(X == 0 ~ NA,
    .default = X),
    Y = case_when(Y == 0 ~ NA,
    .default = Y
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

# test
input_data_convert_dms_short_clean |>
  filter(corrected_addr == "") |>
  glimpse()

input_data_convert_dms_short |>
  filter(X == 0) |>
  glimpse()
# $ OID_              <int> 194, 664, 695, 1368, 2511, 2898, 3122, 3157
# $ Place_addr        <chr> "", "", "", "", "", "", "", ""
# $ corrected_addr    <chr> "", "", "", "", "", "", "", ""
# $ corrected_lat     <dbl> NA, NA, NA, NA, NA, NA, NA, NA
# $ corrected_long    <dbl> NA, NA, NA, NA, NA, NA, NA, NA
# $ Y                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0
# $ converted_dms_lat <dbl> 28.03278, 28.03278, 28.03278, NA, NA, 28.03278, NA, NA
# $ X                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0
# $ converted_dms_lon <dbl> 97.39194, 97.39194, 97.39194, NA, NA, 97.39194, NA, NA
# $ USER_NYEAR        <int> 2015, 2016, 2018, 2014, 2020, 2020, 2013, 2015
# $ USER_UseCount     <int> 4, 1, 3, 136, 1, 3, 40, 28

# input_data_convert_dms_short_clean |>
#   filter(OID_ == 194) |>
#   glimpse()


# from tidygeo ----

## add missing coords to input_data_raw_nominatim_converted ----
no_addded_coords_filter <-
  rlang::quo(is.na(X) | is.na(Y))

input_data_raw_nominatim_converted_has_coords <-
  input_data_raw_nominatim_converted |>
  mutate(has_coords =
           case_when(!!no_addded_coords_filter ~ "no_xy",
                     .default = "yes_xy"))

### no coords ----
input_data_raw_nominatim_converted_no_coord <- 
  input_data_raw_nominatim_converted_has_coords |> 
  filter(has_coords == "no_xy")

# dim(input_data_raw_nominatim_converted_no_coord)
# Rows: 1,307
# Columns: 30

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
# [1] 1307   90

join_nominatim_n_arcgis <- 
  join_nominatim_n_arcgis |>
  remove_empty_cols() |>
  distinct()

dim(join_nominatim_n_arcgis)
# [1] 1307   77

#### no coords clean up ----

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

dim(join_nominatim_n_arcgis_add_coord)
# [1] 1307   82

#### "has coords" clean up ----
# add use_ columns to the "good" part
input_data_raw_nominatim_converted_coord <- 
  input_data_raw_nominatim_converted_has_coords |> 
  filter(has_coords == "yes_xy")

input_data_raw_nominatim_converted_coord |> dim()
# [1] 2111   30

# display_name
# [1] "13725, Tram Avenue, Bayou La Batre, Mobile County, Alabama, 36509, United States"
# [2] "Scenic Highway 98, Destin, Okaloosa County, Florida, 32541, United States"

input_data_raw_nominatim_converted_coord_clean <-
  input_data_raw_nominatim_converted_coord |>
  mutate(
    X = case_when(X == 0 ~ NA,
    .default = X),
    Y = case_when(Y == 0 ~ NA,
    .default = Y
  )
  ) |>
  mutate(
    use_lat =
      dplyr::coalesce(
        # corrected_lat,
                      Y,
                      converted_dms_lat),
    use_lon =
      dplyr::coalesce(
        # corrected_long,
                      X,
                      -abs(converted_dms_lon)),
    use_addr = paste(USER_STREET, USER_CITY, USER_STATE, USER_ZIP, sep = ", ")
  )

# test
# input_data_raw_nominatim_converted_coord_clean |>
#   filter(use_addr == "") |>
#   dim()
# 0

# input_data_raw_nominatim_converted_coord_clean |>
#   filter(X == 0) |>
#   dim()
# 0

### merge back ----
good_c_names <- names(input_data_raw_nominatim_converted_coord_clean)
bad_c_names <- names(join_nominatim_n_arcgis_add_coord)

setdiff(good_c_names, bad_c_names)

setdiff(bad_c_names, good_c_names)

rbind(input_data_raw_nominatim_converted_coord_clean)
input_data_raw_nominatim_converted_has_coords |> 
  input_data_raw_nominatim_converted_no_coord <- 

  filter()
join_nominatim_n_arcgis_add_coord