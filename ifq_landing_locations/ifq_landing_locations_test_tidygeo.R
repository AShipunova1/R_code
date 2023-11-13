tic("geocode6")
lat_longs <- input_data_raw %>%
  head() |>
  geocode(
    street = "STREET",
    city = "CITY",
    state = "STATE",
    postalcode = "ZIP"
  )
# ,
  # country = NULL,
  # lat = "lat",
  # long = "long",
  # return_input = TRUE,
  # limit = 1,
  # return_addresses = NULL,
# 
#     addr,
# method = 'arcgis',
          # method = 'osm',
          # lat = latitude ,
          # long = longitude)
toc()

# View(lat_longs)
# print_df_names(input_data_convert_dms)

lat_longs |>
  left_join(
    input_data_convert_dms,
    by = join_by(
      NYEAR == USER_NYEAR,
      FK_LANDING_LOCATION_ID == USER_FK_LANDING_LOCATION_ID,
      UseCount == USER_UseCount
    )
  ) |>
  select(NYEAR, FK_LANDING_LOCATION_ID, LATITUDE, LONGITUDE,
         lat, long, Y, X.y, OID_, Place_addr) |> 
  View()
# diff in 3rd digit

## test with another method ----

tic("geocode6_esri")
lat_longs_esri <- input_data_raw %>%
  mutate(address = paste(STREET,
                         CITY,
                         STATE,
                         ZIP,
                         sep = ", ")) |>
  head() |>
  geocode(address = address,
          return_addresses = TRUE,
          method = 'arcgis')
toc()
# 1.5 sec elapsed

lat_longs_esri |>
  left_join(
    input_data_convert_dms,
    by = join_by(
      NYEAR == USER_NYEAR,
      FK_LANDING_LOCATION_ID == USER_FK_LANDING_LOCATION_ID,
      UseCount == USER_UseCount
    )
  ) |>
  select(NYEAR, FK_LANDING_LOCATION_ID, LATITUDE, LONGITUDE,
         lat, long, Y, X.y, OID_, Place_addr) |> 
  View()
# the diff in 4-5 digit

