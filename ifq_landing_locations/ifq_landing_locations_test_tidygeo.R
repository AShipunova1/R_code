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

# compare missing values with ARCgis result ----

# check arcgis_address vs my_address diff ----
# split to strit, city, state (FL in input vs Florida), zip and compare separately

input_data_convert_dms |>
  filter(!tolower(IN_Address) == tolower(arcgis_address)) |> 
  View()


in_address_sep <- 
  stringr::str_split(input_data_convert_dms$IN_Address, ",",
                     simplify = TRUE) |> 
  as.data.frame()


head(in_address_sep, 2)
# names(in_address_sep) <- c("in_street",
#                            "in_city",
#                            "in_state",
#                            "in_zip",
#                            "V5")

clean_addr <- 
  function(addr_str) {
    return(trimws(tolower(addr_str)))
  }

map(in_address_sep$V5, length) |> 
  head()
# 1

names(state.abb) <- state.name
names(state.name) <- state.abb
state.name["FL"]

tic("in_address_sep_clean")
in_address_sep_clean <- 
  in_address_sep |> 
  rowwise() |> 
  mutate(
    # v5_len = length(trimws(V5))
    in_city_sep = case_when(length(trimws(V5)) > 1 ~ clean_addr(V3),
                                .default = clean_addr(V2)),
    in_state_sep = case_when(length(trimws(V5)) > 1 ~ clean_addr(V4),
                                .default = clean_addr(V3)),
    in_zip_sep = case_when(length(trimws(V5)) > 1 ~ clean_addr(V5),
                                .default = clean_addr(V4))
  ) |> 
  ungroup() |> 
  rename(in_street_sep = V1) |> 
  select(-starts_with("V"))
toc()
# in_address_sep_clean: 8.24 sec elapsed

head(in_address_sep_clean, 2)

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

