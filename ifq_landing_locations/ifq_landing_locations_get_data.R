# get data for ifq_landing_locations ----
# 1) convert addresses from the original csv to coordinates with ARCgis
# 2) manually (google search) add corrected_addr,	corrected_lat and	corrected_long if ExInfo is not NA (where possible);
# or 
# 1a) use tidygeocoder;

# upload the arcGIS result to R ----
input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use_geocoded_ex.csv)")

# file.exists(input_data_file_path)

# 'header = TRUE' indicates that the first row of the CSV file contains column names.
input_data <-
  read.csv(
    input_data_file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "latin1"
  )

# problems(input_data)

dim(input_data)


# using tidygeocoder ----

raw_input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use.csv)")

input_data_raw <-
  read.csv(
    raw_input_data_file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "latin1"
  )

print_df_names(input_data_raw)
# [1] "NYEAR, FK_LANDING_LOCATION_ID, LATITUDE, LONGITUDE, STREET, CITY, STATE, ZIP, UseCount, X"
# 
# [1] 3418   10

get_lat_lon_by_addr_esri <-
  function(input_df) {
    input_data_raw_esri <- input_df %>%
      dplyr::mutate(my_address = paste(STREET,
                                    CITY,
                                    STATE,
                                    ZIP,
                                    sep = ", ")) |>
      tidygeocoder::geocode(address = my_address,
                            return_addresses = TRUE,
                            method = 'arcgis',
                            full_results = TRUE)
    return(input_data_raw_esri)
  }
# Passing 558 addresses to the ArcGIS single address geocoder
# [===========================================] 558/558 (100%) Elapsed:  4m Remaining:  0s
# geocode_esri: 268.83 sec elapsed

esri_rds_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\input_data_raw_esri.rds)")

input_data_raw_esri <-
  read_rds_or_run(esri_rds_file_path,
                  my_data = input_data_raw,
                  get_lat_lon_by_addr)
# 2023-11-10 run for input_data_raw_esri.rds: 262.95 sec elapsed

# dim(input_data_raw_esri)
# [1] 3418   14

get_lat_lon_by_addr_nominatim <-
  function(input_df) {
    input_data_raw_nominatim <- input_df %>%
      tidygeocoder::geocode(
        street = "STREET",
        city = "CITY",
        state = "STATE",
        postalcode = "ZIP",
        return_addresses = TRUE,
        full_results = TRUE
      )
    return(input_data_raw_nominatim)
  }

nominatim_rds_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\input_data_raw_nominatim.rds)")

input_data_raw_nominatim <-
  read_rds_or_run(nominatim_rds_file_path,
                  my_data = input_data_raw,
                  get_lat_lon_by_addr_nominatim)
# Passing 555 addresses to the Nominatim single address geocoder
# [===================================] 555/555 (100%) Elapsed: 10m Remaining:  0s

# 2023-11-13 run for input_data_raw_nominatim.rds: 573.09 sec elapsed

dim(input_data_raw_nominatim)
# [1] 3418   28

# rename tidygeo input ----
# to be the same as from ARCgis
# View(input_data_raw_nominatim)
input_data_raw_esri_renamed <-
  input_data_raw_esri |>
  select(-c(X, my_address)) |>
  rename(USER_NYEAR = NYEAR,
         USER_FK_LANDING_LOCATION_ID = FK_LANDING_LOCATION_ID,
         USER_LATITUDE = LATITUDE,
         USER_LONGITUDE = LONGITUDE,
         USER_UseCount = UseCount,
         IN_Address = address,
         Y = lat,
         X = long
         )
 
# print_df_names(input_data_raw_nominatim)
input_data_raw_nominatim_renamed <-
  input_data_raw_nominatim |>
  select(-c(X)) |>
  rename(USER_NYEAR = NYEAR,
         USER_FK_LANDING_LOCATION_ID = FK_LANDING_LOCATION_ID,
         USER_LATITUDE = LATITUDE,
         USER_LONGITUDE = LONGITUDE,
         USER_STREET = STREET,
         USER_CITY = CITY,
         USER_STATE = STATE,
         USER_ZIP = ZIP,
         USER_UseCount = UseCount,
         Y = lat,
         X = long
  )

# get shapes ----
south_east_coast_states <- c(
  "Alabama",
  # "Connecticut",
  # "Delaware",
  "Florida",
  "Georgia",
  "Louisiana",
  # "Maine",
  # "Maryland",
  # "Massachusetts",
  "Mississippi",
  # "New Hampshire",
  # "New Jersey",
  # "New York",
  "North Carolina",
  # "Pennsylvania",
  # "Rhode Island",
  "South Carolina",
  "Texas"
  # ,
  # "Virginia",
  # "Washington DC"
)

## us maps ----
# The code loads U.S. state boundary shapefile data using the 'tigris' package, and the resulting spatial data is stored in the 'us_s_shp' variable as a simple feature (sf) object. The progress bar is disabled during the data loading process.
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.

us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

## Rows are retained if the 'NAME' column (state name) matches any of the values in 'states_sa'.
south_states_shp <-
  us_s_shp |>
  filter(NAME %in% south_east_coast_states)

# View(south_states_shp)

# str(south_east_coast_states)

## read in GOM shp ----
## Create a file path using 'file.path' by combining elements from 'my_paths' and specifying a shapefile path.
GOM_400fm_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOM_400fm\GOM_400fm.shp)")

# file.exists(GOM_400fm_path)
# T

## Read a shapefile from the specified file path using 'sf::read_sf'.
## Then, group the resulting data by 'StatZone' and summarize it.
GOMsf_all <-
  sf::read_sf(GOM_400fm_path)

# glimpse(GOMsf)

my_file_path_local <- file.path(my_paths$outputs,
                           "fishing_effort_location",
                           "st_union_GOMsf.rds")
my_file_path_out <- file.path(my_paths$outputs,
                           "st_union_GOMsf.rds")

# If the file exists, read the data from the RDS file (to speed it up).
if (file.exists(my_file_path_local)) {
  current_path <- my_file_path_local
  st_union_GOMsf <- readr::read_rds(current_path)
} else if (file.exists(my_file_path_out)) {
  current_path <- my_file_path_out
  st_union_GOMsf <- readr::read_rds(current_path)
} else {
  # Start measuring the time it takes to perform the operation and display a message.
  tic("st_union(GOMsf)")
  st_union_GOMsf <- sf::st_union(GOMsf)
  # Stop measuring time and display the elapsed time.
  toc()
  
  readr::write_rds(st_union_GOMsf,
                   my_file_path_out)
}


# mapview::mapview(input_data_convert_dms_short_clean_short_cnt_sf) +
#   south_states_shp

