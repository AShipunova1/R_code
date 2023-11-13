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

# input_data <- 
#   input_data_raw_esri |> 
#   select(-c(X, my_address)) |>
#   rename(USER_NYEAR = NYEAR,
#          USER_FK_LANDING_LOCATION_ID = FK_LANDING_LOCATION_ID,
#          USER_LATITUDE = LATITUDE, 
#          USER_LONGITUDE = LONGITUDE,
#          USER_UseCount = UseCount,
#          IN_Address = address,
#          Y = lat,
#          X = long
#          )

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

