# setup for ifq_landing_locations ----
# library(leaflet)
# Load the 'mapview' library for interactive viewing of spatial data.
library(mapview)

if (!require(tidygeocoder)) {
  install.packages("tidygeocoder")
  library(tidygeocoder)
}
# help(tidygeocoder)

## Load the 'tigris' package to access geographic data.
library(tigris)
## Set the 'tigris_use_cache' option to TRUE. This will enable caching of
## data retrieved from the TIGER/Line Shapefiles service, which can help
## improve data retrieval performance for future sessions.
tigris_use_cache = TRUE

source("~/R_code_github/useful_functions_module.r")

# my_paths <- set_work_dir(): Calls a custom function set_work_dir() to set the working directories.
# 
# current_project_dir_path <- get_current_file_directory(): Calls the get_current_file_directory() function to retrieve the path of the current R project's directory and assigns it to the variable current_project_dir_path.
# 
# current_project_dir_name <- basename(current_project_dir_path): Uses the basename() function to extract the name of the current R project's directory from the current_project_dir_path and stores it in the variable current_project_dir_name.

my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

# get data for ifq_landing_locations ----
get_data_path <- file.path(my_paths$git_r,
                 current_project_dir_name,
                 "ifq_landing_locations_get_data.R")


# file.exists(get_data_path)

source(get_data_path)
# dim(input_data)
# [1] 3418   83
# dim(input_data_raw_esri)
# [1] 3418   22
# dim(input_data_raw_nominatim)
# [1] 3418   28

# unify user coordinate format ----
# Assuming North West coords only in our data
convert_dms_to_dd_nw <- 
  function(one_dms_coord) {
    # browser()
    # a flag to keep negative info
    minus = FALSE
    
    # remove "W and N" at the end
    if (grepl("^-*[0-9.]+\\s*[A-z]+$", one_dms_coord)) {
      one_dms_coord = sub("^(-*[0-9.]+)\\s*[A-z]+$", "\\1", one_dms_coord)
    }

    if (grepl("^-", one_dms_coord)) {
      # use abs to remove "-"
      one_dms_coord = abs(as.double(one_dms_coord))
      # keep a record
      minus = TRUE
    }
    
    # In this code, the if statement is used to check whether the variable one_dms_coord contains any characters that are not digits (0-9) or periods (.) using the grepl() function. If such characters are found, the code proceeds to split the one_dms_coord string into a list of substrings that contain only digits and periods. The strsplit() function is used for this purpose, and it splits the string at one or more non-digit characters, which is specified by the regular expression "\D+". The resulting substrings are stored in the digits_only_list.

    if (grepl("[^0-9.]", one_dms_coord)) {
      digits_only_list <-
        strsplit(one_dms_coord,
                 "\\D+")
      
      degrees <-
        digits_only_list[[1]][1] |>
        as.integer()
      
      minutes <-
        digits_only_list[[1]][2] |>
        as.integer()
      
      seconds <-
        digits_only_list[[1]][3] |>
        as.integer()
      
      dd_coord <-
        degrees + minutes / 60 + seconds / 3600
      
    } else {
      # here if already in the DD format
      dd_coord <- as.double(one_dms_coord)
    }

    # restore the sign if deleted 
    if (minus) {
      dd_coord = -abs(dd_coord)
    }
    return(dd_coord)
  }

# test
one_dms_coord = "97° 07'991"
# one_dms_coord2 = "-82.149261"
# one_dms_coord3 = "-83.029 W"

convert_dms_to_dd_nw(one_dms_coord)
# convert_dms_to_dd_nw(one_dms_coord2)
# convert_dms_to_dd_nw(one_dms_coord3)
# convert_dms_to_dd_nw("29.136 N")

## convert all user input coord format ----
unify_all_user_input_coords <- function(input_data) {
  input_data_convert_dms <-
    input_data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      converted_dms_lat = convert_dms_to_dd_nw(USER_LATITUDE),
      converted_dms_lon = convert_dms_to_dd_nw(USER_LONGITUDE)
    ) |>
    dplyr::ungroup()
}

tic("input_data_convert_dms")
input_data_raw_nominatim_converted <- 
  unify_all_user_input_coords(input_data_raw_nominatim_renamed)
toc()
# input_data_convert_dms: 0.39 sec elapsed

# glimpse(input_data_raw_nominatim_converted)
# input_data_raw_nominatim_converted |> 
#   filter(USER_FK_LANDING_LOCATION_ID == 57) |> 
#   View()

# test arcgis data
test_data_path <- file.path(my_paths$git_r,
                 current_project_dir_name,
                 "ifq_landing_locations_test_arcgis.R")


# file.exists(test_data_path)

# source(test_data_path)

prepare_data_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "ifq_landing_locations_prepare_data.R")


# file.exists(prepare_data_path)

# source(prepare_data_path)

# shorten ----
keep_fields_list_arcgis <-
  c(
    "OID_",
    "Place_addr",
    "corrected_addr",
    "corrected_lat",
    "corrected_long",
    "Y",
    "converted_dms_lat",
    "X",
    "converted_dms_lon",
    "USER_NYEAR",
    "USER_UseCount"
  )

input_data_convert_dms_short <- 
  input_data_convert_dms |> 
  select(any_of(keep_fields_list_arcgis)) |> 
  distinct()
  
# dim(input_data_convert_dms_short)
  # [1] 3418    11

# check
# input_data_convert_dms_short |>
#   filter(USER_NYEAR == 1899) |>
#   select(OID_, USER_UseCount, USER_NYEAR) |>
#   distinct() |>
#   count(wt = USER_UseCount)
# 43

# input_data_convert_dms_short |>
#     add_count(USER_NYEAR, wt = USER_UseCount, name = "total_use_count_y") |>
#     filter(USER_NYEAR == 1899) |>
#   glimpse()

# clean ARCgis df ----
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

# Don't unique yet, bc counts could be the same
input_data_convert_dms_short_clean_short <-
  input_data_convert_dms_short_clean |>
  select(OID_, use_lat, use_lon, USER_NYEAR, USER_UseCount, use_addr)

# glimpse(input_data_convert_dms_short_clean_short)
# [1] 3418    6

# add counts ----
# In summary, the code takes the 'input_data_convert_dms_short_clean_short' data frame, adds two new columns ('use_lat_round' and 'use_lon_round') by rounding the 'use_lat' and 'use_lon' columns, and then applies three 'add_count' operations to calculate counts based on different groupings of columns. The results are stored in the 'input_data_convert_dms_short_clean_short_cnt' data frame.

input_data_convert_dms_short_clean_short_cnt <-
  input_data_convert_dms_short_clean_short |>
  mutate(use_lat_round = round(use_lat, 4),
         use_lon_round = round(use_lon, 4)) |>
  add_count(USER_NYEAR, wt = USER_UseCount, name = "total_use_count_y") |>
  add_count(use_lat_round, use_lon_round, wt = USER_UseCount, name = "total_place_cnt") |>
  add_count(USER_NYEAR,
            use_lat_round,
            use_lon_round,
            wt = USER_UseCount,
            name = "count_by_year_and_coord")

input_data_convert_dms_short_clean_short_cnt |> 
  filter(USER_NYEAR == 1899) |>
  glimpse()

# leave only cnts and unique ----
input_data_convert_dms_short_clean_short_cnt_short <- 
  input_data_convert_dms_short_clean_short_cnt |>
  select(
    use_lat_round,
    use_lon_round,
    count_by_year_and_coord,
    total_use_count_y,
    total_place_cnt,
    use_addr,
    USER_NYEAR
  ) |>
  distinct()
# |>
#   dim()
# 3190    

# print_df_names(input_data_convert_dms_short_clean_short_cnt)

# check missing addresses (arcgis) ----
input_data_convert_dms_short_clean |>
  filter(is.na(X) | is.na(Y)) |>
  filter(is.na(converted_dms_lat) | is.na(converted_dms_lon)) |>
  remove_empty_cols() |>
  select(USER_NYEAR, USER_UseCount) |>
  head()

# 
# using tidygeocoder ----

# ## convert all input coord format ----
# tic("input_data_convert_dms")
# input_data_convert_dms <- 
#   input_data |>
#   dplyr::rowwise() |>
#   dplyr::mutate(
#     converted_dms_lat = convert_dms_to_dd_nw(USER_LATITUDE),
#     converted_dms_lon = convert_dms_to_dd_nw(USER_LONGITUDE)
#   ) |>
#   dplyr::ungroup()
# toc()
# # input_data_convert_dms: 0.2 sec elapsed
# 
# glimpse(input_data_convert_dms)

# check if given lat/lon is different from geocoded ----
# USER_LATITUDE, USER_LONGITUDE, X, Y, OID_, USER_FK_LANDING_LOCATION_ID
## compare user_coord with geocoded ----

# input_data_convert_dms |>
#   filter(
#     !round(abs(X), 2) == round(abs(converted_dms_lon), 2) |
#       !round(Y, 2) == round(converted_dms_lat, 2)
#   ) |>
#   select(X, converted_dms_lon,
#          Y, converted_dms_lat) |>
#   distinct() |>
#   dim()
# 63  4
# 58 if round to 1 digit
# from tidy
# [1] 65  4

# input_data_convert_dms |>
#   filter(
#     !round(abs(DisplayX), 2) == round(abs(converted_dms_lon), 2) |
#       !round(DisplayY, 2) == round(converted_dms_lat, 2)
#   ) |>
#     select(X, DisplayX, converted_dms_lon,
#          Y, DisplayY, converted_dms_lat) |>
#   distinct() |>
#   dim()
# [1] 65  6
# View(input_data_convert_dms)
# Fields:
# https://pro.arcgis.com/en/pro-app/latest/help/data/geocoding/what-is-included-in-the-geocoded-results-.htm#:~:text=The%20DisplayX%20and%20DisplayY%20values,.%2C%20Redlands%2C%20CA%2092373.

# DisplayX—The display x-coordinate of an address returned in the spatial reference of the locator. The display x-coordinate is returned in spatial reference WGS84 (WKID 4326) by the ArcGIS World Geocoding Service. For matches to PointAddress or Subddress as indicated in the Addr_type field and PointAddress role-based locators, this value represents the x-coordinate value of the building rooftop or parcel centroid for the address. It differs from the primary x-value, which represents the x-coordinate of the side of street location or the street entry for an address. However, there are exceptions, as some data sources used by the ArcGIS World Geocoding Service only provide the rooftop location of PointAddress and Subaddress features. In other cases, only the street-side location is available for some PointAddress and Subaddress features. For such cases, the X and DisplayX values are equivalent. For all other Addr_type matches and locators, this value is equal to the x-value.

# compare geocoded coords
# input_data_convert_dms |>
#   filter(
#     !round(DisplayX, 2) == round(X, 2) |
#       !round(DisplayY, 2) == round(Y, 2)
#   ) |>
#     select(X, DisplayX, converted_dms_lon,
#          Y, DisplayY, converted_dms_lat) |>
#   distinct() |> 
#   dim()
# [1] 13  6

# check ExInfo and missing coords ----
#   print_df_names(input_data_convert_dms)
# [1] "OID_, Loc_name, Status, Score, Match_type, Match_addr, LongLabel, ShortLabel, Addr_type, PlaceName, Place_addr, Rank, AddNum, AddNumFrom, AddNumTo, AddRange, Side, StPreDir, StName, StType, StDir, StAddr, Nbrhd, City, MetroArea, Subregion, Region, RegionAbbr, Postal, PostalExt, Country, CntryName, LangCode, Distance, X, Y, DisplayX, DisplayY, Xmin, Xmax, Ymin, Ymax, ExInfo, IN_Address, IN_City, IN_Region, IN_Postal, USER_NYEAR, USER_FK_LANDING_LOCATION_ID, USER_LATITUDE, USER_LONGITUDE, USER_STREET, USER_CITY, USER_STATE, USER_ZIP, USER_UseCount, USER_Field10, converted_dms_lat, converted_dms_lon"


# clean df from tidygeo ----
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
      dplyr::coalesce(
        # corrected_lat,
                      Y,
                      converted_dms_lat),
    use_lon =
      dplyr::coalesce(
        # corrected_long,
                      X,
                      -abs(converted_dms_lon)),
    # can't use coalesce, bc corrected_addr can be ""
    use_addr = IN_Address
      # case_when(
      #   !is.na(corrected_addr) &
      #     !corrected_addr == "" ~ corrected_addr,
      #   .default = Place_addr
      # )
  )

# test
input_data_convert_dms_short_clean |>
  filter(corrected_addr == "") |>
  glimpse()

input_data_convert_dms_short |>
  filter(X == 0) |>
  glimpse()

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

# Don't unique yet, bc counts could be the same
input_data_convert_dms_short_clean_short <-
  input_data_convert_dms_short_clean |>
  select(any_of(
    c(
      "OID_",
      "use_lat",
      "use_lon",
      "USER_NYEAR",
      "USER_UseCount",
      "use_addr"
    )
  ))

# dim(input_data_convert_dms_short_clean_short)
# [1] 3418    6

# add counts ----
# In summary, the code takes the 'input_data_convert_dms_short_clean_short' data frame, adds two new columns ('use_lat_round' and 'use_lon_round') by rounding the 'use_lat' and 'use_lon' columns, and then applies three 'add_count' operations to calculate counts based on different groupings of columns. The results are stored in the 'input_data_convert_dms_short_clean_short_cnt' data frame.

input_data_convert_dms_short_clean_short_cnt <-
  input_data_convert_dms_short_clean_short |>
  mutate(use_lat_round = round(use_lat, 4),
         use_lon_round = round(use_lon, 4)) |>
  add_count(USER_NYEAR, wt = USER_UseCount, name = "total_use_count_y") |>
  add_count(use_lat_round, use_lon_round, wt = USER_UseCount, name = "total_place_cnt") |>
  add_count(USER_NYEAR,
            use_lat_round,
            use_lon_round,
            wt = USER_UseCount,
            name = "count_by_year_and_coord")

# input_data_convert_dms_short_clean_short_cnt |> 
#   filter(use_lon_round > 0)
# 0

input_data_convert_dms_short_clean_short_cnt |> 
  filter(USER_NYEAR == 1899) |>
  glimpse()

# leave cnts and unique ----
input_data_convert_dms_short_clean_short_cnt_short <- 
  input_data_convert_dms_short_clean_short_cnt |>
  select(
    use_lat_round,
    use_lon_round,
    count_by_year_and_coord,
    total_use_count_y,
    total_place_cnt,
    use_addr,
    USER_NYEAR
  ) |> 
  distinct()

dim(input_data_convert_dms_short_clean_short_cnt_short)
# 3190    
# [1] 3312    7 tgeo

# print_df_names(input_data_convert_dms_short_clean_short_cnt)

# make sf ----
# In summary, the code takes the 'input_data_convert_dms_short_clean_short_cnt_short' data frame, adds a new factor column 'year_fct' based on the 'USER_NYEAR' column, and then converts the resulting data frame into a spatial object using the 'sf' package. The spatial object is defined with coordinates based on 'use_lon_round' and 'use_lat_round', and it uses the EPSG 4326 CRS (WGS 84). NA values are allowed in the spatial object. The final result is stored in 'input_data_convert_dms_short_clean_short_cnt_sf'.

input_data_convert_dms_short_clean_short_cnt_sf <-
  input_data_convert_dms_short_clean_short_cnt_short |>
  mutate(
    year_fct = factor(USER_NYEAR)
  ) |>
  sf::st_as_sf(
    coords = c("use_lon_round", "use_lat_round"),
    crs = 4326,
    na.fail = FALSE
  )

# glimpse(input_data_convert_dms_short_clean_short_cnt_sf)

# View(south_states_shp)

# str(south_east_coast_states)

## plot_by_year ---- 
# Create a new variable 'plot_by_year'.
plot_by_year <- ggplot() +  # Initialize a ggplot object.
  geom_sf(data = st_union_GOMsf)
  # Add a spatial feature layer to the plot using 'geom_sf' and 'input_data_convert_dms_short_clean_short_cnt_sf' as data.
  # Map the size aesthetic to 'count_by_year_and_coord'.
  geom_sf(data = input_data_convert_dms_short_clean_short_cnt_sf,
          mapping = aes(size = count_by_year_and_coord),
          colour = "red") +
  
  # Create facets based on year, arranging them in a 3-column layout.
  facet_wrap(vars(year_fct), ncol = 3) +
  
  ggtitle("IFQ Landing Locations") +
  
  theme(legend.position = "bottom") +
  
  # Customize the legend for the size aesthetic.
  guides(size = guide_legend(title = "Counts by year and place"))

plot_by_year
# TODO: check the diff with arcGIS

output_file_name <- 
  "facets_by_year_tidygeo.png"

# output_file_name <- 
#   "facets_by_year_w_states.png"

ggsave(
  file = output_file_name,
  plot = plot_by_year,
  device = "png",
  path = file.path(my_paths$outputs,
                   current_project_dir_name),
  width = 30,
  height = 20,
  units = "cm"
)

# By year, map the landing location with somehow displaying which locations are used the most. ----
# I think we can do this with color coding.

# zcol = "my_label",  # Use the 'my_label' column for labeling map points.
# cex = "total_place_cnt",  # Control the size of map points based on 'total_place_cnt'.
# alpha = 0.3,  # Set the transparency of map points to 0.3 (partially transparent).
# col.regions = viridisLite::turbo,  # Define the color palette for map points using 'turbo' from 'viridisLite'.

input_data_convert_dms_short_clean_short_cnt_sf |>
  mutate(my_label =
           str_glue("{use_addr}; # = {total_place_cnt}")) |>
  mapview(
    zcol = "my_label",
    cex = "total_place_cnt",
    alpha = 0.3,
    col.regions = viridisLite::turbo,
    legend = FALSE,
    layer.name = 'Counts by year and place'
  )

# check missing addresses ----
input_data_convert_dms_short_clean |>
  filter(is.na(X) | is.na(Y)) |>
  filter(is.na(converted_dms_lat) | is.na(converted_dms_lon)) |>
  remove_empty_cols() |>
  select(USER_NYEAR, USER_UseCount) |>
  head()

