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
# from arcGIS
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

# data from arcGIS
input_data_convert_dms <-
  unify_all_user_input_coords(input_data)

# data from tidygeo
input_data_raw_nominatim_converted <- 
  unify_all_user_input_coords(input_data_raw_nominatim_renamed)

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

# prepare_data ----
prepare_data_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "ifq_landing_locations_prepare_data.R")


# file.exists(prepare_data_path)

source(prepare_data_path)

# check
input_data_convert_dms_short |>
  filter(USER_NYEAR == 1899) |>
  select(OID_, USER_UseCount, USER_NYEAR) |>
  distinct() |>
  count(wt = USER_UseCount)
# 43

# input_data_raw_nominatim_converted_coord_short |> 
#   filter(USER_NYEAR == 1899) |>
#   select(USER_UseCount, USER_NYEAR) |>
#   count(wt = USER_UseCount)
# 43

# input_data_convert_dms_short |>
#   add_count(USER_NYEAR, wt = USER_UseCount, name = "total_use_count_y") |>
#   filter(USER_NYEAR == 1899) |>
#   glimpse()

# input_data_raw_nominatim_converted_coord_short |>
#   add_count(USER_NYEAR,
#             wt = USER_UseCount,
#             name = "total_use_count_y") |>
#   filter(USER_NYEAR == 1899) |>
#   glimpse()

# add counts ----
# In summary, the code takes the 'input_data_convert_dms_short_clean_short' data frame, adds two new columns ('use_lat_round' and 'use_lon_round') by rounding the 'use_lat' and 'use_lon' columns, and then applies three 'add_count' operations to calculate counts based on different groupings of columns. The results are stored in the 'input_data_convert_dms_short_clean_short_cnt' data frame.

add_counts <- function(my_df) {
  my_df_cnts <- 
  my_df |>
    mutate(use_lat_round = round(use_lat, 4),
           use_lon_round = round(use_lon, 4)) |>
    # add_count(USER_NYEAR, 
    #           wt = USER_UseCount, 
    #           name = "total_use_count_y") |>
    add_count(use_lat_round, 
              use_lon_round, 
              wt = USER_UseCount, 
              name = "total_place_cnt") |>
    add_count(USER_NYEAR,
              use_lat_round,
              use_lon_round,
              wt = USER_UseCount,
              name = "count_by_year_and_coord")
  return(my_df_cnts)
}

# arcgis
input_data_convert_dms_short_clean_short_cnt <-
  add_counts(input_data_convert_dms_short_clean_short)

# tidygeo
input_data_convert_dms_short_clean_short_cnt_tidy_geo <-
  add_counts(input_data_raw_nominatim_converted_coord_short)

# check
input_data_convert_dms_short_clean_short_cnt |>
  filter(count_by_year_and_coord == 50) |>
  glimpse()

test_arc207 <- 
  input_data_convert_dms_short_clean_short_cnt |>
  filter(USER_FK_LANDING_LOCATION_ID == 207) |>
  arrange(USER_NYEAR) |>
  select(-OID_)
glimpse(test_arc207)
# Rows: 14
# Columns: 12

test_geo207 <- 
  input_data_convert_dms_short_clean_short_cnt_tidy_geo |>
  filter(USER_FK_LANDING_LOCATION_ID == 207) |>
  arrange(USER_NYEAR)

glimpse(test_geo207)
# Rows: 14
# Columns: 11

diffdf::diffdf(test_arc207,
               test_geo207)
# total_use_count_y       14        4693   82233  

test_arc207[14,] |> 
  glimpse()

test_geo207[14,] |> 
  glimpse()

# ---
input_data_convert_dms_short_clean_short_cnt |>
  filter(USER_NYEAR == 1899 &
           USER_FK_LANDING_LOCATION_ID == 108) |>
  # select(starts_with("u")) |> 
  glimpse()

input_data_convert_dms_short_clean_short_cnt_tidy_geo |>
  filter(USER_NYEAR == 1899 &
           USER_FK_LANDING_LOCATION_ID == 108) |>
  # select(starts_with("u")) |> 
  glimpse()

# leave only cnts and unique ----
cnts_only <-
  c(
    "use_lat_round",
    "use_lon_round",
    "count_by_year_and_coord",
    "total_place_cnt",
    "use_addr",
    "USER_NYEAR"
  )

input_data_convert_dms_short_clean_short_cnt_short <-
  input_data_convert_dms_short_clean_short_cnt |>
  select(all_of(cnts_only)) |>
  distinct()

dim(input_data_convert_dms_short_clean_short_cnt_short)
# 3190    
# [1] 3282    7 from tydigeo

input_data_convert_dms_short_clean_short_cnt_to_plot <-
  input_data_convert_dms_short_clean_short_cnt |>
  select(all_of(cnts_only)) |>
  distinct()

input_data_convert_dms_short_clean_short_cnt_tidy_geo_to_plot <- 
  input_data_convert_dms_short_clean_short_cnt_tidy_geo |> 
  select(all_of(cnts_only)) |>
  distinct()

# print_df_names(input_data_convert_dms_short_clean_short_cnt)

# plots ----
get_plotting_path <- file.path(my_paths$git_r,
                 current_project_dir_name,
                 "ifq_landing_locations_plots.R")


file.exists(get_plotting_path)

source(get_plotting_path)