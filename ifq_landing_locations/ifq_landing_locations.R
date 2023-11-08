# setup for ifq_landing_locations ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

# get data for ifq_landing_locations ----
# 1) convert addresses from the original csv to coordinates with ARCgis
# 2) upload the result to R
input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use_geocoded.csv)")

input_data <- readr::read_csv(input_data_file_path)

# problems(input_data)
# # A tibble: 2 × 5
#     row   col expected actual file                                                      
#   <int> <int> <chr>    <chr>  <chr>                                                     
# 1  1264    68 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…
# 2  1264    78 a double TX     C:/Users/anna.shipunova/Documents/R_files_local/my_inputs…

# data_overview(input_data)
# print_df_names(input_data)

# fill in empty ----
input_data |>
  filter(USER_FK_LANDING_LOCATION_ID == "2128") |>
  select(USER_LATITUDE,
         USER_LONGITUDE) |> 
  distinct() |>
  glimpse()

# test
one_dms_coord = "97° 07'991" 
one_dms_coord2 = "-82.149261"

# Assuming North West coords only in our data
convert_dms_to_dd_nw <- 
  function(one_dms_coord) {
    
    # browser()
    if (grepl("^-", one_dms_coord)) {
      # use abs to remove "-"
      one_dms_coord = abs(as.double(one_dms_coord))
    }
    
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
      dd_coord <- as.double(one_dms_coord)
    }
    
    return(dd_coord)
  }

# convert_dms_to_dd_nw("97° 07' 991")
# convert_dms_to_dd_nw("-82.149261")

tic("input_data_convert_dms")
input_data_convert_dms <- 
  input_data |>
  dplyr::rowwise() |>
  dplyr::mutate(
    converted_dms_lat = convert_dms_to_dd_nw(USER_LATITUDE),
    converted_dms_lon = convert_dms_to_dd_nw(USER_LONGITUDE)
  ) |>
  dplyr::ungroup()

toc()
# input_data_convert_dms: 0.06 sec elapsed

glimpse(input_data_convert_dms)

# check if given lat/lon is different from geocoded ----
# USER_LATITUDE, USER_LONGITUDE, X, Y, OID_, USER_FK_LANDING_LOCATION_ID
## compare user_coord with geocoded ----
# print_df_names(input_data_convert_dms)
input_data_convert_dms |>
  filter(
    !round(abs(X), 2) == round(converted_dms_lon, 2) |
      !round(abs(Y), 2) == round(converted_dms_lat, 2)
  ) |>
  select(X, converted_dms_lon,
         Y, converted_dms_lat) |>
  distinct() |>
  dim()
# 63
# 58 if round to 1 digit

input_data_convert_dms |>
  filter(
    !round(abs(DisplayX), 2) == round(converted_dms_lon, 2) |
      !round(abs(DisplayY), 2) == round(converted_dms_lat, 2)
  ) |>
    select(X, DisplayX, converted_dms_lon,
         Y, DisplayY, converted_dms_lat) |>
  distinct() |>
  dim()
# [1] 65  4

input_data_convert_dms |>
  filter(
    !round(DisplayX, 2) == round(X, 2) |
      !round(DisplayY, 2) == round(Y, 2)
  ) |>
    select(X, DisplayX, converted_dms_lon,
         Y, DisplayY, converted_dms_lat) |>
  distinct() |> 
  dim()
# [1] 13  6

# USER_FK_LANDING_LOCATION_ID
# 2128	27° 54.478' N	97° 07.991' W
# 27° 54' 478"
# = 27° + 54'/60 + 478"/3600
# = 28.032778°
# 
# 97° 07' 991"
# = 97° + 07'/60 + 991"/3600
# = 97.391944°

# fix ----
input_data_convert_dms |>
  filter(X == 0) |> 
  dim()
# 8


dim(input_data_convert_dms)
Filter(function(x)!all(is.na(x)), input_data_convert_dms) |> dim()

# dim(input_data_convert_dms)
# [1] 3418   82
# 
# Filter(function(x)!all(is.na(x)), input_data_convert_dms) |> dim()
# [1] 3418   64

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))
input_data_convert_dms %>% select(where(not_all_na)) |> dim()
# [1] 3418   64

input_data_convert_dms %>% 
  remove_empty_cols() |>
  filter(X == 0) |>
  mutate(x_a = NA,
         a = coalesce(x_a, converted_dms_lon)) |>
  remove_empty_cols() |>
  # remove_0_cols() |> 
  glimpse()

# shorten ---
keep_fields_list <-
  c(
    "OID_",
    "converted_dms_lat",
    "converted_dms_lon",
    "USER_NYEAR",
    "USER_UseCount",
    "X",
    "Y"
  )

input_data_convert_dms_short <- 
  input_data_convert_dms |> 
  select(all_of(keep_fields_list)) |> 
  distinct()
  
# dim(input_data_convert_dms_short)
  # [1] 3418    7

# By year, map all the landing locations - so we can see the growth of time. 
# By year, map the landing location with somehow displaying which locations are used the most.  I think we can do this with color coding.
