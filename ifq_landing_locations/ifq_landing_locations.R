# setup for ifq_landing_locations ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

# get data for ifq_landing_locations ----
# 1) convert addresses from the original csv to coordinates with ARCgis
# 2) manually (google search) add corrected_addr	corrected_lat	corrected_long if ExInfo is not NA (where possible)
# 2) upload the result to R
input_data_file_path <-
  file.path(my_paths$inputs,
            r"(ifq_landing_locations\IFQ_Landing_Location_Use_geocoded_ex.csv)")

input_data <-
  read.csv(
    input_data_file_path,
    header = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "latin1"
  )

str(input_data)
# problems(input_data)
# # A tibble: 2 × 5
#     row   col expected actual file                                                      
#   <int> <int> <chr>    <chr>  <chr>                                                     
# 1  2739    68 a double TX     C:/Users/anna.shipunova/Documents/R_files_…

# data_overview(input_data)
# print_df_names(input_data)

# fill in empty ----

# test
# one_dms_coord = "97° 07'991" 
# one_dms_coord2 = "-82.149261"
# one_dms_coord3 = "-83.029 W"

# Assuming North West coords only in our data
convert_dms_to_dd_nw <- 
  function(one_dms_coord) {
    # browser()
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
    
    if (minus) {
      dd_coord = -abs(dd_coord)
    }
    return(dd_coord)
  }

# convert_dms_to_dd_nw("97° 07'991")
# convert_dms_to_dd_nw("-82.149261")
# convert_dms_to_dd_nw("-83.029 W")
# convert_dms_to_dd_nw("29.136 N")

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
# input_data_convert_dms: 0.2 sec elapsed

glimpse(input_data_convert_dms)

# check if given lat/lon is different from geocoded ----
# USER_LATITUDE, USER_LONGITUDE, X, Y, OID_, USER_FK_LANDING_LOCATION_ID
## compare user_coord with geocoded ----
# print_df_names(input_data_convert_dms)
input_data_convert_dms |>
  filter(
    !round(abs(X), 2) == round(abs(converted_dms_lon), 2) |
      !round(Y, 2) == round(converted_dms_lat, 2)
  ) |>
  select(X, converted_dms_lon,
         Y, converted_dms_lat) |>
  distinct() |>
  dim()
# 63  4
# 58 if round to 1 digit

input_data_convert_dms |>
  filter(
    !round(abs(DisplayX), 2) == round(abs(converted_dms_lon), 2) |
      !round(DisplayY, 2) == round(converted_dms_lat, 2)
  ) |>
    select(X, DisplayX, converted_dms_lon,
         Y, DisplayY, converted_dms_lat) |>
  distinct() |>
  dim()
# [1] 65  6
# View(input_data_convert_dms)
# Fields:
# https://pro.arcgis.com/en/pro-app/latest/help/data/geocoding/what-is-included-in-the-geocoded-results-.htm#:~:text=The%20DisplayX%20and%20DisplayY%20values,.%2C%20Redlands%2C%20CA%2092373.

# DisplayX—The display x-coordinate of an address returned in the spatial reference of the locator. The display x-coordinate is returned in spatial reference WGS84 (WKID 4326) by the ArcGIS World Geocoding Service. For matches to PointAddress or Subddress as indicated in the Addr_type field and PointAddress role-based locators, this value represents the x-coordinate value of the building rooftop or parcel centroid for the address. It differs from the primary x-value, which represents the x-coordinate of the side of street location or the street entry for an address. However, there are exceptions, as some data sources used by the ArcGIS World Geocoding Service only provide the rooftop location of PointAddress and Subaddress features. In other cases, only the street-side location is available for some PointAddress and Subaddress features. For such cases, the X and DisplayX values are equivalent. For all other Addr_type matches and locators, this value is equal to the x-value.

# compare geocoded coords
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

# check ExInfo
# ExInfo—A collection of strings from the input that could not be matched to any part of an address and were used to score or penalize the result.
input_data_convert_dms |>
  filter(!is.na(ExInfo)) |>
  remove_empty_cols() |>
  distinct() |> 
  # dim()
# [1] 466  59
#   print_df_names()
# [1] "OID_, Loc_name, Status, Score, Match_type, Match_addr, LongLabel, ShortLabel, Addr_type, PlaceName, Place_addr, Rank, AddNum, AddNumFrom, AddNumTo, AddRange, Side, StPreDir, StName, StType, StDir, StAddr, Nbrhd, City, MetroArea, Subregion, Region, RegionAbbr, Postal, PostalExt, Country, CntryName, LangCode, Distance, X, Y, DisplayX, DisplayY, Xmin, Xmax, Ymin, Ymax, ExInfo, IN_Address, IN_City, IN_Region, IN_Postal, USER_NYEAR, USER_FK_LANDING_LOCATION_ID, USER_LATITUDE, USER_LONGITUDE, USER_STREET, USER_CITY, USER_STATE, USER_ZIP, USER_UseCount, USER_Field10, converted_dms_lat, converted_dms_lon"
  # Loc_name == "World"
  select(
    Place_addr,
    ExInfo,
    IN_Address,
    USER_STREET,
    USER_CITY,
    USER_STATE,
    USER_ZIP,
    Postal,
    X,
    converted_dms_lon,
    Y,
    converted_dms_lat
  ) |>
  distinct() |>
  filter(!USER_ZIP == Postal) |> 
  View()
# [1] 67 11


# fix ----
input_data_convert_dms |>
  filter(X == 0) |> 
  dim()
# 8


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

# print_df_names(input_data_convert_dms)
input_data_convert_dms_short <- 
  input_data_convert_dms |> 
  select(all_of(keep_fields_list)) |> 
  distinct()
  
# dim(input_data_convert_dms_short)
  # [1] 3418    10

# View(input_data_convert_dms_short)
# input_data_convert_dms_short |> 
#     select(OID_, USER_UseCount, USER_NYEAR) |> 
#     group_by(USER_NYEAR) |> 
#     count(wt = USER_UseCount) |> 
#     head(3)
# 1       1899    43
# 2       2010  5434
# 3       2011  6042

# same result for
# input_data_convert_dms_short |> 
#     select(USER_UseCount, USER_NYEAR) |> 
#     count(USER_NYEAR, wt = USER_UseCount) |> 
#     head(3)

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

# input_data_convert_dms_short |> 
#   glimpse()
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
                      Y,
                      converted_dms_lon),
    # can't use coalesce, bc corrected_addr can be ""
    use_addr =
      case_when(
        !is.na(corrected_addr) &
          !corrected_addr == "" ~ corrected_addr,
        .default = Place_addr
      )
  )

# input_data_convert_dms_short |> 
#   filter(corrected_addr == "") |> 
#   rowwise() |> 
#   mutate(ll = length(corrected_addr)) |> 
#   glimpse()

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

input_data_convert_dms_short_clean |>
  filter(OID_ == 194) |>
  glimpse()

# print_df_names(input_data_convert_dms_short_clean)
# Don't unique, bc counts could be the same
input_data_convert_dms_short_clean_short <-
  input_data_convert_dms_short_clean |>
  select(OID_, use_lat, use_lon, USER_NYEAR, USER_UseCount, use_addr)

# glimpse(input_data_convert_dms_short_clean_short)
# [1] 3418    6

# add counts ----

input_data_convert_dms_short_clean_short_cnt <- 
  input_data_convert_dms_short_clean_short |> 
  add_count(USER_NYEAR, wt = USER_UseCount, name = "total_use_count_y")

input_data_convert_dms_short_clean_short_cnt |> 
  filter(USER_NYEAR == 1899) |>
  glimpse()

# By year, map all the landing locations - so we can see the growth of time. 
# By year, map the landing location with somehow displaying which locations are used the most.  I think we can do this with color coding.
