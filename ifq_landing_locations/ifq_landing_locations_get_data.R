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

# test_arcGIS_data ----
# check if given lat/lon is different from geocoded ----
# USER_LATITUDE, USER_LONGITUDE, X, Y, OID_, USER_FK_LANDING_LOCATION_ID
## compare user_coord with geocoded ----

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

# check ExInfo and missing coords ----
#   print_df_names(input_data_convert_dms)
# [1] "OID_, Loc_name, Status, Score, Match_type, Match_addr, LongLabel, ShortLabel, Addr_type, PlaceName, Place_addr, Rank, AddNum, AddNumFrom, AddNumTo, AddRange, Side, StPreDir, StName, StType, StDir, StAddr, Nbrhd, City, MetroArea, Subregion, Region, RegionAbbr, Postal, PostalExt, Country, CntryName, LangCode, Distance, X, Y, DisplayX, DisplayY, Xmin, Xmax, Ymin, Ymax, ExInfo, IN_Address, IN_City, IN_Region, IN_Postal, USER_NYEAR, USER_FK_LANDING_LOCATION_ID, USER_LATITUDE, USER_LONGITUDE, USER_STREET, USER_CITY, USER_STATE, USER_ZIP, USER_UseCount, USER_Field10, converted_dms_lat, converted_dms_lon"

# ExInfo — A collection of strings from the input that could not be matched to any part of an address and were used to score or penalize the result.
input_data_convert_dms |>
  filter(!is.na(ExInfo)) |> 
  filter(!ExInfo == "") |>
  remove_empty_cols() |>
  distinct() |> 
  # dim()
# [1] 466  66
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
  dim()
# [1] 67 11

input_data_convert_dms |>
  filter(X == 0) |> 
  dim()
# 8

# using tidygeocoder ----

input_data <- 
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

