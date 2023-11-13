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
