# today()
# [1] "2024-01-03"
# Dates for non compliant area map:
# vessel home port information: Jan 2, 2024 (vessels_permits.rds);
# compliance information: Dec 4, 2024 (compl_err_db_data_raw.rds);
# metrics tracking: Aug 23, 2023 (Detail_Report_12312021_12312022__08_23_2023.csv);
# SRHS vessels: Aug 18, 2023 (2022_SRHS_Vessels_08_18_2023.xlsx)

# Colored terminal output
library(crayon)

# get db data ----
db_data_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_db_data\get_db_data.R)")

source(db_data_path)

tic("run_all_get_db_data()")
all_get_db_data_result_l <- run_all_get_db_data()
toc()
# run_all_get_db_data(): 8.86 sec elapsed

# prepare (non) compliant vessels 2022 info ----
compl_err_db_data <- 
  all_get_db_data_result_l$compl_err_db_data

## use metricks only vessels ----
source(r"(~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")

# fhier_reports_metrics_tracking_not_srhs_ids

# Keep only ids in fhier_reports_metrics_tracking_not_srhs_ids

fhier_reports_metrics_tracking_not_srhs_all_cols_2022 <-
  fhier_reports_metrics_tracking_not_srhs_all_cols_list$`2022`

# ---
# Explanations:
# The code uses the left_join() function to merge two data frames,
# 'fhier_reports_metrics_tracking_not_srhs_all_cols_2022' and 'compl_err_db_data'.
# The join is performed based on the equality of 'vessel_official_number' and
# 'vessel_official_nbr'. The result is stored in 'compl_err_db_data_metrics'.
# A left join retains all rows from the left data frame and adds matching rows
# from the right data frame. Unmatched rows in the right frame will have NAs
# in the corresponding columns in the result.

compl_err_db_data_metrics <-
  left_join(
    fhier_reports_metrics_tracking_not_srhs_all_cols_2022,
    compl_err_db_data,
    join_by(vessel_official_number == vessel_official_nbr)
  )

# fhier_reports_metrics_tracking_not_srhs_all_cols_2022 |> 
#   filter(permit_grouping_region == "GOM") |> 
#   select(vessel_official_number) |> 
#   distinct() |> 
#   dim()
# 1325

# compl_err_db_data_metrics |> 
#   filter(permit_grouping_region == "GOM") |> 
#   select(vessel_official_number) |> 
#   distinct() |> 
#   nrow()
# [1] 1325

dim(compl_err_db_data_metrics)
# [1] 408454     31

## 2022 only ---

# Explanations:
# The code uses the filter() function from the dplyr package to subset the
# 'compl_err_db_data_metrics' data frame based on date conditions:
# - Rows where 'comp_week_start_dt' is before '2023-01-01'.
# - Rows where 'comp_week_end_dt' is on or after '2022-01-01'.
# The result is stored in 'compl_err_db_data_metrics_2022'.
compl_err_db_data_metrics_2022 <-
  compl_err_db_data_metrics |>
  filter(comp_week_start_dt < '2023-01-01' &
           comp_week_end_dt >= '2022-01-01')

dim(compl_err_db_data_metrics_2022)
# [1] 145261     31

compl_err_db_data_metrics_2022 |>
  filter(permit_grouping_region == "GOM") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 1232

# 135+75+14+121+644
# 989

## Remove empty columns ---
compl_err_db_data_metrics_2022_clean <-
  compl_err_db_data_metrics_2022 |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(compl_err_db_data_metrics_2022_clean)
# [1] 145261     29

# compl_err_db_data_metrics_2022_clean |> View()

n_distinct(compl_err_db_data_metrics_2022_clean$vessel_official_number)
# 3473

## split into separate dfs by permit region in metrics tracking ----
# check
compl_err_db_data_metrics_2022_clean |>
  select(permit_grouping_region, sa_permits_, gom_permits_) |>
  distinct()
# A tibble: 3 × 3
#   permit_grouping_region sa_permits_ gom_permits_
#   <chr>                  <chr>       <chr>       
# 1 SA                     Y           N           
# 2 GOM                    N           Y           
# 3 GOM                    Y           Y           
# I.e. GOM == "gom and dual"

## split into separate dfs by permit region ----
# Explanations:
# The code uses the split() function to divide the data frame
# 'compl_err_db_data_metrics_2022_clean' into a list of data frames.
# The splitting criterion is the values in the 'permit_grouping_region' column.
# Each unique value in this column will correspond to a separate data frame
# in the resulting list, stored in 'compl_err_db_data_metrics_2022_clean_list'.
# This is useful for further analysis or processing on subsets of the data.

compl_err_db_data_metrics_2022_clean_list <- 
  compl_err_db_data_metrics_2022_clean |> 
  split(as.factor(compl_err_db_data_metrics_2022_clean$permit_grouping_region))

map(compl_err_db_data_metrics_2022_clean_list, dim)
# $GOM
# [1] 54765    29
# 
# $SA
# [1] 90496    29

## check vessel/compl counts ----
compl_err_db_data_metrics_2022_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      dplyr::distinct() |>
      dplyr::count(is_comp)
  })
# $GOM
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0   260
# 2       1  1232
# 
# $SA
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0  1236
# 2       1  1740

map(compl_err_db_data_metrics_2022_clean_list,
    \(reg_df) {
      n_distinct(reg_df$vessel_official_number)
    })
# $GOM
# [1] 1232
# 
# $SA
# [1] 2241

# Metrics:
# Total Vessels  3,539
# Total Vessels With SA Only  2,211
# Total Vessels With GOM Permit Only  1,028
# Total Dual (SA & GOM) Permitted Vessels  300

# if compliance is checked for only when permit is active add:
# comp_week_start_dt and comp_week_end_dt to select()

# if override is taken in the account, add it

## Remove columns not use in this analysis ----
compl_err_db_data_metrics_2022_clean_list_short <- 
  compl_err_db_data_metrics_2022_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      distinct()
  })

# prepare vessel_permit_data ----
## 2022 permits ----

# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_22' using the pipe
# operator and a series of dplyr functions:
# - Filtering: Rows are filtered based on conditions involving date columns
#   ('LAST_EXPIRATION_DATE', 'END_DATE', 'EXPIRATION_DATE', 'EFFECTIVE_DATE').
#   The filter conditions involve checking whether the dates are greater than
#   or less than "2021-12-31".
# - remove_empty_cols(): This custom function removes any empty columns from
#   the resulting data frame.
# The final result is a filtered and cleaned data frame containing relevant permit data.

vessels_permits_home_port_22 <-
  all_get_db_data_result_l$vessels_permits |>
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2021-12-31" |
      END_DATE > "2021-12-31" |
      EXPIRATION_DATE > "2021-12-31"
  ) |> 
  dplyr::filter(EFFECTIVE_DATE < "2021-12-31") |> 
  remove_empty_cols()
  
## add permit region ----
# 
# This code creates a summarized data frame for vessels with permits in 2022 by grouping, summarizing, and separating permit types into three groups. Here's the breakdown of the comments:
# 
# 1. Creating a summarized data frame for vessels with permits in 2022.
# 
# 2. Using the pipe operator (`|>`) to pass the data frame `vessels_permits_home_port_22` to the subsequent functions.
# 
# 3. Grouping the data by vessel official number using the `dplyr::group_by` function.
# 
# 4. Adding a new column named 'all_permits' with all unique permit types as a comma-separated string using the `dplyr::mutate` function.
# 
# 5. Separating permits into three groups using a custom function named `separate_permits_into_3_groups`, with the new permit group field specified as "all_permits".
# 
# 6. Ungrouping the data to revert to the original data frame structure using the `ungroup` function.

vessels_permits_home_port_22_reg <-
  vessels_permits_home_port_22 |>
  dplyr::group_by(SERO_OFFICIAL_NUMBER) |> 
  dplyr::mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") |>
  ungroup()

vessels_permits_home_port_22_reg |>
  select(permit_sa_gom) |> 
  distinct()
# 1 sa_only      
# 2 gom_only     
# 3 dual         

# vessels_permits_home_port_22_reg |> dim()
# [1] 36784    52

## shorten permit_vessel ----
vessels_permits_home_port_22_reg_short <-
  vessels_permits_home_port_22_reg |>
  dplyr::select(SERO_OFFICIAL_NUMBER,
                permit_sa_gom,
                dplyr::starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  dplyr::distinct()

# glimpse(vessels_permits_home_port_22_reg_short)
# [1] 4729    5

vessels_permits_home_port_short <-
  all_get_db_data_result_l$vessels_permits |>
  dplyr::select(SERO_OFFICIAL_NUMBER,
                dplyr::starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  dplyr::distinct()

# View(vessels_permits_home_port_short)

cat("Result to use for vessels home port and its permit region:",
"vessels_permits_home_port_22_reg_short",
"To use all data from the db:",
"vessels_permits_home_port_short",
sep = "\n")

# Map 'us_s_shp' using the 'tigris' package to obtain U.S. state shapes. ----
# The 'cb = TRUE' parameter specifies that you want the U.S. state boundaries.
us_s_shp <-
  tigris::states(cb = TRUE, progress_bar = FALSE)

### Load additional dictionaries ----
misc_info_path <- file.path(my_paths$git_r,
               r"(get_data\misc_info.R)")

source(misc_info_path)

## Rows are retained if the 'NAME' column (state name) matches any of the values in 'south_east_coast_states'. ----
south_east_coast_states_shp <-
  us_s_shp |>
  filter(NAME %in% south_east_coast_states)

## get the bounding box ----
# Explanations:
# The code uses the st_bbox() function from the sf package to create a bounding box
# for the spatial object 'south_east_coast_states_shp'. A bounding box is a rectangular
# region defined by minimum and maximum coordinates along each axis (x and y).
# The resulting bounding box is stored in the variable 'south_east_coast_states_shp_bb'.
# This bounding box can be used for various spatial operations and analyses.

south_east_coast_states_shp_bb <- 
  sf::st_bbox(south_east_coast_states_shp)

# south_east_coast_states_shp_bb
#       xmin       ymin       xmax       ymax 
# -106.64565   24.52310  -75.46062   36.58812 

## save crs ----
tigris_crs <- sf::st_crs(south_east_coast_states_shp)
# User input: NAD83 
# ID["EPSG",4269]]

# Prepare home port coordinates ----
## Fix port addresses ----
# run once, gives vessels_permits_home_port_c_st_fixed

fix_ports_file_path <-
  file.path(my_paths$git_r,
            current_project_dir_name,
            "non_compliant_areas_fix_lat_lon.R")

source(fix_ports_file_path)

dim(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8
# [1] 6762    7

dim(all_vessels_permits_home_port_clean0_fixed)
# [1] 6894    8

### shorten fixed ----
vessels_permits_home_port_c_st_fixed_short <-
  all_vessels_permits_home_port_clean0_fixed |> 
  # vessels_permits_home_port_c_st_fixed |>
  select(SERO_OFFICIAL_NUMBER,
         # permit_sa_gom,
         city_fixed,
         state_fixed) |>
  distinct() |>
  remove_empty_cols()
  
# glimpse(vessels_permits_home_port_c_st_fixed)
# dim(vessels_permits_home_port_c_st_fixed_short)
# [1] 4729    4
# [1] 6762    3

## Add lat long to fixed ports ----

my_file_path_lat_lon <-
  file.path(
    my_paths$outputs,
    current_project_dir_name,
    paste0(
      current_project_dir_name,
      "_no_county_fixed_all_vessels.rds"
    )
  )

file.exists(my_file_path_lat_lon)

# Add lat and lon geo coordinates by city and state

# Explanations:
# The code defines a custom R function 'get_lat_lon_no_county':
# - It takes a data frame 'vessels_permits_home_port_c_st_fixed_short' as input.
# - Utilizes the tidygeocoder::geocode() function to geocode the city and state
#   information, obtaining latitude and longitude coordinates.
# - Returns the resulting data frame 'vessels_permits_home_port_c_st_fixed_lat_longs'.
# This function is used to add geolocation information to the original data frame.

get_lat_lon_no_county <-
  function(vessels_permits_home_port_c_st_fixed_short) {
    vessels_permits_home_port_c_st_fixed_lat_longs <-
      vessels_permits_home_port_c_st_fixed_short |>
      tidygeocoder::geocode(city = "city_fixed",
                            state = "state_fixed")
    return(vessels_permits_home_port_c_st_fixed_lat_longs)
  }

vessels_permits_home_port_lat_longs_city_state <-
  read_rds_or_run(
    my_file_path_lat_lon,
    my_data =
      as.data.frame(vessels_permits_home_port_c_st_fixed_short),
    get_lat_lon_no_county
  )
# 2023-11-27 run for non_compliant_areas_no_county_fixed.rds: 632.64 sec elapsed
# Passing 825 addresses to the Nominatim single address geocoder
# [==================================] 825/825 (100%) Elapsed: 14m Remaining:  0s
# 2024-01-02 run for non_compliant_areas_no_county_fixed_all_vessels.rds: 853.8 sec elapsed

dim(vessels_permits_home_port_lat_longs_city_state)
# [1] 5029    5
# [1] 4729    6
# [1] 6892    5

# data_overview(vessels_permits_home_port_lat_longs_city_state)
# SERO_OFFICIAL_NUMBER 4729
# permit_sa_gom           3
# city_fixed            592
# lat                   547

# today()
# [1] "2024-01-02"
# SERO_OFFICIAL_NUMBER 6854
# city_fixed            802
# state_fixed            32
# lat                   704
# long                  704

# print out get_data results ----

cat(
  blue("All DB data:"),
  "all_get_db_data_result_l",
  blue("compl 2022:"),
  "compl_err_db_data_metrics_2022_clean_list_short",
  blue("vessel_permit 2022 with lat/long:"),
  "vessels_permits_home_port_lat_longs_city_state",
  blue("Maps:"),
  "us_s_shp",
  sep = "\n"
)
