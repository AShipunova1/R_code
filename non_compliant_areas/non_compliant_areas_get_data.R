# today()
# [1] "2024-02-08"
# Now for processed data both years

# Data for non compliant area map sources:
# 1) compliance information:
# a) FHIER_Compliance_2023__01_24_2023.csv (from FHIER / compliance report)
# b) FHIER_Compliance_2022__02_05_2024.csv (from FHIER / compliance report)

# 2) Processed Metrics tracking
# SEFHIER_permitted_vessels_nonSRHS_{my_year}

# 3) "Permits - 2024-01-25_0904.xlsx"
# get it from PIMS
# Menu: permits
# Filter:
# Fishery = RCG - Gulf Charter/headboat For Reef Fish, CHG - Gulf Charter/headboat For Coastal Migratory Pelagic Fish, SC - South Atlantic Charter/headboat For Snapper-grouper, CHS - Atlantic Charter/headboat For Coastal Migratory Pelagics, HCHG - Historical Captain Gulf Charter/headboat For Coastal Migratory Pelagic Fish, HRCG - Historical Captain Gulf Charter/headboat For Reef Fish, CDW - Atlantic Charter/headboat For Dolphin/wahoo
# 
# download
# 
# skip first 5 lines in R)

# 4) same for vessels, skip first 3 lines
# Vessels - 2024-02-01_0909.xlsx

# Colored terminal output
library(crayon)

# get db data ----
# db_data_path <-
#   file.path(my_paths$git_r,
#             r"(get_data\get_db_data\get_db_data.R)")

# source(db_data_path)

# tic("run_all_get_db_data()")
# all_get_db_data_result_l <- run_all_get_db_data()
# toc()
# run_all_get_db_data(): 8.86 sec elapsed

# prepare (non) compliant vessels 2023 info ----
# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT
# get data from csvs ----
get_data_from_FHIER_csvs <- function() {

  filenames = c(
    r"(2024_02_05\FHIER_Compliance_2022__02_05_2024.csv)",
    r"(2024_01_24\FHIER_Compliance_2023__01_24_2024.csv)"
  )

  ## ---- get csv data into variables ----
  csv_names_list <- prepare_csv_names(filenames)

  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)

  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
  compl_clean <- compliance_cleaning(csvs_clean1)

  return(compl_clean)
}

get_data_from_csv <- function() {
  # uncomment to run
  # browser()

  compl_clean <- get_data_from_FHIER_csvs()
  
  if (class(compl_clean) == "list" &
      length(compl_clean) == 1) {
    compl_clean <- compl_clean[[1]]
  }
  # View(compl_clean)
  
  # Check the result is a single dataframe, and if not, combine separate dataframes for all years into one.
  if (length(compl_clean) > 1) {
    compl_clean_1 <- join_same_kind_csvs(compl_clean)
  }

  dim(compl_clean_1)
  # [1] 296294     20
  
  compl_clean_2 <- additional_clean_up(compl_clean_1)

  cat("compl_clean_sa_vs_gom_m_int_c")  
  return(compl_clean_2)
}

additional_clean_up <- function(my_df) {
  
  # add columns for month and quarter
  compl_clean_sa_vs_gom_m <- 
    my_df %>%
    # Add a new column 'year_month' by extracting the year and month from the 'week_start' column
    dplyr::mutate(year_month = zoo::as.yearmon(week_start)) %>%
    # Add another new column 'year_quarter' by extracting the year and quarter from the 'week_start' column
    dplyr::mutate(year_quarter = zoo::as.yearqtr(week_start))
  
  # convert report numbers to numeric
  # Create a new data frame 'compl_clean_sa_vs_gom_m_int' by applying integer conversion to selected columns in 'compl_clean_sa_vs_gom_m'.
  compl_clean_sa_vs_gom_m_int <- compl_clean_sa_vs_gom_m %>%
    dplyr::mutate(
      captainreports__ = as.integer(captainreports__),
      negativereports__ = as.integer(negativereports__),
      gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
    )

  return(compl_clean_sa_vs_gom_m_int)
}

# Uncomment and run above functions if using csvs downloaded from FHIER
tic("get_data_from_csv")
compl_clean_sa_vs_gom_m_int_c <- get_data_from_csv()
toc()

# get permit info from PIMS ----
get_permit_data_from_PIMS <- function() {
  permit_names_file_path =
    file.path(my_paths$inputs,
              r"(from PIMS\Permits - 2024-01-25_0904.xlsx)")
  
  # file.exists(permit_names_file_path)
  
  active_permits_from_pims_raw <- 
    read_xlsx(permit_names_file_path, skip = 5)
  
  dim(active_permits_from_pims_raw)
  # [1] 23575    11
  
  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw %>%
    clean_headers()
  
  # separate columns
# Use the 'separate_wider_delim' function to split the 'permit__' column in the 'active_permits_from_pims_temp1' dataframe
# based on the delimiter "-", creating new columns 'permit_code' and 'permit_num'.
# The 'too_many' argument is set to "merge," which means any excess columns generated during the split will be merged.
active_permits_from_pims_temp2 <- 
  active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",                # Delimiter used for splitting
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%

    # Use the 'separate_wider_regex' function to split the 'vessel_or_dealer' column in the resulting dataframe.
    # This function uses regular expressions to define patterns for creating new columns.
    # In this case, it defines patterns for 'vessel_official_number' and 'vessel_name.'
    # The 'too_few' argument is set to "align_start," which aligns any missing columns at the start.
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",  # Regular expression for vessel official number (more than one alphanumeric character)
        " */* ",                                  # Pattern for separating columns with slashes
        vessel_name = "[A-Za-z0-9]+"              # Regular expression for vessel name (more than one alphanumeric character)
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names with "_date"
  # Use the 'grep' function to find and extract column names from the 'active_permits_from_pims_temp2' dataframe
  # that has "_date".
  ends_with_date_fields <- grep("_date", # Pattern to search for in column names
                                names(active_permits_from_pims_temp2),  # Names of columns to search within
                                value = TRUE)         # Return matching column names as values in the result.


  # convert to the date format
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    dplyr::select(status_date) %>%                 # Select 'status_date' column
    dplyr::arrange(dplyr::desc(status_date)) %>%   # Arrange in descending order
    dplyr::distinct() %>%                               # Remove duplicate rows
    head()                                       # Retrieve the first few rows
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permit_info <- get_permit_data_from_PIMS()
# print_df_names(permit_info)
# [1] 23575    13

# get vessel (home port) info from PIMS ----
get_vessel_data_pims <- 
  function() {
  vessel_names_file_path =
    file.path(my_paths$inputs,
              r"(from PIMS\Vessels - 2024-02-01_0909.xlsx)")
  
  # file.exists(vessel_names_file_path)
  
  vessels_from_pims_raw <- 
    read_xlsx(vessel_names_file_path, skip = 3)
  
  dim(vessels_from_pims_raw)
# [1] 23036     8
  
  # clean_headers
  vessels_from_pims <-
    vessels_from_pims_raw %>%
    clean_headers()
  
  # View(vessels_from_pims_temp1)
  
  return(vessels_from_pims)
  }

vessels_from_pims <- get_vessel_data_pims()

dim(vessels_from_pims)
# [1] 23036     8

# get metrics tracking and SRHS vessel info (from FHIER)
metrics_tracking_from_fhier_file_name <- 
  str_glue("Detail Report - via Valid and Renewable Permits Filter (SERO_NEW Source)_{my_year}.csv")

## use metricks only vessels ----
metric_tracking_no_srhs_path <-
  file.path(my_paths$git_r,
            r"(get_data\get_data_from_fhier\metric_tracking_no_srhs.R)")
# file.exists(metric_tracking_no_srhs_path)

source(metric_tracking_no_srhs_path)

# fhier_reports_metrics_tracking_not_srhs_ids

fhier_reports_metrics_tracking_not_srhs_all_cols_2023_perm <-
  fhier_reports_metrics_tracking_not_srhs_all_cols_list$`2023` |> 
  mutate(permit_sa_gom_metr =
           case_when(sa_permits_ == "Y" &
                       gom_permits_ == "N" ~ "sa_only",
                     sa_permits_ == "N" &
                       gom_permits_ == "Y" ~ "gom_only",
                     sa_permits_ == "Y" &
                       gom_permits_ == "Y" ~ "dual",
                     ))

# Keep only ids in fhier_reports_metrics_tracking_not_srhs_ids ----


# ---
# Explanations:
# The code uses the left_join() function to merge two data frames,
# 'fhier_reports_metrics_tracking_not_srhs_all_cols_2023' and 'compl_err_db_data'.
# The join is performed based on the equality of 'vessel_official_number' and
# 'vessel_official_nbr'. The result is stored in 'compl_err_db_data_metrics'.
# A left join retains all rows from the left data frame and adds matching rows
# from the right data frame. Unmatched rows in the right frame will have NAs
# in the corresponding columns in the result.

compl_err_db_data_metrics <-
  left_join(
    fhier_reports_metrics_tracking_not_srhs_all_cols_2023_perm,
    compl_err_db_data,
    join_by(vessel_official_number == vessel_official_nbr)
  )

dim(compl_err_db_data_metrics)
# [1] 408454     31
# [1] 411980     31 2023

## 2023 only ---

# Explanations:
# The code uses the filter() function from the dplyr package to subset the
# 'compl_err_db_data_metrics' data frame based on date conditions:
# - Rows where 'comp_week_start_dt' is before '2023-01-01'.
# - Rows where 'comp_week_end_dt' is on or after '2023-01-01'.
# The result is stored in 'compl_err_db_data_metrics_2023'.
compl_err_db_data_metrics_2023 <-
  compl_err_db_data_metrics |>
  filter(comp_week_end_dt >= '2023-01-01' &
           comp_week_start_dt < '2024-01-01')

dim(compl_err_db_data_metrics_2023)
# [1] 145261     31
# [1] 146434     31 2023

# compl_err_db_data_metrics_2023 |>
#   filter(permit_grouping_region == "GOM") |>
#   select(vessel_official_number) |>
#   distinct() |>
#   nrow()
# [1] 1232

# 135+75+14+121+644
# 989

## Remove empty columns ---
compl_err_db_data_metrics_2023_clean <-
  compl_err_db_data_metrics_2023 |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(compl_err_db_data_metrics_2023_clean)
# [1] 145261     29
# [1] 146434     29 2023

# compl_err_db_data_metrics_2023_clean |> View()

n_distinct(compl_err_db_data_metrics_2023_clean$vessel_official_number)
# 3473
# 3377 2023

fhier_reports_metrics_tracking_not_srhs_all_cols_2023_perm |> 
  select(permit_grouping_region, sa_permits_, gom_permits_) |>
  distinct()
#   permit_grouping_region sa_permits_ gom_permits_
#   <chr>                  <chr>       <chr>       
# 1 SA                     Y           N           
# 2 GOM                    Y           Y           
# 3 GOM                    N           Y           

## split into separate dfs by permit region in metrics tracking ----
# check
compl_err_db_data_metrics_2023_clean |>
  select(permit_grouping_region, sa_permits_, gom_permits_) |>
  distinct()
# A tibble: 3 × 3
#   permit_grouping_region sa_permits_ gom_permits_
#   <chr>                  <chr>       <chr>       
# 1 SA                     Y           N           
# 2 GOM                    N           Y           
# 3 GOM                    Y           Y           
# I.e. GOM == "gom and dual"

compl_err_db_data_metrics_2023_clean_perm <- 
  compl_err_db_data_metrics_2023_clean |>
  mutate(permit_sa_gom_metr =
           case_when(sa_permits_ == "Y" &
                       gom_permits_ == "N" ~ "sa_only",
                     sa_permits_ == "N" &
                       gom_permits_ == "Y" ~ "gom_only",
                     sa_permits_ == "Y" &
                       gom_permits_ == "Y" ~ "dual",
                     ))

## split into separate dfs by permit region ----
# Explanations:
# The code uses the split() function to divide the data frame
# 'compl_err_db_data_metrics_2023_clean' into a list of data frames.
# The splitting criterion is the values in the 'permit_grouping_region' column.
# Each unique value in this column will correspond to a separate data frame
# in the resulting list, stored in 'compl_err_db_data_metrics_2023_clean_list'.
# This is useful for further analysis or processing on subsets of the data.

compl_err_db_data_metrics_2023_clean_list <- 
  compl_err_db_data_metrics_2023_clean_perm |> 
  split(as.factor(compl_err_db_data_metrics_2023_clean_perm$permit_sa_gom_metr))

map(compl_err_db_data_metrics_2023_clean_list, dim)
# $GOM
# [1] 54765    29
# 
# $SA
# [1] 90496    29
# ---
# 2023
# $dual
# [1] 12151    30
# 
# $gom_only
# [1] 43886    30
# 
# $sa_only
# [1] 90397    30

## check vessel/compl counts ----
compl_err_db_data_metrics_2023_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      dplyr::distinct() |>
      dplyr::count(is_comp)
  })
# $dual
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0   193
# 2       1   241
# 
# $gom_only
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0    15
# 2       1   980
# 
# $sa_only
# # A tibble: 2 × 2
#   is_comp     n
#     <int> <int>
# 1       0  1338
# 2       1  1799

map(compl_err_db_data_metrics_2023_clean_list,
    \(reg_df) {
      n_distinct(reg_df$vessel_official_number)
    })
# $GOM
# [1] 1232
# 
# $SA
# [1] 2241
# ---
# $dual
# [1] 251
# 
# $gom_only
# [1] 981
# 
# $sa_only
# [1] 2145

# Metrics:
# Total Vessels  3,539
# 3,513
# Total Vessels With SA Only  2,211
# 2,207
# Total Vessels With GOM Permit Only  1,028
# 1,026
# Total Dual (SA & GOM) Permitted Vessels  300
# 280

# if compliance is checked for only when permit is active add:
# comp_week_start_dt and comp_week_end_dt to select()

# if override is taken in the account, add it

## Remove columns not use in this analysis ----
compl_err_db_data_metrics_2023_clean_list_short <- 
  compl_err_db_data_metrics_2023_clean_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, is_comp) |>
      distinct()
  })

# prepare vessel_permit_data ----
## 2023 permits ----

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
# This code creates a summarized data frame for vessels with permits in 2023 by grouping, summarizing, and separating permit types into three groups. Here's the breakdown of the comments:
# 
# 1. Creating a summarized data frame for vessels with permits in 2023.
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

# dim(vessels_permits_home_port_22_reg_short)
# [1] 4729    5
# [1] 4739    5

vessels_permits_home_port_short <-
  all_get_db_data_result_l$vessels_permits |>
  dplyr::select(SERO_OFFICIAL_NUMBER,
                dplyr::starts_with("SERO_HOME")) |>
  remove_empty_cols() |>
  dplyr::distinct()

# View(vessels_permits_home_port_short)

# keep only vessels in metric tracking ----
vessels_from_pims__vessels_from_metrics_j <-
  left_join(
    fhier_reports_metrics_tracking_not_srhs_all_cols_2023_perm,
    vessels_from_pims,
    join_by(vessel_official_number == official__)
  )

  # vessels_from_pims |>
  # filter(
  #   official__ %in% fhier_reports_metrics_tracking_not_srhs_all_cols_2023$vessel_official_number
  # )

dim(vessels_from_pims__vessels_from_metrics_j)
# [1] 3245    8
# [1] 3446   21

# get home port info from PIMS ----
# print_df_names(vessels_from_pims__vessels_from_metrics_j)
vessels_from_pims__vessels_from_metrics_short <-
  vessels_from_pims__vessels_from_metrics_j |>
  select(vessel_official_number,
         hailing_port,
         permit_sa_gom_metr) |>
  distinct()

dim(vessels_from_pims__vessels_from_metrics_short)
# [1] 3191    2
# [1] 3392    3

cat(
  "Result to use for vessels home port and its permit region:",
  "vessels_from_pims__vessels_from_metrics_short",
  sep = "\n"
)

# cat("Result to use for vessels home port and its permit region:",
# "vessels_permits_home_port_22_reg_short",
# "vessels_from_pims__vessels_from_metrics_short",
# "To use all data from the db:",
# "vessels_permits_home_port_short",
# sep = "\n")

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

vessels_from_pims__vessels_from_metrics_short_1 <- 
  vessels_from_pims__vessels_from_metrics_short |> 
  mutate(hailing_port =
           str_replace(hailing_port,
                       " ,",
                       ",")) |> 
    mutate(hailing_port =
           str_replace_all(hailing_port,
                       "  ",
                       " "))

grep(",[a-zA-Z]",
     vessels_from_pims__vessels_from_metrics_short_1$hailing_port,
     value = T)
# 0

## Fix port addresses ----
# run once, gives vessels_permits_home_port_c_st_fixed

# fix_ports_file_path <-
#   file.path(my_paths$git_r,
#             current_project_basename,
#             "non_compliant_areas_fix_lat_lon.R")
# 
# source(fix_ports_file_path)

# print out get_data results ----

cat(
  blue("All DB data:"),
  "all_get_db_data_result_l",
  blue("compl 2023:"),
  "compl_err_db_data_metrics_2023_clean_list_short",
  blue("vessel_permit 2023 with lat/long:"),
  "vessels_permits_home_port_lat_longs_city_state",
  blue("Maps:"),
  "us_s_shp",
  sep = "\n"
)
# grep("  ",
#      vessels_from_pims__vessels_from_metrics_short$hailing_port,
#      value = T)
# [1] "CRYSTAL  RIVER, FL"

# diffdf::diffdf(vessels_from_pims__vessels_from_metrics_short,
#                vessels_from_pims__vessels_from_metrics_short_1)
#      Variable    No of Differences 
#    hailing_port         179        
# 
# eX.
#    hailing_port        PORT ORANGE , FL     PORT ORANGE, FL   


