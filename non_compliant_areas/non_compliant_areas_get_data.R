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
# get_data_from_csv: 7.55 sec elapsed

compl_clean_sa_vs_gom_m_int_c_short <-
  compl_clean_sa_vs_gom_m_int_c |>
  select(vessel_official_number,
         compliant_,
         year,
         week_start, 
         week_end) |>
  distinct()

dim(compl_clean_sa_vs_gom_m_int_c_short)
# [1] 9365    3
# [1] 296286      5 w weeks

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
dim(permit_info)
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

## shorten info from PIMS ----
# print_df_names(vessels_from_pims)
vessels_from_pims_short <-
  vessels_from_pims |>
  rename("vessel_official_number" = official__) |>
  select(vessel_official_number,
         hailing_port) |>
  distinct()

dim(vessels_from_pims_short)
# [1] 22819     2

# Get processed metrics tracking ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# file names for all years
processed_metrics_tracking_file_names <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_permitted_vessels_nonSRHS_*",
             recursive = TRUE,
             full.names = TRUE)

processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

dim(processed_metrics_tracking_permits)
# [1] 6822    9

# In compl_clean_sa_vs_gom_m_int_c_short keep only ids in metrics_tracking_not_srhs ----

compl_err_db_data_metrics <-
  left_join(
    processed_metrics_tracking_permits,
    compl_clean_sa_vs_gom_m_int_c_short,
    join_by(vessel_official_number),
            relationship =
              "many-to-many"
  )

# TODO: check
# ℹ Row 1 of `x` matches multiple rows in `y`.
# ℹ Row 2744 of `y` matches multiple rows in `x`.

dim(compl_err_db_data_metrics)
# [1] 408454     31
# [1] 411980     31 2023
# [1] 535393     30
# [1] 16450    11
# [1] 535382     13 w weeks

# keep 2022 and 20233 only ----
# print_df_names(compl_err_db_data_metrics)

compl_err_db_data_metrics_2022_23 <-
  compl_err_db_data_metrics |>
  filter(week_end >= my_beginning1 &
           week_start <= my_end2)

dim(compl_err_db_data_metrics_2022_23)
# [1] 145261     31
# [1] 146434     31 2023
# 535323     both

# min(compl_err_db_data_metrics_2022_23$week_start)
# [1] "2022-01-03"
# min(compl_err_db_data_metrics_2022_23$week_end)
# [1] "2022-01-09"
# max(compl_err_db_data_metrics_2022_23$week_start)
# [1] "2023-12-25"
# max(compl_err_db_data_metrics_2022_23$week_end)
# [1] "2023-12-31"

compl_err_db_data_metrics |>
  select(week_start) |> 
  distinct() |> 
  arrange(week_start) |> 
  # head(1)
# 1 2022-01-03
  tail()
# 104 2023-12-25
# 105       <NA>

compl_err_db_data_metrics |>
  select(week_end) |> 
  distinct() |> 
  arrange(week_end) |> 
  # head(1)
# 1 2022-01-09
  tail()
# 104 2023-12-31
# 105       <NA>

# TODO: Where is the first week (52 of the previous year)?

## Remove empty and unused columns ---
# weeks were used for date filter
compl_err_db_data_metrics_2022_23_clean <-
  compl_err_db_data_metrics_2022_23 |>
  remove_empty_cols() |>
  select(-c(week_start, 
            week_end,
            effective_date, 
            end_date)) |>
  distinct()

dim(compl_err_db_data_metrics_2022_23)
# [1] 535323     13

dim(compl_err_db_data_metrics_2022_23_clean)
# [1] 145261     29
# [1] 146434     29 2023
# [1] 496101     30
# [1] 14933    11 no weeks
# [1] 9559    9 no permit dates

# compl_err_db_data_metrics_2022_23_clean |> View()

n_distinct(compl_err_db_data_metrics_2022_23_clean$vessel_official_number)
# 3473
# 3377 2023
# 4017 both

# Add home port info to compl_err_db_data_metrics_2022_23_clean
# print_df_names(compl_err_db_data_metrics_2022_23_clean)
# vessel_official_number
# print_df_names(vessels_from_pims_short)

compl_err_db_data_metrics_2022_23_clean__ports <-
  left_join(compl_err_db_data_metrics_2022_23_clean,
            vessels_from_pims_short)
# ,
#             relationship =
#               "many-to-many")

# TODO:
# ℹ Row 1360 of `x` matches multiple rows in `y`.
compl_err_db_data_metrics_2022_23_clean[1360,]$vessel_official_number
# FL1431JU
# vessels_from_pims_short |> 
#   filter(vessel_official_number == "FL1431JU") 
# 2 ports per vessel

# ℹ Row 20494 of `y` matches multiple rows in `x`.
vessels_from_pims_short[20494,]$vessel_official_number
# FL3185SY               
compl_err_db_data_metrics_2022_23_clean |>
  filter(vessel_official_number == "FL3185SY") |>
  select(year) |> 
  distinct()
# 1 2022
# 2 2023
# multiple years

# Remove columns not used in this analysis ----
print_df_names(compl_err_db_data_metrics_2022_23_clean__ports)
# [1] "vessel_official_number, vessel_name, permits, sa_permits_, gom_permits_, permit_region, permit_sa_gom_dual, compliant_, year, hailing_port"

compl_err_db_data_metrics_2022_23_clean__ports_short <-
  compl_err_db_data_metrics_2022_23_clean__ports |>
  select(vessel_official_number,
         permit_sa_gom_dual,
         compliant_,
         year,
         hailing_port) |>
  distinct()

dim(compl_err_db_data_metrics_2022_23_clean__ports_short)
# [1] 9362    5

# Add a combined column for year and permit_sa_gom_dual ----
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col <-
  compl_err_db_data_metrics_2022_23_clean__ports_short |>
  rowwise() |>
  mutate(year_permit_sa_gom_dual = 
           paste(year, permit_sa_gom_dual)) |>
  ungroup()

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |> 
  select(year_permit_sa_gom_dual) |> 
  distinct()
# 1 2022 gom_only          
# 2 2023 gom_only          
# 3 2022 dual              
# 4 2023 dual              
# 5 2022 sa_only           
# 6 2023 sa_only           

## check, Metrics tracking error! ----
compl_err_db_data_metrics_2022_23_clean |>
  select(permit_grouping_region, sa_permits_, gom_permits_) |>
  distinct()
# A tibble: 3 × 3
#   permit_grouping_region sa_permits_ gom_permits_
#   <chr>                  <chr>       <chr>       
# 1 SA                     Y           N           
# 2 GOM                    N           Y           
# 3 GOM                    Y           Y           
# I.e. GOM == "gom and dual"

## split into separate dfs by permit region in metrics tracking ----

# print_df_names(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col)

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |>
  split(
    as.factor(
      compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col$year_permit_sa_gom_dual
    )
  )

# View(compl_err_db_data_metrics_2022_23_clean__comb_col_list)

map(compl_err_db_data_metrics_2022_23_clean__comb_col_list, dim)
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

# ---
# $`2022 dual`
# [1] 384  10
# 
# $`2022 gom_only`
# [1] 1190   10
# 
# $`2022 sa_only`
# [1] 2991   10
# 
# $`2023 dual`
# [1] 538  10
# 
# $`2023 gom_only`
# [1] 1180   10
# 
# $`2023 sa_only`
# [1] 3276   10
# === Fewer columns
# $`2022 dual`
# [1] 383   6
# 
# $`2022 gom_only`
# [1] 1174    6
# 
# $`2022 sa_only`
# [1] 2915    6
# 
# $`2023 dual`
# [1] 536   6
# 
# $`2023 gom_only`
# [1] 1167    6
# 
# $`2023 sa_only`
# [1] 3187    6  
  
## check vessel/compl counts ----
# compl_err_db_data_metrics_2022_23_clean__comb_col_list$`2023 sa_only`$compliant_

compl_err_db_data_metrics_2022_23_clean__comb_col_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, compliant_) |>
      dplyr::distinct() |>
      dplyr::count(compliant_)
  })
# 2023
# $dual
# # A tibble: 2 × 2
#   compliant_     n
#     <int> <int>
# 1       0   193
# 2       1   241

# 1 NO           235
# 2 YES          301

# 
# $gom_only
# # A tibble: 2 × 2
#   compliant_     n
#     <int> <int>
# 1       0    15
# 2       1   980
# 1 NO            24
# 2 YES         1142

# $sa_only
# # A tibble: 2 × 2
#   compliant_     n
#     <int> <int>
# 1       0  1338
# 2       1  1799

# 1 NO          1363
# 2 YES         1818

map(compl_err_db_data_metrics_2022_23_clean__comb_col_list,
    \(reg_df) {
      n_distinct(reg_df$vessel_official_number)
    })
# 2023
# $GOM
# [1] 1232
# 
# $SA
# [1] 2241
# ---
# $dual
# [1] 251
# 315

# $gom_only
# [1] 981
# 1147

# $sa_only
# [1] 2145
# 2178

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
# View(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col)

cat(
  c(
    "Result to use for vessels home port and its permit region:",
    "compl_err_db_data_metrics_2022_23_clean__comb_col_list",
    "compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col"
  ),
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
  "compl_err_db_data_metrics_2022_23_clean__comb_col_list_short",
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


