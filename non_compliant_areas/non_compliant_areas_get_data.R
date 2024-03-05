# today()
# [1] "2024-02-14"
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
  csv_contents <- load_csv_names(my_paths$inputs, csv_names_list)

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

# Run above functions if using csvs downloaded from FHIER
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
# [1] 296286      5 w weeks

# get vessel (home port) info from PIMS with 2 names ----
# "~\R_files_local\my_inputs\non_compliant_areas\vessels_permit_hailng_port_double_name.xlsx"

# Explanations:
# The function 'get_vessel_data_pims' reads vessel data from an Excel file, performs some initial checks, and returns the cleaned vessel data:
# 1. Reads the Excel file at 'vessel_names_file_path' skipping the specified number of rows.
# 2. Displays the dimensions (number of rows and columns) of the raw vessel data.
# 3. Cleans headers of the vessel data using the 'clean_headers' function.
# 4. Returns the cleaned vessel data.
get_vessel_data_pims <-
  function(vessel_names_file_path,
           to_skip = 0,
           my_sheet = "Sheet 1") {
    # file.exists(vessel_names_file_path)
    
    vessels_from_pims_raw <-
      read_xlsx(vessel_names_file_path,
                sheet = my_sheet,
                skip = to_skip)
    
    # clean_headers
    vessels_from_pims <-
      vessels_from_pims_raw %>%
      clean_headers()
    
    return(vessels_from_pims)
  }

vessel_data_pims_double_address <-
    file.path(my_paths$inputs,
              r"(non_compliant_areas\vessels_permit_hailng_port_double_name.xlsx)")

vessel_names_file_path <- 
    file.path(my_paths$inputs,
              r"(non_compliant_areas\Vessels - 2024-02-12_1633.xlsx)")

vessels_from_pims <- get_vessel_data_pims(vessel_names_file_path)

# print_df_names(vessels_from_pims)

vessels_from_pims_double <- 
  get_vessel_data_pims(vessel_data_pims_double_address,
                       to_skip = 0)

dim(vessels_from_pims)
# [1] 23059     8

dim(vessels_from_pims_double)
# [1] 652   3

names(vessels_from_pims_double)
# [1] "vessel_official_number1" "vessel_official_number2" "hailing_port"           

## shorten info from PIMS ----
vessels_from_pims_short <-
  vessels_from_pims |>
  rename("vessel_official_number" = official__) |>
  select(vessel_official_number,
         hailing_port) |>
  distinct()

dim(vessels_from_pims_short)
# 22842     

# remove "NOVESID" vessels
vessels_from_pims_short_ok <-
  vessels_from_pims_short |>
  filter(!grepl("^NOVESID", vessel_official_number))

# dim(vessels_from_pims_short_ok)
# [1] 22466     2

vessels_from_pims_short_ok_ga <- 
vessels_from_pims_short_ok |>
  filter(grepl(", GA", hailing_port)) |> 
  select(vessel_official_number) |> 
  distinct()
  nrow()
# 411

# Get processed metrics tracking ----
processed_input_data_path <- 
  file.path(my_paths$inputs,
            "processing_logbook_data",
            "Outputs")
dir.exists(processed_input_data_path)
# T  

# [1] "~\\R_files_local\\my_inputs\\processing_logbook_data//Outputs//SEFHIER_permitted_vessels_nonSRHS_2023.rds"
# file names for all years
processed_metrics_tracking_file_names <-
  list.files(
    path = processed_input_data_path,
    pattern =
      str_glue("SEFHIER_permitted_vessels_nonSRHS_*"),
    recursive = TRUE,
    full.names = TRUE
  )

# Explanations:
# The variable 'processed_metrics_tracking_permits' is created by applying the 'read_rds' function to each element in the 'processed_metrics_tracking_file_names' list using the 'map_df' function from the 'purrr' package. The result is a combined data frame.

processed_metrics_tracking_permits <-
  map_df(processed_metrics_tracking_file_names,
         read_rds)

# Explanations:
# The variable names of 'processed_metrics_tracking_permits' are set to lowercase using the 'tolower' function. 
names(processed_metrics_tracking_permits) <-
  names(processed_metrics_tracking_permits) |>
  tolower()

dim(processed_metrics_tracking_permits)
# [1] 6822    9

# In compl_clean_sa_vs_gom_m_int_c_short keep only ids in metrics_tracking_not_srhs ----

# Explanations:
# The variable 'compl_err_db_data_metrics' is created by performing a left join between 'processed_metrics_tracking_permits' and 'compl_clean_sa_vs_gom_m_int_c_short' data frames based on the 'vessel_official_number' column. The relationship is specified as "many-to-many."

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
# [1] 535382     13 w weeks

# keep 2022 and 20233 only ----
# print_df_names(compl_err_db_data_metrics)

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23' is created by filtering rows from 'compl_err_db_data_metrics' where the 'week_end' is greater than or equal to 'my_beginning1' (2022-01-01) and the 'week_start' is less than or equal to 'my_end2' (2023-12-31). This operation selects data within a specified time range (2 years).

compl_err_db_data_metrics_2022_23 <-
  compl_err_db_data_metrics |>
  filter(week_end >= my_beginning1 &
           week_start <= my_end2)

dim(compl_err_db_data_metrics_2022_23)
# 535323     13

min(compl_err_db_data_metrics_2022_23$week_start)
# [1] "2022-01-03"
min(compl_err_db_data_metrics_2022_23$week_end)
# [1] "2022-01-09"
max(compl_err_db_data_metrics_2022_23$week_start)
# [1] "2023-12-25"
max(compl_err_db_data_metrics_2022_23$week_end)
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
# [1] 265306     13

compl_err_db_data_metrics_2022_23 |> 
  filter(permit_sa_gom_dual == "sa_only") |> 
  select(vessel_official_number) |> 
  distinct() |> 
  nrow()
# 2145

dim(compl_err_db_data_metrics_2022_23_clean)
# [1] 9559    9 no permit dates

compl_err_db_data_metrics_2022_23_clean |> 
  filter(permit_sa_gom_dual == "sa_only") |> 
  select(vessel_official_number) |> 
  distinct() |> 
  nrow()
# 2145

n_distinct(compl_err_db_data_metrics_2022_23_clean$vessel_official_number)
# 4017 both years

# Add home port info to compl_err_db_data_metrics_2022_23_clean ----
# print_df_names(compl_err_db_data_metrics_2022_23_clean)
# vessel_official_number
# print_df_names(vessels_from_pims_short)

compl_err_db_data_metrics_2022_23_clean__ports <-
  left_join(compl_err_db_data_metrics_2022_23_clean,
            vessels_from_pims_short_ok,
            relationship =
              "many-to-many")

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
# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col' is created by:
# 1. Taking the 'compl_err_db_data_metrics_2022_23_clean__ports_short' data frame.
# 2. Applying row-wise operations using 'rowwise()' to treat each row independently.
# 3. Creating a new column 'year_permit_sa_gom_dual' by concatenating the 'year' and 'permit_sa_gom_dual' columns.
# 4. Removing any grouping structure using 'ungroup()'.
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
  select(permit_region, sa_permits_, gom_permits_) |>
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

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list' is created by splitting the 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col' data frame into a list based on the levels of the 'year_permit_sa_gom_dual' column. Each element of the list corresponds to a unique combination of year and permit_sa_gom_dual.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |>
  split(
    as.factor(
      compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col$year_permit_sa_gom_dual
    )
  )

map(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list, dim)
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

# Explanations:
# The code applies a series of operations to each data frame in 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list':
# 1. Selects specific columns 'vessel_official_number' and 'compliant_' from the current data frame.
# 2. Keeps only distinct rows based on the selected columns.
# 3. Counts the occurrences of each level in the 'compliant_' column, providing a count of compliant and non-compliant instances for each vessel.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list |>
  map(\(curr_df) {
    curr_df |>
      dplyr::select(vessel_official_number, compliant_) |>
      dplyr::distinct() |>
      dplyr::count(compliant_)
  })

map(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list,
    \(reg_df) {
      n_distinct(reg_df$vessel_official_number)
    })
# 2023
# $gom_only
# 1147

# $sa_only
# 2178

# if compliance is checked for only when permit is active add:
# comp_week_start_dt and comp_week_end_dt to select()

setdiff(
  names(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list[[1]]),
  names(
    compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col
  )
)
# 0

cat(
  c(
    "Result to use for vessels home port and its permit region:",
    "compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list",
    "compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col"
  ),
  sep = "\n"
)

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

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_1' is created by modifying the 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col' data frame:
# 1. Replaces the space followed by a comma with just a comma in the 'hailing_port' column.
# 2. Replaces all occurrences of double spaces with a single space in the 'hailing_port' column.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_1 <- 
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |> 
  mutate(hailing_port =
           str_replace(hailing_port,
                       " ,",
                       ",")) |> 
    mutate(hailing_port =
           str_replace_all(hailing_port,
                       "  ",
                       " "))

# check
grep(",[a-zA-Z]",
     compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_1$hailing_port,
     value = T)
# 0

# print out get_data results ----

cat(
  blue("compl 2023:"),
   "compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_list",
    "compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col",
  blue("Maps:"),
  "us_s_shp",
  sep = "\n"
)

# diffdf::diffdf(vessels_from_pims__vessels_from_metrics_short,
#                compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_1)
#      Variable    No of Differences 
#    hailing_port         179        
# 
# eX.
#    hailing_port        PORT ORANGE , FL     PORT ORANGE, FL   

