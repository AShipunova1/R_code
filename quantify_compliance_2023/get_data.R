# this file is called from quantify_compliance.R

# files to upload:
# FHIER_Compliance_2023__01_24_2023.csv (from FHIER / compliance report)
# FHIER_Compliance_2022__02_05_2024.csv (from FHIER / compliance report)

# Compliance_Error_Types_03_29_2023.csv

# Permits - 2024-01-25_0904.xlsx
# Get it from PIMS
# 
# Menu: permits
# Filter:
# Fishery = RCG - Gulf Charter/headboat For Reef Fish, CHG - Gulf Charter/headboat For Coastal Migratory Pelagic Fish, SC - South Atlantic Charter/headboat For Snapper-grouper, CHS - Atlantic Charter/headboat For Coastal Migratory Pelagics, HCHG - Historical Captain Gulf Charter/headboat For Coastal Migratory Pelagic Fish, HRCG - Historical Captain Gulf Charter/headboat For Reef Fish, CDW - Atlantic Charter/headboat For Dolphin/wahoo
# 
# download
# 
# skip first 5 lines in R)

project_dir_name <- "FHIER Compliance"

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

# Define a function named 'get_compliance_error_definitions' with no parameters
get_compliance_error_definitions <- function() {
  
  # Create a character vector 'err_desc_filenames' containing the file path
  err_desc_filenames = c(file.path(project_dir_name, "Compliance_Error_Types_03_29_2023.csv"))
  
  # Load the contents of the CSV file specified in 'err_desc_filenames' using 'load_csv_names' and 'my_paths'
  err_desc_csv_contents <- load_csv_names(my_paths, err_desc_filenames)
  
  # Clean the headers of the loaded CSV content using 'clean_headers'
  err_desc_clean_headers_csv_content <-
    clean_headers(err_desc_csv_contents[[1]])
  
  # Convert a specific column ("last_updated") to date format using 'change_to_dates'
  err_desc <-
    change_to_dates(err_desc_clean_headers_csv_content,
                    "last_updated",
                    "%m/%d/%Y %I:%M:%S %p")
  
  # Return the cleaned and processed data frame 'err_desc'
  return(err_desc)
}

get_permit_data_from_PIMS <- function() {
  permit_names_file_path =
    file.path(curr_proj_input_path,
              r"(Permits - 2024-01-25_0904.xlsx)")
  
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
active_permits_from_pims_temp2 <- active_permits_from_pims_temp1 %>%
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

get_data_from_csv <- function() {
  # uncomment to run
  # browser()

  compl_clean <- get_data_from_FHIER_csvs()
  
  if (class(compl_clean) == "list" &
      length(compl_clean) == 1) {
    compl_clean <- compl_clean[[1]]
  }
  # View(compl_clean)
  
  ## get compliance error definitions from csvs ----
  err_desc <- get_compliance_error_definitions()
  
  # Check the result is a single dataframe, and if not, combine separate dataframes for all years into one.
  if (length(compl_clean) > 1) {
    compl_clean_1 <- join_same_kind_csvs(compl_clean)
  }

  dim(compl_clean_1)
  # [1] 296294     20
  
  ## get permit data from PIMS csv ----
  
  compl_clean_2 <- additional_clean_up(compl_clean_1)

  cat("compl_clean_sa_vs_gom_m_int_c")  
  return(compl_clean_2)
}

# add year_permit column ----
# adjust to a new year

# Create a new data frame 'compl_clean_sa_vs_gom_m_int_c' by adding a new column 'year_permit' to 'compl_clean_sa_vs_gom_m_int'. Depending on the values in the 'year' and 'permit_sa_gom' columns, different combinations of 'year' and a descriptive label are created for the 'year_permit' column using the 'paste' function.

# add_year_permit_col <- 
#   function(compl_clean_sa_vs_gom_m_int) {
# 
#     compl_clean_sa_vs_gom_m_int_c <-     
#     compl_clean_sa_vs_gom_m_int |>
#       dplyr::mutate(
#         year_permit =
#           dplyr::case_when(
#             year == my_year2 &
#               (permit_sa_gom == "sa_only" | permit_sa_gom == "dual") ~
#               paste(year, "sa_dual"),
# 
#             year == my_year1 &
#               (permit_sa_gom == "gom_only" |
#                  permit_sa_gom == "dual") ~
#               paste(year, "gom_dual"),
#             
#             year == my_year2 & permit_sa_gom == "gom_only" ~
#               paste(year, "gom_only"),
#             
#             .default = "unknown"
#           )
#       )
#     
#     return(compl_clean_sa_vs_gom_m_int_c)
#   }

additional_clean_up <- function(compl_clean) {
  
  # separate SA and GOM permits
  compl_clean_sa_vs_gom <-
    separate_permits_into_3_groups(compl_clean)
  
  # View(compl_clean_sa_vs_gom)
  
  # add columns for month and quarter
  compl_clean_sa_vs_gom_m <- compl_clean_sa_vs_gom %>%
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

# compl_clean_sa_vs_gom_m_int_c <-
#   add_year_permit_col(compl_clean_sa_vs_gom_m_int)

active_permits_from_pims <- get_permit_data_from_PIMS()

# get data from db ----
source(file.path(my_paths$git_r, r"(get_data\all_logbooks_data_short_prep.R)"))

# run_all_get_db_data(): 8.56 sec elapsed                                   

# results:
# all_get_db_data_result_l
# all_logbooks_data_2023_short
