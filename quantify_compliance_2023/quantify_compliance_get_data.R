# this file is called from quantify_compliance.R

# files to upload:
# FHIER_Compliance_2023__01_24_2023.csv (from FHIER / compliance report)
# FHIER_Compliance_2022__02_05_2024.csv (from FHIER / compliance report)

# Processed Metrics tracking
# SEFHIER_permitted_vessels_nonSRHS_{my_year}

# "~\R_files_local\my_inputs\processing_logbook_data\Outputs\SEFHIER_processed_Logbooks_2022.rds"

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

get_data_from_csv <- function() {
  # uncomment to run
  # browser()

  compl_clean <- get_data_from_FHIER_csvs()
  
  if (class(compl_clean) == "list" &
      length(compl_clean) == 1) {
    compl_clean <- compl_clean[[1]]
  }

  # Check if the result is a single dataframe, and if not, combine separate dataframes for all years into one.
  if (length(compl_clean) > 1) {
    compl_clean_1 <- join_same_kind_csvs(compl_clean)
  }

  # dim(compl_clean_1)
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

# get_permit_data_from_metrics_tracking ----
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

# get processed logbook data ----
# For all years
processed_logbooks_file_names <-
  list.files(path = processed_input_data_path,
             pattern = "SEFHIER_processed_Logbooks_*",
             recursive = TRUE,
             full.names = TRUE)

processed_logbooks <-
  map_df(processed_logbooks_file_names,
         read_rds)

names(processed_logbooks) <-
  names(processed_logbooks) |>
  tolower()

# get vessels with no logbooks ----
# For all years
vessels_no_logbooks_file_names <-
  list.files(path = processed_input_data_path,
             pattern = "vessels_with_zero_logbooks_*",
             recursive = TRUE,
             full.names = TRUE)

vessels_no_logbooks <-
  map_df(vessels_no_logbooks_file_names,
         read_rds) |> 
  distinct()

names(vessels_no_logbooks) <-
  names(vessels_no_logbooks) |>
  tolower()

results <- c(
  "compl_clean_sa_vs_gom_m_int_c",
  "processed_metrics_tracking_permits",
  "processed_logbooks",
  "vessels_no_logbooks"
)

cat(results, sep = "\n")
