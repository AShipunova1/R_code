# setup ----
# devtools::install_github("AShipunova1/R_code/auxfunctions@development",
#                          force = TRUE)

if (!require("auxfunctions")) devtools::install_github("AShipunova1/R_code/auxfunctions@development")

library(auxfunctions)
library(openxlsx)
library(ROracle)

#' Colored terminal output
library(crayon)

# today()

my_paths <- set_work_dir()

#' Make time zones the same
#' These lines of code configure the environment to use the system's current timezone for both the TZ and ORA_SDTZ variables, which is necessary for ensuring that date and time operations are consistent with the local timezone settings.
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

program_start_date <- lubridate::dmy("04/01/2021")

# err msg if no connection, but keep running
if (!exists("con")) {
  try(con <- auxfunctions::connect_to_secpr())
}

#' 1) "Permits - 2024-02-28_0930.xlsx"
#'
#' get it from PIMS
#'
#' Menu: permits
#'
#' Filter:
#'
#' Fishery = RCG - Gulf Charter/headboat For Reef Fish, CHG - Gulf Charter/headboat For Coastal Migratory Pelagic Fish, SC - South Atlantic Charter/headboat For Snapper-grouper, CHS - Atlantic Charter/headboat For Coastal Migratory Pelagics, HCHG - Historical Captain Gulf Charter/headboat For Coastal Migratory Pelagic Fish, HRCG - Historical Captain Gulf Charter/headboat For Reef Fish, CDW - Atlantic Charter/headboat For Dolphin/wahoo
#'
#' download
#'
#' skip first 5 lines in R)
#'
#' 2) same for vessels, skip first 3 lines
#'
#' "Vessels - 2024-02-28_0930.xlsx"
#' 

# auxiliary functions ----

#' Explanations:
#' 
#' - `convert_to_dates <- function(my_df, ymd_format = c("Ymd", "mdY"))` defines the function `convert_to_dates` with two parameters:
#' 
#'   - `my_df`: the input data frame.
#' 
#'   - `ymd_format = c("Ymd", "mdY")`: a vector specifying the date formats to be used for parsing.
#' 
#' - `browser()` is a debugging line that can be uncommented to debug the function interactively. It pauses execution and allows inspection of the environment.
#' 
#' - `my_df_w_dates <-` assigns the result of the following pipeline to the variable `my_df_w_dates`.
#' 
#'   - `my_df |>` starts a pipeline with the data frame `my_df`.
#' 
#'   - `dplyr::mutate(dplyr::across(...))` applies transformations across multiple columns.
#' 
#'     - `tidyselect::where(is.character) & (tidyselect::ends_with("date"))` selects columns that are of character type and whose names end with "date".
#' 
#'     - `~ lubridate::parse_date_time(.x, orders = ymd_format)` is a lambda function that parses the selected columns into Date format using the specified `ymd_format` orders. The `parse_date_time` function from the `lubridate` package is used to handle multiple date formats.
#' 
#' - `return(my_df_w_dates)` returns the modified data frame `my_df_w_dates` with the specified columns converted to Date format.
#' 
#' This function processes the input data frame `my_df`, converting all character columns that end with "date" to Date format using the specified date formats. The resulting data frame is returned.
#'

convert_to_dates <-
  function(my_df, ymd_format = c("Ymd", "mdY")) {
    # browser()
    my_df_w_dates <-
      my_df |>
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.character) &
          (tidyselect::ends_with("date")),
        ~ lubridate::parse_date_time(.x, orders = ymd_format)
      ))
    return(my_df_w_dates)
  }

# upload vessels from PIMS ----
vessel_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Vessels - 2024-06-18_0838.xlsx)")

file.exists(vessel_names_file_path)

#' Explanations:
#' 
#' - `vessels_from_pims <-` assigns the result of the function call to the variable `vessels_from_pims`.
#' 
#' - `auxfunctions::my_read_xlsx(vessel_names_file_path, start_row = 4)` calls the `my_read_xlsx` function from the `auxfunctions` package:
#' 
#'   - `vessel_names_file_path` is the file path to the Excel file containing vessel names.
#' 
#'   - `start_row = 4` specifies that the function should start reading the data from the 4th row of the Excel file.
#' 
#' This line of code reads data from the specified Excel file starting at the 4th row, using a custom function `my_read_xlsx` from the `auxfunctions` package, and stores the resulting data frame in the variable `vessels_from_pims`.
#' 

vessels_from_pims <-
  auxfunctions::my_read_xlsx(vessel_names_file_path, 
                             start_row = 4)

# print_df_names(vessels_from_pims)

# glimpse(vessels_from_pims)
dim(vessels_from_pims)
# [1] 23359     8

# TODO: get vessel (home port) info from PIMS with 2 names
# "~\R_files_local\my_inputs\non_compliant_areas\vessels_permit_hailng_port_double_name.xlsx"

# vessel_data_pims_double_address <-
#     file.path(my_paths$inputs,
#               r"(non_compliant_areas\vessels_permit_hailng_port_double_name.xlsx)")

# vessels_from_pims_double <- 
#   get_vessel_data_pims(vessel_data_pims_double_address,
#                        to_skip = 0)

# dim(vessels_from_pims_double)
# [1] 652   3

# names(vessels_from_pims_double)
# [1] "vessel_official_number1" "vessel_official_number2" "hailing_port"         
# permits ----

## permits from db ----

permits_query <- 
  stringr::str_glue("SELECT
  *
FROM
  srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE
  top IN ( 'CHS', 'SC', 'CDW', 
           'RCG', 'HRCG', 'CHG', 'HCHG' )
  AND ( expiration_date >= TO_DATE('{program_start_date}', 'yyyy-mm-dd')
        OR end_date >= TO_DATE('{program_start_date}', 'yyyy-mm-dd') )
ORDER BY
  vessel_id")

#' formatted current month to use in file names, e.g. "2024_06"
current_year_month <-
  format(lubridate::parse_date_time(lubridate::today(), orders = c("Ymd")),
         "%Y_%m")

permits_query_file_path <-
  file.path(my_paths$inputs,
            "get_db_data",
            stringr::str_glue("permit_info_{current_year_month}.rds"))

file.exists(permits_query_file_path)
# T

permits_query_fun <- function(permits_query) {
  result <- try(dbGetQuery(con, permits_query))
  return(result)
}

get_permit_info <-
  function() {
    read_rds_or_run(permits_query_file_path,
                    permits_query,
                    permits_query_fun
                    #, force_from_db = TRUE
                    )
  }

permit_info_from_db <- get_permit_info()
# 2024-06-21 run for permit_info_2024-06-21.rds: 7.98 sec elapsed

nrow(permit_info_from_db)
# 36947

#' "Gulf" is in all GoM permit names
# permit_info_from_db |> 
#     select(TOP_NAME) |> 
#     distinct()
# ===  

## upload permits from pims ----
permits_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Permits - 2024-06-18_0839.xlsx)")

file.exists(permits_names_file_path)

#' This line of code reads data from the specified Excel file starting at the 5th row, using a custom function `my_read_xlsx` from the `auxfunctions` package, and stores the resulting data frame in the variable `permits_from_pims`.
permits_from_pims <-
  auxfunctions::my_read_xlsx(permits_names_file_path, 
                             start_row = 5)

# Vessel, permits, participants from db ----
vessel_permit_owner_query <- 
  "SELECT
  v_p.*,
  f_p.first_name,
  f_p.middle_name,
  f_p.last_name,
  f_p.name_suffix,
  f_p.address_1,
  f_p.address_2,
  f_p.state,
  f_p.postal_code,
  f_p.phone_nbr,
  f_p.email,
  f_p.license_nbr,
  f_p.participant_id,
  f_p.permit_id,
  f_p.status f_p_status
FROM
       safis.full_participant@secapxdv_dblk.sfsc.noaa.gov f_p
  JOIN (
    SELECT
      v.sero_home_port_city,
      v.sero_home_port_county,
      v.sero_home_port_state,
      v.sero_official_number,
      v.coast_guard_nbr,
      v.event_id,
      v.hull_id_nbr,
      v.owner_id,
      v.state_reg_nbr,
      v.status v_status,
      v.supplier_vessel_id,
      v.ue,
      v.vessel_id v_vessel_id,
      v.vessel_name,
      p.effective_date,
      p.end_date,
      p.entity_id,
      p.expiration_date,
      p.new_owner,
      p.permit,
      p.permit_status,
      p.prior_owner,
      p.vessel_alt_num,
      p.vessel_id p_vessel_id
    FROM
           srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p
      JOIN safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
      ON ( p.vessel_id = sero_official_number )
    WHERE
      p.top IN ( 'CHG', 'HCHG', 'HRCG', 'RCG', 
                 'CHS', 'SC', 'CDW' )
      AND ( p.expiration_date >= ( sysdate - ( 365 / 2 ) )
            OR p.end_date >= ( sysdate - ( 365 / 2 ) ) )
  ) v_p
  ON ( to_char(license_nbr) = to_char(entity_id) )
"

vessel_permit_owner_query_file_path <-
  file.path(my_paths$inputs,
            "get_db_data",
            stringr::str_glue("vessel_permit_owner_{current_year_month}.rds"))

file.exists(vessel_permit_owner_query_file_path)
# T

vessel_permit_owner_query_fun <- function(vessel_permit_owner_query) {
  result <- dbGetQuery(con, vessel_permit_owner_query)
  return(result)
}

get_vessel_permit_owner <-
  function() {
    read_rds_or_run(vessel_permit_owner_query_file_path,
                    vessel_permit_owner_query,
                    vessel_permit_owner_query_fun
                    # , force_from_db = TRUE
                    )
  }

vessel_permit_owner_from_db <- get_vessel_permit_owner()
# 2024-06-24 run for vessel_permit_owner_2024-06-24.rds: 8.66 sec elapsed

nrow(vessel_permit_owner_from_db)
# [1] 31369

# print_df_names(vessel_permit_owner_from_db)

# Clean data ----
#'
#' Explanations:
#'
#' - `vessel_or_dealer_col_name <-` assigns the result of the pipeline to the variable `vessel_or_dealer_col_name`.
#'
#' - `auxfunctions::find_col_name(permits_from_pims, "vessel", "dealer")` calls the `find_col_name` function from the `auxfunctions` package:
#'
#'   - `permits_from_pims` is the data frame being passed as an argument.
#'
#'   - `"vessel"` and `"dealer"` are the strings passed as arguments to the function, likely used to search for column names containing these substrings.
#'
#' - `|>` is the pipe operator, used to pass the result of the function call on the left to the function on the right.
#'
#' - `rlang::sym()` converts the string result from `find_col_name` into a symbol. The `sym` function from the `rlang` package is used to treat the resulting column name as a symbol, which can be used programmatically in tidyverse functions.
#'
#' This line of code searches for a column name in the `permits_from_pims` data frame that contains either "vessel" or "dealer", converts this column name into a symbol, and assigns it to the variable `vessel_or_dealer_col_name`.
#' 

vessel_or_dealer_col_name <- 
  auxfunctions::find_col_name(permits_from_pims, "vessel", "dealer") |> 
  rlang::sym()

# print_df_names(permits_from_pims)

## permits split vessel_or_dealer ----
#'
#' Explanations:
#'
#' - `permits_from_pims__split1 <-` assigns the result of the pipeline to the variable `permits_from_pims__split1`.
#'
#' - `permits_from_pims |>` starts the pipeline with the data frame `permits_from_pims`.
#'
#' - `tidyr::separate(!!vessel_or_dealer_col_name, c('vessel_official_number', 'dealer'), sep = " / ")` separates the `vessel_or_dealer_col_name` column into two new columns:
#'
#'   - `!!vessel_or_dealer_col_name` uses the `rlang` bang-bang operator to evaluate the symbol stored in `vessel_or_dealer_col_name`.
#'
#'   - `c('vessel_official_number', 'dealer')` specifies the names of the new columns created by splitting the original column.
#'
#'   - `sep = " / "` specifies that the column should be split at occurrences of " / ".
#'
#' - `dplyr::mutate(dplyr::across(c('vessel_official_number', 'dealer'), stringr::str_squish))` applies the `str_squish` function to the new columns:
#'
#'   - `dplyr::mutate()` creates new columns or modifies existing ones.
#'
#'   - `dplyr::across(c('vessel_official_number', 'dealer'), stringr::str_squish)` applies the `str_squish` function from the `stringr` package across the specified columns. `str_squish` trims leading and trailing whitespace and reduces internal multiple spaces to a single space.
#'
#' This code processes the `permits_from_pims` data frame by splitting a column (determined by `vessel_or_dealer_col_name`) into two new columns (`vessel_official_number` and `dealer`), then squishes the whitespace in these new columns. The resulting data frame is stored in `permits_from_pims__split1`.
#' 
permits_from_pims__split1 <-
  permits_from_pims |>
  tidyr::separate(!!vessel_or_dealer_col_name,
           c('vessel_official_number', 'dealer'),
           sep = " / ") |>
  dplyr::mutate(dplyr::across(c('vessel_official_number', 'dealer'),
                stringr::str_squish))

# View(permits_from_pims__split1)
# Expected 2 pieces. Missing pieces filled with `NA` in 3038 rows [229, 244, 294,

## permits clean and shorten ----

#' Explanations:
#'
#' - `permits_from_pims__split1_short <-` assigns the final result of the pipeline to the variable `permits_from_pims__split1_short`.
#'
#' - `permits_from_pims__split1 |>` starts the pipeline with the data frame `permits_from_pims__split1`, allowing subsequent transformations.
#'
#' - `convert_to_dates()` is a user-defined function (or could be from a package) that converts certain columns in the data frame to Date format. This function is applied to the data frame to ensure that date columns are correctly formatted.
#'
#' - `dplyr::filter(dplyr::if_any(tidyselect::ends_with("date"), ~ . > program_start_date))` filters rows based on a condition:
#'
#'   - `dplyr::filter()` is a function from the `dplyr` package used to filter rows of the data frame based on a given condition.
#'
#'   - `dplyr::if_any(tidyselect::ends_with("date"), ~ . > program_start_date)` checks if any columns that end with "date" have values greater than `program_start_date`.
#'
#'     - `dplyr::if_any()` is used to apply the filtering condition across multiple columns.
#'
#'     - `tidyselect::ends_with("date")` selects all columns whose names end with "date".
#'
#'     - `~ . > program_start_date` is a lambda function where `.` represents each element in the selected columns, and the condition checks if these elements are greater than `program_start_date`.
#' 
#' This code processes the `permits_from_pims__split1` data frame by converting relevant columns to Date format and then filtering the rows to include only those where any date column has a value greater than `program_start_date`. The resulting data frame is stored in `permits_from_pims__split1_short`.
#' 

permits_from_pims__split1_short <-
  permits_from_pims__split1 |>
  convert_to_dates() |> 
  dplyr::filter(dplyr::if_any(tidyselect::ends_with("date"), 
                ~ . > program_start_date))

dim(permits_from_pims)
# [1] 23934    11

dim(permits_from_pims__split1_short)
# [1] 14675    12

## permits split permit number ----
#'
#' Explanations:
#'
#' - `permits_from_pims__split1_short__split2 <-` assigns the result of the pipeline to the variable `permits_from_pims__split1_short__split2`.
#'
#' - `permits_from_pims__split1_short |>` starts the pipeline with the data frame `permits_from_pims__split1_short`.
#'
#' - `tidyr::separate(permit__, c('permit', 'permit_number'), sep = "-")` separates the `permit__` column into two new columns:
#'
#'   - `permit__` specifies the column to be split.
#'
#'   - `c('permit', 'permit_number')` specifies the names of the new columns created by splitting the original column.
#'
#'   - `sep = "-"` specifies that the column should be split at occurrences of the hyphen character ("-").
#'
#' - `dplyr::mutate(dplyr::across(tidyselect::starts_with('permit'), stringr::str_squish))` applies the `str_squish` function to columns starting with 'permit':
#'
#'   - `dplyr::mutate()` creates new columns or modifies existing ones.
#'
#'   - `dplyr::across(tidyselect::starts_with('permit'), stringr::str_squish)` applies the `str_squish` function from the `stringr` package across the columns whose names start with 'permit'. `str_squish` trims leading and trailing whitespace and reduces internal multiple spaces to a single space.
#'
#' This code processes the `permits_from_pims__split1_short` data frame by splitting the `permit__` column into two new columns (`permit` and `permit_number`), then squishing the whitespace in these new columns. The resulting data frame is stored in `permits_from_pims__split1_short__split2`.
#' 
permits_from_pims__split1_short__split2 <- 
  permits_from_pims__split1_short |> 
  tidyr::separate(permit__,
           c('permit', 'permit_number'),
           sep = "-") |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with('permit'),
                stringr::str_squish))
# View(permits_from_pims__split1_short__split2)

## vessels clean and shorten  ----
#'
#' Explanations:
#'
#' - `hailing_port_col_name <-` assigns the result of the pipeline to the variable `hailing_port_col_name`.
#'
#' - `auxfunctions::find_col_name(vessels_from_pims, "hailing", "port")` calls the `find_col_name` function from the `auxfunctions` package:
#'
#'   - `vessels_from_pims` is the data frame being passed as an argument.
#'
#'   - `"hailing"` and `"port"` are the strings passed as arguments to the function, used to search for column names containing these substrings.
#'
#' - `|>` is the pipe operator, used to pass the result of the function call on the left to the function on the right.
#'
#' - `rlang::sym()` converts the string result from `find_col_name` into a symbol. The `sym` function from the `rlang` package is used to treat the resulting column name as a symbol, which can be used programmatically in tidyverse functions.
#'
#' This line of code searches for a column name in the `vessels_from_pims` data frame that contains either "hailing" or "port", converts this column name into a symbol, and assigns it to the variable `hailing_port_col_name`.
#' 
hailing_port_col_name <- 
  auxfunctions::find_col_name(vessels_from_pims, "hailing", "port") |>
  rlang::sym()

#'
#' Explanations:
#'
#' - `vessels_from_pims_short <-` assigns the result of the pipeline to the variable `vessels_from_pims_short`.
#'
#' - `vessels_from_pims |>` starts the pipeline with the data frame `vessels_from_pims`.
#'
#' - `dplyr::rename("vessel_official_number" = official__)` renames the column `official__` to `vessel_official_number`:
#'
#'   - `dplyr::rename()` is used to change column names in a data frame.
#'
#'   - `"vessel_official_number" = official__` specifies the new name for the column `official__`.
#'
#' - `select(vessel_official_number, !!hailing_port_col_name)` selects the `vessel_official_number` and the column referenced by `hailing_port_col_name`:
#'
#'   - `select()` is used to choose specific columns from a data frame.
#'
#'   - `vessel_official_number` specifies the first column to select.
#'
#'   - `!!hailing_port_col_name` uses the `rlang` bang-bang operator to evaluate the symbol stored in `hailing_port_col_name` to select the corresponding column.
#'
#' - `distinct()` removes duplicate rows from the resulting data frame:
#'
#'   - `distinct()` is used to retain only unique rows in a data frame.
#'
#' This code processes the `vessels_from_pims` data frame by renaming the `official__` column to `vessel_official_number`, selecting the `vessel_official_number` and the column identified by `hailing_port_col_name`, and removing duplicate rows. The resulting data frame is stored in `vessels_from_pims_short`.
#' 
vessels_from_pims_short <-
  vessels_from_pims |>
  dplyr::rename("vessel_official_number" = official__) |>
  select(vessel_official_number,
         !!hailing_port_col_name) |>
  distinct()

# print_df_names(vessels_from_pims)

dim(vessels_from_pims_short)
# [1] 23142     2

## vessels remove "NOVESID" ----
vessels_from_pims_short_ok <-
  vessels_from_pims_short |>
  dplyr::filter(!grepl("^NOVESID", vessel_official_number))

dim(vessels_from_pims_short_ok)
# [1] 22764     2

# View(vessels_from_pims_short_ok)

## vessel split double names ----
#'
#' Explanations:
#'
#' - `vessels_from_pims_short_ok__split1 <-` assigns the result of the pipeline to the variable `vessels_from_pims_short_ok__split1`.
#'
#' - `vessels_from_pims_short_ok |>` starts the pipeline with the data frame `vessels_from_pims_short_ok`.
#'
#' - `tidyr::separate(vessel_official_number, c('vessel_official_number', 'vessel_official_number2'), sep = " / ")` separates the `vessel_official_number` column into two new columns:
#'
#'   - `vessel_official_number` specifies the column to be split.
#'
#'   - `c('vessel_official_number', 'vessel_official_number2')` specifies the names of the new columns created by splitting the original column.
#'
#'   - `sep = " / "` specifies that the column should be split at occurrences of " / ".
#'
#' - `dplyr::mutate(dplyr::across(tidyselect::starts_with('vessel_official_number'), stringr::str_squish))` applies the `str_squish` function to columns starting with 'vessel_official_number':
#'
#'   - `dplyr::mutate()` creates new columns or modifies existing ones.
#'
#'   - `dplyr::across(tidyselect::starts_with('vessel_official_number'), stringr::str_squish)` applies the `str_squish` function from the `stringr` package across the columns whose names start with 'vessel_official_number'. `str_squish` trims leading and trailing whitespace and reduces internal multiple spaces to a single space.
#'
#' This code processes the `vessels_from_pims_short_ok` data frame by splitting the `vessel_official_number` column into two new columns (`vessel_official_number` and `vessel_official_number2`), then squishing the whitespace in these columns. The resulting data frame is stored in `vessels_from_pims_short_ok__split1`.
#' 
vessels_from_pims_short_ok__split1 <-
  vessels_from_pims_short_ok |>
  tidyr::separate(vessel_official_number,
           c('vessel_official_number', 'vessel_official_number2'),
           sep = " / ") |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with('vessel_official_number'),
                stringr::str_squish))

# Expected 2 pieces. Missing pieces filled with `NA` in 21895 rows [1, 2, 3, 4, 5,

# View(vessels_from_pims_short_ok__split1)

## vessel make one column of double names ----
### split into 2 dataframes and rename the id column ----

# [1] "vessel_official_number, vessel_official_number2, hailing_port"

# remove vessel_official_number2
vessels_from_pims_double_1 <-
  vessels_from_pims_short_ok__split1 |> 
  dplyr::select(-vessel_official_number2)

# keep only one vessel_official_number column, from vessel_official_number2
vessels_from_pims_double_2 <-
  vessels_from_pims_short_ok__split1 |> 
  dplyr::select(-vessel_official_number) |>
  dplyr::rename("vessel_official_number" = vessel_official_number2)

### combine in one df ----
vessels_from_pims_double_bind <-
  rbind(
    vessels_from_pims_double_1,
    vessels_from_pims_double_2
  ) |> 
  dplyr::distinct() |> 
  dplyr::filter(!is.na(vessel_official_number))

dim(vessels_from_pims_double_bind)
# [1] 23330     2

# Results ----
result_names <- 
  c("permit_info_from_db",
    "permits_from_pims__split1_short__split2",
    "vessels_from_pims_double_bind",
    "vessel_permit_owner_from_db")

auxfunctions::pretty_print(result_names, "data from PIMS:")
