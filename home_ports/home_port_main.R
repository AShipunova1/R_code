#' This code cleans homeport city and state from PIMS

# Setup ----

## Install packages if needed ----
needed_packages <- c(
  "tidyverse",
  "devtools", # Collection of package development tools
  "Hmisc" #to help with Label Attribute of an Object
)

#'
#' Explanations for the following code:
#'
#' - `needed_packages %in% rownames(installed.packages())` checks which packages from `needed_packages` are installed:
#'
#'   - `installed.packages()` returns a matrix of information about all installed packages.
#'
#'   - `rownames(installed.packages())` extracts the names of the installed packages.
#'
#'   - `needed_packages %in% ...` checks if each package in `needed_packages` is in the list of installed packages, returning a logical vector indicating the presence of each package.
#'
#' - `if (any(installed_packages == FALSE)) { ... }` checks if any package is not installed:
#'
#'   - `any(installed_packages == FALSE)` returns `TRUE` if at least one element in `installed_packages` is `FALSE`.
#'
#'   - `install.packages(packages[!installed_packages])` installs the packages that are not installed:
#'
#'     - `packages[!installed_packages]` selects the packages from `packages` that are not installed.
#'
#'     - `install.packages()` installs the selected packages.  

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

### Install helper functions for SEFHIER data analysis ----
#
# Explanations for the following code:
# 
# The installation details depend on the username.
# 
# For most users, install from the main branch if not already installed.
# 
# One doesn't have to have a GitHub account to use it.
# 
# For the package developer, install from the development branch.
#
# - `if (!require("auxfunctions"))` checks if the `auxfunctions` package is installed and loaded:
#
#   - `require("auxfunctions")` attempts to load the `auxfunctions` package.
#
#   - The `!` operator negates the result, so the condition is true if the package is not installed or cannot be loaded.
#
# - `devtools::install_github("AShipunova1/R_code/auxfunctions")` installs the `auxfunctions` package from the specified GitHub repository:
#
#   - `devtools::install_github()` is a function from the `devtools` package that installs an R package directly from a GitHub repository.
#
#   - `"AShipunova1/R_code/auxfunctions"` specifies the repository and subdirectory containing the package.
# 
# This code checks if the `auxfunctions` package is available, and if not, it installs it from the GitHub repository `AShipunova1/R_code/auxfunctions`.
# 

install_helper_functions <- function() {
  if (!auxfunctions::get_username() == "anna.shipunova") {
    if (!require('auxfunctions')) {
      devtools::install_github("AShipunova1/R_code/auxfunctions")
    }
  } else {
    # For a developer, rebuild the package from the development branch. To force the installation change to 'force = TRUE'
    devtools::install_github("AShipunova1/R_code/auxfunctions@development", force = FALSE)
    # restart R session to pick up changes
    # .rs.restartR()
    library(auxfunctions)
  }
}

install_helper_functions()

## Define dates ----

program_start_date <- lubridate::dmy("01/01/2021")

# Variables for the current year(s)
# Manually change if needed
my_years <- c("2022", "2023", "2024")

my_year_dates <-
  purrr::map(my_years, \(one_year) {
    my_beginning <- stringr::str_glue("{one_year}-01-01")
    my_end <- stringr::str_glue("{one_year}-12-31")
    
    res <- list(beg = my_beginning, end = my_end)
    return(res)
  })

names(my_year_dates) <- my_years

str(my_year_dates)

## Set up paths ----
#
# Different methods are used based on the user to accommodate different directory structure.
# 
# This allows the script to run correctly on multiple systems without manual path changes.
# 
# In the code in this section all user provided values have the word "manually" in the description. Everything else is created automatically.
#
# Manually: Change the following 2 lists (**my_paths** and **current_in_out_paths**) to your environment if needed. The variable _names_ are used throughout the code, so please change only the quoted _values_ inside the lists.
# 

# Check if the current username is not "anna.shipunova"
if (!auxfunctions::get_username() == "anna.shipunova") {
  auxfunctions::function_message_print(
    "Please CHANGE the following 2 lists values to your environment if needed. Use full path to your directories in quotes."
  )
  
  # 1) General directories (to look up additional files, e.g. processed data). It can be left as is if you don't have it. You can provide path to individual files later.
  my_paths <- list(inputs  = "~/my_inputs",
                   outputs = "~/my_outputs",
                   git_r   = "~/R_code")
  
  # 2) Current project code, input and output directories
  current_in_out_paths <-
    list(
      project_name = "home_port",
      code = "~/home_port/code",
      input = "~/home_port/input",
      output = "~/home_port/output"
    )
  
} else {
  # If the username is "anna.shipunova", use Anna's directory structure.
  my_paths <- auxfunctions::set_work_dir()
  current_in_out_paths <- auxfunctions::current_project_paths()
}

# The following section uses provided directory names lists to automatically create separate variables for future use and create current input/output directories if they do not exists.
# 
# 
# Create variables to store shortcuts to project directories
# 
# This is usually the current directory name.
current_project_name <- current_in_out_paths$project_name

current_project_path <- current_in_out_paths$code
            
current_project_input_path <- current_in_out_paths$input

current_project_output_path <- current_in_out_paths$output

# Create input and output directories if they don't exist
auxfunctions::create_dir_if_not(current_project_input_path)

auxfunctions::create_dir_if_not(current_project_output_path)

### Additional individual paths to data files ----
# This section sets up paths for specific data files used in the project

#### Vessels from PIMS ----
vessel_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Vessels - 2024-08-06_0819.xlsx)")

file.exists(vessel_names_file_path)

#### Permits from PIMS ----
permits_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Permits - 2024-08-06_0819.xlsx)")

file.exists(permits_names_file_path)

#' %%%%% Prepare data
#' 

# This is used only with source()
get_data_path <- 
  file.path(current_project_path, paste0(current_project_name, "_get_data.R"))

file.exists(get_data_path)

source(get_data_path)

#' Result:
#' 
#' vessels_from_pims_ok

# Fix city and state ----

## See errors ----
# View(vessels_from_pims_split_addr)

#' Numbers in hailing_port
#'
#' Explanations for the following code:
#'
#' - `dplyr::filter(grepl("\\d", hailing_port))` filters rows where `hailing_port` contains a digit:
#'
#'   - `grepl("\\d", hailing_port)` uses a regular expression to check for the presence of a digit (`\\d`) in the `hailing_port` column. `grepl` returns `TRUE` for rows where a digit is found and `FALSE` otherwise.
#'
#'   - `dplyr::filter(...)` keeps only the rows where the condition inside the `filter` function is `TRUE`.
#'
#' - `dplyr::distinct()` removes duplicate rows from the resulting data frame.
addresses_w_digit <- 
  vessels_from_pims_ok |>
  dplyr::filter(grepl("\\d", hailing_port)) |> 
  dplyr::distinct()
   # vessel_official_number hailing_port            
# 1                1040384                    2, AL
# 2               FL3407ML  xml:space="preserve">0,
# 3               FL8939JR xml:space="preserve">00,
# 4                 925240  xml:space="preserve">0,
# 5                 139403  xml:space="preserve">0,
# 6               SC4334DB  xml:space="preserve">0,

#' Extra commas in hailing_port
#'
#' Explanations for the following code:
#'
#' - `dplyr::filter(grepl(",.+,", hailing_port))` filters rows where `hailing_port` contains a pattern matching `,.+,`:
#'
#'   - `grepl(",.+,", hailing_port)` uses a regular expression to check for the presence of a comma followed by one or more characters and then another comma (`,.+,`) in the `hailing_port` column. `grepl` returns `TRUE` for rows where the pattern is found and `FALSE` otherwise.
#'
#'   - `dplyr::filter(...)` keeps only the rows where the condition inside the `filter` function is `TRUE`.
#'
#' - `dplyr::distinct()` removes duplicate rows from the resulting data frame. 
vessels_from_pims_ok |>
  dplyr::filter(grepl(",.+,", hailing_port)) |> 
  dplyr::distinct()
# 8811432134                 PEMBROKE, PINES, FL

## Find not acceptable characters in addresses ----

#'
#' Explanations for the following code:
#'
#' - `wrong_chars <- "[^A-Za-z0-9 .=',]"` assigns the regular expression pattern to the variable `wrong_chars`. This pattern matches any character that is not an uppercase letter (A-Z), lowercase letter (a-z), digit (0-9), space, period, equal sign, apostrophe, or comma.
#'
#' - `dplyr::filter(grepl(wrong_chars, hailing_port))` filters rows where `hailing_port` contains characters matching the `wrong_chars` pattern:
#'
#'   - `grepl(wrong_chars, hailing_port)` uses the regular expression pattern stored in `wrong_chars` to check for the presence of any character not included in the pattern in the `hailing_port` column. `grepl` returns `TRUE` for rows where such characters are found and `FALSE` otherwise.
#'
#'   - `dplyr::filter(...)` keeps only the rows where the condition inside the `filter` function is `TRUE`.
#'
#' - `dplyr::select(hailing_port)` selects only the `hailing_port` column from the filtered rows.
#'
#' - `dplyr::distinct()` removes duplicate rows from the resulting data frame, ensuring that each `hailing_port` value appears only once.
#' 
wrong_chars <- "[^A-Za-z0-9 .=',]"
vessels_from_pims_ok |> 
  dplyr::filter(grepl(wrong_chars, hailing_port)) |> 
  dplyr::select(hailing_port) |> 
  dplyr::distinct()
# 28

## Remove html ----
#'
#' Explanations for the following code:
#'
#' - `dplyr::mutate(hailing_port = ...)` modifies an existing column `hailing_port` with the specified transformation:
#'
#'   - `stringr::str_replace(hailing_port, 'xml:space=\"preserve\">', "")` replaces occurrences of the substring `'xml:space=\"preserve\">'` in the `hailing_port` column with an empty string:
#'
#'     - `stringr::str_replace` is a function from the `stringr` package that replaces the first instance of a pattern in a string.
#'
#'     - The pattern `'xml:space=\"preserve\">'` matches the exact sequence `xml:space="preserve">` in the `hailing_port` values.
#'
#'     - `""` is the replacement string, effectively removing the matched pattern from the `hailing_port` values.
vessels_from_pims_ok_no_html <-
  vessels_from_pims_ok |>
  dplyr::mutate(hailing_port =
           stringr::str_replace(hailing_port, 'xml:space=\"preserve\">', ""))

## Separate hailing_port into city and state ----
#'
#' Explanations for the following code:
#'
#' - `vessels_from_pims_split_addr <- ...` assigns the result of the operations to the variable `vessels_from_pims_split_addr`.
#'
#' - `vessels_from_pims_ok_no_html |>` pipes the `vessels_from_pims_ok_no_html` data frame to the subsequent functions.
#'
#' - `tidyr::separate_wider_delim(...)` splits the `hailing_port` column into multiple columns based on the specified delimiter:
#'
#'   - `hailing_port` specifies the column to be split.
#'
#'   - `delim = ","` specifies the delimiter used to split the column, which is a comma in this case.
#'
#'   - `names = c("city", "state")` specifies the names of the resulting columns after the split.
#'
#'   - `too_many = "merge"` specifies that if there are more pieces than column names, the extra pieces will be merged into the last column.
#'
#'   - `too_few = "align_start"` specifies that if there are fewer pieces than column names, the missing pieces will be aligned with the start of the column names.
#'
#' - `dplyr::mutate(dplyr::across(tidyselect::where(is.character), stringr::str_squish))` trims leading and trailing whitespace from character columns:
#'
#'   - `dplyr::mutate` is used to create or modify columns.
#'
#'   - `dplyr::across(tidyselect::where(is.character), stringr::str_squish)` applies the `stringr::str_squish` function to all character columns, removing any leading or trailing whitespace and reducing multiple spaces to a single space.
#'   
vessels_from_pims_split_addr <-
  vessels_from_pims_ok_no_html |>
  tidyr::separate_wider_delim(
    hailing_port,
    delim = ",",
    names = c("city", "state"),
    too_many = "merge",
    too_few = "align_start"
  ) |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.character), 
                              stringr::str_squish))

## fix known home port typos ----

#' This list is created manually

to_fix_list <- 
  list(c("117 HAWK LANDING LN#GA",
         "BRUNSWICK#GA"),
       c("22411 GENO LANE#AL",
         "GULF SHORES#AL"),
       c("ALEXANDER CITY, AL#AL",
         "ALEXANDER CITY#AL"),
       c("BAYOU LABATRE#AL",
         "BAYOU LA BATRE#AL"),
       c("CAROLINA BEACH#UN",
         "CAROLINA BEACH#NC"),
       c("CHALESTON#SC",
         "CHARLESTON#SC"),
       c("CHAUVIN, LA#LA",
         "CHAUVIN#LA"),
       c("CHAUVIN#LA, LA",
         "CHAUVIN#LA"),
       c("FERNADINA BCH#FL",
         "FERNANDINA BEACH#FL"),
       c("FIGURE 8 ISLAND#NC",
         "FIGURE EIGHT ISLAND#NC"),
       c("FORT MORGAN MARINA#AL",
         "FORT MORGAN#AL"),
       c("JACKSONVILLE#FL, UNITED STATES, FL",
         "JACKSONVILLE#FL"),
       c("GALLINANO#LA",
         "GALLIANO#LA"),
       c("GEORGRTOWN#SC",
         "GEORGETOWN#SC"),
       c("GULFSHORES#AL",
         "GULF SHORES#AL"),
       c("HILISBORO INLET#FL",
         "HILLSBORO INLET#FL"),
       c("HO0MASASSA#FL",
         "HOMOSASSA#FL"),
       c("HOMOASSA#FL",
         "HOMOSASSA#FL"),
       c("HOUMA LA#LA",
         "HOUMA#LA"),
       c("INTERCOASTAL CITY#LA",
         "INTRACOASTAL CITY#LA"),
       c("ISLAMORADA#UN",
         "ISLAMORADA#FL"),
       c("KEYWEST#FL",
         "KEY WEST#FL"),
       c("LITTLE RIVERNHV1N4WH#SC",
         "LITTLE RIVER#SC"),
       c("LOXLEY AL#AL",
         "LOXLEY#AL"),
       c("MADIERA BEACH#FL",
         "MADEIRA BEACH#FL"),
       c("MATLACHA#BOKKELIA, FL",
         "MATLACHA#FL"),
       c("MAYPPORT#FL",
         "MAYPORT#FL"),
       c("MCLELLANVILLE#SC",
         "MCCLELLANVILLE#SC"),
       c("MURELLS INLET#SC",
         "MURRELLS INLET#SC"),
       c("MURRELS INLET#SC",
         "MURRELLS INLET#SC"),
       c("NAPLE4S#FL",
         "NAPLES#FL"),
       c("NEW SMYMA BEACH#FL",
         "NEW SMYRNA BEACH#FL"),
       c("NEW SYMRNA BEACH#FL",
         "NEW SMYRNA BEACH#FL"),
       c("OCEEAN CITY#MD",
         "OCEAN CITY#MD"),
       c("PEMBROKE#PINES, FL",
         "PEMBROKE PINES#FL"),
       c("POINT PLEASANT NJ#NJ",
         "POINT PLEASANT#NJ"),
       c("PORT AERANSAS#TX",
         "PORT ARANSAS#TX"),
       c("PORT CANVERAL#FL",
         "PORT CANAVERAL#FL"),
       c("PORT O CANNOR#TX",
         "PORT O CONNOR#TX"),
       c("PORT OCONNOR#TX",
         "PORT O'CONNOR#TX"),
       c("PORT ST.LUICE#FL",
         "PORT ST LUCIE#FL"),
       c("PUNTA GORGA#FL",
         "PUNTA GORDA#FL"),
       c("REDINGTON SHORES#FL, FL",
         "REDINGTON SHORES#FL"),
       c("RIVERIA BEACH#FL",
         "RIVIERA BEACH#FL"),
       c("S PADRE ISLE#TX",
         "S. PADRE ISLAND#TX"),
       c("SEBASTAIN#FL",
         "SEBASTIAN#FL"),
       c("ST AUGUSTIN#FL",
         "ST AUGUSTINE#FL"),
       c("ST PETERSBURG BEACH#FL",
         "ST PETERSBURG#FL"),
       c("STEINAHTCHEE#FL",
         "STEINHATCHEE#FL"),
       c("SUMMRLND KEY#FL",
         "SUMMERLAND KEY#FL"),
       c("SWANQUARTER#FL",
         "SWAN QUARTER#NC"),
       c("TAVENIER#FL",
         "TAVERNIER#FL"),
       c("WANCHEESE#NC",
         "WANCHESE#NC"))

# ---
#'
#' Explanations for the following code:
#'
#' - `dplyr::mutate(city_state = ...)` creates a new column `city_state` with the specified transformation:
#'
#'   - `paste(trimws(city), trimws(state), sep = "#")` concatenates the `city` and `state` columns with a `#` separator:
#'
#'     - `trimws(city)` removes any leading or trailing whitespace from the `city` column values.
#'
#'     - `trimws(state)` removes any leading or trailing whitespace from the `state` column values.
#'
#'     - `paste(..., sep = "#")` concatenates the `city` and `state` values, separating them with a `#` character.
#'     
vessels_from_pims_split_addr__city_state <-
  vessels_from_pims_split_addr |>
  dplyr::mutate(city_state =
           paste(
             trimws(city),
             trimws(state),
             sep = "#"
           ))


### check numbers in an address again with the pound ----
vessels_from_pims_split_addr__city_state |>
  dplyr::filter(grepl("\\d", city_state)) |> 
  dplyr::select(city_state) |> 
  dplyr::distinct()
# 1 2#AL      
# 2 0#        
# 3 00#       

#' extra commas in city, state 
vessels_from_pims_split_addr__city_state |>
  dplyr::filter(grepl(",", city_state)) |> 
  dplyr::select(city_state)
# PEMBROKE#PINES, FL   

### Get wrong addresses only ----
#' Explanations for the following code:
#' 
#' 1. **Column Extraction Using sapply:**
#'
#'    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#'
#'    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
#'
#' 2. **Column Extraction Using '[':**
#'
#'    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#'
#'    - This operation is used to extract a specific column or element from each list or data frame within 'to_fix_list'.
#'
#' 3. **Final Result:**
#'
#'    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

### Fix addresses from the list ----
#'
#' Explanations for the following code:
#'
#' The function 'get_correct_addr_by_wrong' takes a 'wrong_addr' as input and performs the following steps:
#'
#' 1. Finds the index of 'wrong_addr' in the 'to_fix_list'.
#'
#' 2. Uses 'tryCatch' to handle errors, printing information about the error and the index if one occurs.
#'
#' 3. Extracts the correct address from the pair.
#'
#' 4. Returns the correct address.
#' 
get_correct_addr_by_wrong <-
  function(wrong_addr) {
    idx <- grep(wrong_addr, to_fix_list)
    
    names_pair <-
      tryCatch(
        to_fix_list[[idx]],
        error = function(e) {
          print(e)
          print(stringr::str_glue("Index: {idx}"))
        }
      )
    good_addr <- names_pair[[2]]
    
    return(good_addr)
  }

#'
#' Explanations for the following code:
#'
#' The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed' is created by:
#'
#' 1. Creating a new column 'city_state_fixed' by replacing wrong addresses using 'get_correct_addr_by_wrong' for rows where 'city_state' is in 'wrong_port_addr'.
#'
#' 2. Separating the 'city_state_fixed' column into two columns ('city_fixed' and 'state_fixed') using '#' as the delimiter.
#' 
vessels_from_pims_split_addr__city_state__fix1 <-
  vessels_from_pims_split_addr__city_state |>
  dplyr::rowwise() |>
  dplyr::mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  dplyr::ungroup() |>
  tidyr::separate_wider_delim(city_state_fixed,
                              delim = "#",
                              names = c("city_fixed",
                                        "state_fixed")) |> 
  dplyr::distinct()

dplyr::n_distinct(vessels_from_pims_split_addr__city_state__fix1$vessel_official_number)
# [1] 23045

## Add more fixes manually ----

### Define filters ----
lt_6_id_len_filter <- 
  rlang::quo(id_len < 6)

gt_8_id_len_filter <- 
  rlang::quo(id_len > 8)

non_alphanumeric_filter <- 
  rlang::quo(grepl("[^A-Za-z0-9]", vessel_official_number))

is_empty <- c(NA, "NA", "", "UN", "N/A")
is_empty_filter <-
  rlang::quo(vessel_official_number %in% is_empty)

wrong_vessel_ids <- c("FL", "FLORIDA", "MD", "NO", "NONE")
weird_vessel_ids_filter <-
  rlang::quo(vessel_official_number %in% wrong_vessel_ids)

filter_list <- Hmisc::llist(
  lt_6_id_len_filter, 
  gt_8_id_len_filter, 
  non_alphanumeric_filter, 
  is_empty_filter, 
  weird_vessel_ids_filter
)

### Find empty and bad vessel ids ----

#' 
#' Add vessel id length 
vessels_from_pims_split_addr__city_state__fix1_ids_len <-
  vessels_from_pims_split_addr__city_state__fix1 |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(id_len = stringr::str_length(vessel_official_number)) |>
  dplyr::ungroup()

### Get "bad" vessel ids ----

#'
#' Explanations for the following code:
#'
#' - `bad_vessel_ids <- ...` assigns the result of the operations to the variable `bad_vessel_ids`.
#'
#' - `purrr::map(filter_list, \(curr_filter) { ... })` applies the anonymous function to each element in `filter_list`:
#'
#'   - `filter_list` is a list of filter conditions.
#'
#'   - `\(curr_filter)` defines an anonymous function with `curr_filter` as its argument.
#'
#' - Inside the anonymous function:
#'
#'   - `vessels_from_pims_split_addr__city_state__fix1_ids_len |>` pipes the `vessels_from_pims_split_addr__city_state__fix1_ids_len` data frame to the subsequent functions.
#'
#'   - `dplyr::filter(!!curr_filter)` filters rows based on the current filter condition:
#'
#'     - `!!curr_filter` evaluates the `curr_filter` expression.
#'
#'   - `dplyr::select(id_len, vessel_official_number)` selects the `id_len` and `vessel_official_number` columns.
#'
#'   - `dplyr::arrange(dplyr::desc(id_len))` sorts the rows in descending order by the `id_len` column:
#'
#'     - `dplyr::desc(id_len)` specifies descending order for `id_len`.
#'
#'   - `dplyr::distinct()` removes duplicate rows from the result.
#'
#' - The `purrr::map` function returns a list of data frames, each containing filtered and processed rows based on the respective filter condition from `filter_list`.
#' 
bad_vessel_ids <-
  purrr::map(filter_list, \(curr_filter) {
    vessels_from_pims_split_addr__city_state__fix1_ids_len |>
      dplyr::filter(!!curr_filter) |>
      dplyr::select(id_len, vessel_official_number) |>
      dplyr::arrange(dplyr::desc(id_len)) |>
      dplyr::distinct()
  })

str(bad_vessel_ids)

### Write weird ids to Google Drive ----
write_to_google <- 
  function() {
    
    new_ss_info <-
      googlesheets4::gs4_create(
        name = stringr::str_glue("weird_vessel_ids_{lubridate::now()}"),
        sheets = bad_vessel_ids
      )
    
    out_dir_ss <-
      auxfunctions::get_google_drive_folder_by_name(google_drive_project_name = "Anna's tidbits")
    
    new_file_ss_info <-
      googledrive::drive_mv(
        new_ss_info,
        path = out_dir_ss,
        name = stringr::str_glue("weird_vessel_ids_{lubridate::today()}")
      )
    
    return(new_file_ss_info)
  }

# Uncomment to run
# new_file_ss_info <- write_to_google()

# to see the result in the browser
# Uncomment to run
# googledrive::drive_browse(new_file_ss_info)

### List of vessels with double ports ----
# Keep the correct addresses only (from Jeannette)

manual_fixes_double_ports <-
  list(
    list("1112053", "NEW BERN", "NC"),
    list("1166732", "MIAMI", "FL"),
    list("1185107", "KEY WEST", "FL"),
    list("531549", "TOWNSEND", "GA"),
    list("581260", "PONCE INLET", "FL"),
    list("596153", "NEW BERN", "NC"),
    list("646818", "HOUSTON", "TX"),
    list("671353", "SWANSBORO", "NC"),
    list("FL0146BH", "MIAMI", "FL"),
    list("FL1431JU", "MARATHON", "FL"),
    list("FL1553TM", "BILOXI", "MS"),
    list("FL1862SU", "MIAMI", "FL"),
    list("FL2615MT", "STUART", "FL"),
    list("FL3119EE", "BOCA GRANDE", "FL"),
    list("FL3976FH", "PONCE INLET", "FL"),
    list("FL5011MX", "NAPLES", "FL"),
    list("FL5029RM", "KEY WEST", "FL"),
    list("FL5262LD", "LAUDERDALE BY THE SEA", "FL"),
    list("FL7549PJ", "KEY LARGO", "FL"),
    list("FL8000NR", "ST PETERSBURG BEACH", "FL"),
    list("FL8252JK", "MIAMI", "FL"),
    list("LA4017BH", "HACKBERRY", "LA"),
    list("LA6968EP", "LAROSE", "LA"),
    list("NC6164CW", "MOREHEAD CITY", "NC"),
    list("TX9606KA", "HOUSTON", "TX")
  )
    # list("139403", "MIAMI", "FL"), # no! Keep it here as a reminder

### Fix double ports ----
# 
#'
#' Explanations for the following code:
#'
#' - `vessels_from_pims_split_addr__city_state__fix2 <- ...` assigns the result of the operations to the variable `vessels_from_pims_split_addr__city_state__fix2`.
#'
#' - `purrr::map_df(manual_fixes_double_ports, \(x) { ... })` applies the anonymous function to each element in `manual_fixes_double_ports` and returns a combined data frame:
#'
#'   - `manual_fixes_double_ports` is a list of manual fix rules.
#'
#'   - `\(x)` defines an anonymous function with `x` as its argument.
#'
#' - Inside the anonymous function:
#'
#'   - `vessels_from_pims_split_addr__city_state__fix1 |>` pipes the `vessels_from_pims_split_addr__city_state__fix1` data frame to the subsequent functions.
#'
#'   - `dplyr::mutate(...)` creates new columns based on conditions:
#'
#'     - `city_fixed1 = dplyr::case_when(vessel_official_number == x[[1]] ~ x[[2]])` creates the `city_fixed1` column:
#'
#'       - `dplyr::case_when(vessel_official_number == x[[1]] ~ x[[2]])` assigns `x[[2]]` to `city_fixed1` where `vessel_official_number` matches `x[[1]]`.
#'
#'     - `state_fixed1 = dplyr::case_when(vessel_official_number == x[[1]] ~ x[[3]])` creates the `state_fixed1` column:
#'
#'       - `dplyr::case_when(vessel_official_number == x[[1]] ~ x[[3]])` assigns `x[[3]]` to `state_fixed1` where `vessel_official_number` matches `x[[1]]`.
#'
#'   - `return(res)` returns the modified data frame.
#'
#' - The `purrr::map_df` function combines the results into a single data frame.
#'
#' - `dplyr::distinct()` removes duplicate rows from the combined data frame.
#' 
vessels_from_pims_split_addr__city_state__fix2 <-
  purrr::map_df(manual_fixes_double_ports,
         \(x) {
           # browser()
           res <-
             vessels_from_pims_split_addr__city_state__fix1 |>
             dplyr::mutate(
               city_fixed1 =
                 dplyr::case_when(vessel_official_number == x[[1]] ~ x[[2]]),
               state_fixed1 =
                 dplyr::case_when(vessel_official_number == x[[1]] ~ x[[3]])
             )
           return(res)
         }) |>
  dplyr::distinct()

dim(vessels_from_pims_split_addr__city_state__fix2)
# [1] 23109     8

## Replace duplicated values ----
#'
#' Explanations for the following code:
#'
#' 1. Updating 'city_fixed' and 'state_fixed' columns based on conditions using 'case_when':
#'
#'     - If 'city_fixed1' is not NA, update 'city_fixed' with 'city_fixed1'; otherwise, keep the existing value in 'city_fixed'.
#'
#'     - If 'state_fixed1' is not NA, update 'state_fixed' with 'state_fixed1'; otherwise, keep the existing value in 'state_fixed'.
#'
#' 2. Filtering rows where 'vessel_official_number' is not in 'both' or 'state_fixed1' is not missing.
#'
#' 3. Selecting all columns except "city_fixed1" and "state_fixed1".
#'
#' 4. Keeping only distinct rows in the final result to avoid duplications.
#' 
vessels_from_pims_split_addr__city_state__fix2_ok <-
  vessels_from_pims_split_addr__city_state__fix2 |>
  dplyr::mutate(
    city_fixed =
      dplyr::case_when(!is.na(city_fixed1) ~ city_fixed1,
                .default = city_fixed),
    state_fixed =
      dplyr::case_when(!is.na(state_fixed1) ~ state_fixed1,
                .default = state_fixed)
  ) |> 
  dplyr::select(-c("city_fixed1", "state_fixed1")) |>
  dplyr::distinct()

# View(vessels_from_pims_split_addr__city_state__fix2_ok)

dim(vessels_from_pims_split_addr__city_state__fix2_ok)
# [1] 23086     6

## Check no address ----
#' No city
vessels_from_pims_split_addr__city_state__fix2_ok__no_addr <-
  vessels_from_pims_split_addr__city_state__fix2_ok |>
  dplyr::filter(is.na(city))

nrow(vessels_from_pims_split_addr__city_state__fix2_ok__no_addr)
# 6
# 0 OK

#' No state
vessels_from_pims_split_addr__city_state__fix2_ok__no_state <-
  vessels_from_pims_split_addr__city_state__fix2_ok |>
  dplyr::filter(is.na(state_fixed))

nrow(vessels_from_pims_split_addr__city_state__fix2_ok__no_state)
# 0 OK

# remove extra cols ----
vessels_from_pims_split_addr__city_state__fix2_ok_short <-
  vessels_from_pims_split_addr__city_state__fix2_ok |>
  dplyr::select(vessel_official_number, 
                tidyselect::ends_with("_fixed")) |> 
  dplyr::distinct()

# check
# vessels_from_pims_split_addr__city_state__fix2_ok |> 
#   filter(!state == state_fixed) |> 
#   View()

vessels_from_pims_split_addr__city_state__fix2_ok |>
  dplyr::filter(!city == city_fixed) |>
  dplyr::select(-vessel_official_number) |> 
  dplyr::distinct() |> 
  nrow()
# 47
# 50
# 55

## check for double ids/ports ----
double_ids_ports <-
  vessels_from_pims_split_addr__city_state__fix2_ok_short |>
  dplyr::distinct() |>
  dplyr::select(vessel_official_number) |>
  dplyr::count(vessel_official_number) |>
  dplyr::filter(n > 1)

nrow(double_ids_ports)
# 0, ok
# 3
# 1
# 22

double_ports_1 <-
  purrr::map(manual_fixes_double_ports, \(curr_list) {
    as.data.frame(t(unlist(curr_list)))
  }) |>
  purrr::list_rbind()

names(double_ports_1) <- 
  c("vessel_official_number", "city", "state")

double_ports_1 |>
  dplyr::arrange(vessel_official_number) |>
  dplyr::glimpse()

# check
vessels_from_pims_split_addr__city_state__fix2_ok_short |>
  dplyr::filter(vessel_official_number %in% double_ids_ports$vessel_official_number) |>
  dplyr::arrange(vessel_official_number) |> 
  dplyr::filter(vessel_official_number %in% double_ports_1$vessel_official_number) |> 
  dplyr::glimpse()
# 1 PT. CANAVERAL 
# 2 PORT CANAVERAL

# print out ----

out_path <- file.path(current_project_output_path,
            stringr::str_glue("vessels_from_pims_ports_{lubridate::today()}.csv"))

readr::write_csv(
  vessels_from_pims_split_addr__city_state__fix2_ok_short,
  out_path
)

# TODO:
# Write to google drive

# View(vessels_from_pims_split_addr__city_state__fix2_ok_short)

# ===
#' Do weird vessels have permits?
#'
#' Explanations for the following code:
#'
#' - `weird_vessel_ids_only <- ...` assigns the result of the operations to the variable `weird_vessel_ids_only`.
#'
#' - `purrr::map(bad_vessel_ids, dplyr::bind_rows)` applies the `dplyr::bind_rows` function to each element in `bad_vessel_ids`:
#'
#'   - `bad_vessel_ids` is a list of data frames.
#'
#'   - `dplyr::bind_rows` combines the rows of each data frame in the list.
#'
#' - `dplyr::bind_rows(.id = "list_name")` combines all the data frames into a single data frame:
#'
#'   - `.id = "list_name"` adds a column named `list_name` to indicate the source list for each row.
#'
#' - `dplyr::select(vessel_official_number)` selects the `vessel_official_number` column.
#'
#' - `dplyr::distinct()` removes duplicate rows based on the `vessel_official_number` column.
#' 
weird_vessel_ids_only <-
  purrr::map(bad_vessel_ids, dplyr::bind_rows) |>
  dplyr::bind_rows(.id = "list_name") |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

permits_from_pims__split1 |> 
  dplyr::filter(vessel_official_number %in% weird_vessel_ids_only |
           dealer %in% weird_vessel_ids_only) |> 
  nrow()
# No permits for those vessel ids

# Permits' vessel ids ----
permits_from_pims__split1 |> 
  dplyr::select(vessel_official_number) |> 
  dplyr::distinct() |> 
  dplyr::arrange(vessel_official_number) |> 
  dplyr::glimpse()

permits_from_pims__split1_short__split2__id_len <-
  permits_from_pims__split1_short__split2 |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(id_len = stringr::str_length(vessel_official_number)) |>
  dplyr::ungroup()

#'
#' Filter weird vessel ids from permits
#'
#' Explanations for the following code:
#'
#' - `bad_vessel_ids_from_permits <- ...` assigns the result of the operations to the variable `bad_vessel_ids_from_permits`.
#'
#' - `purrr::map(filter_list, \(curr_filter) { ... })` applies the anonymous function to each element in `filter_list` and returns a list of data frames:
#'
#'   - `filter_list` is a list of filter conditions.
#'
#'   - `\(curr_filter)` defines an anonymous function with `curr_filter` as its argument.
#'
#' - Inside the anonymous function:
#'
#'   - `permits_from_pims__split1_short__split2__id_len |>` pipes the `permits_from_pims__split1_short__split2__id_len` data frame to the subsequent functions.
#'
#'   - `dplyr::filter(!!curr_filter)` filters the rows based on the current filter condition (`curr_filter`):
#'
#'     - `!!curr_filter` evaluates the current filter condition using non-standard evaluation.
#'
#'   - `dplyr::select(id_len, vessel_official_number)` selects the `id_len` and `vessel_official_number` columns.
#'
#'   - `dplyr::arrange(dplyr::desc(id_len))` sorts the rows in descending order based on the `id_len` column.
#'
#'   - `dplyr::distinct()` removes duplicate rows from the data frame.
#'
#' - The `purrr::map` function returns a list of data frames, each containing filtered and processed data based on the respective filter condition from `filter_list`.
#' 
bad_vessel_ids_from_permits <-
  purrr::map(filter_list, \(curr_filter) {
    permits_from_pims__split1_short__split2__id_len |>
      dplyr::filter(!!curr_filter) |>
      dplyr::select(id_len, vessel_official_number) |>
      dplyr::arrange(dplyr::desc(id_len)) |>
      dplyr::distinct()
  })

dplyr::glimpse(bad_vessel_ids_from_permits)

#'
#' Get ids only
#'
#' Explanations for the following code:
#'
#' - `weird_vessel_ids_permits_ids_only <- ...` assigns the result of the operations to the variable `weird_vessel_ids_permits_ids_only`.
#'
#' - `purrr::map(bad_vessel_ids_from_permits, dplyr::bind_rows)` applies the `dplyr::bind_rows` function to each element in `bad_vessel_ids_from_permits`:
#'
#'   - `bad_vessel_ids_from_permits` is a list of data frames.
#'
#'   - `dplyr::bind_rows` combines the rows of each data frame in the list.
#'
#' - `dplyr::bind_rows(.id = "list_name")` combines all the data frames into a single data frame:
#'
#'   - `.id = "list_name"` adds a column named `list_name` to indicate the source list for each row.
#'
#' - `dplyr::select(vessel_official_number)` selects the `vessel_official_number` column.
#'
#' - `dplyr::distinct()` removes duplicate rows based on the `vessel_official_number` column.
#' 
weird_vessel_ids_permits_ids_only <-
  purrr::map(bad_vessel_ids_from_permits, 
             dplyr::bind_rows) |>
  dplyr::bind_rows(.id = "list_name") |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

# View(weird_vessel_ids_permits_ids_only)

# write to google drive ids only

add_to_google_ss_ids_only <-
  function() {
    
    googlesheets4::write_sheet(weird_vessel_ids_permits_ids_only,
                               ss = new_file_ss_info,
                               sheet = "ids from Permits")
    googlesheets4::sheet_relocate(ss = new_file_ss_info,
                                  sheet = "ids from Permits",
                                  .before = 1)
    googlesheets4::write_sheet(weird_vessel_ids_only, ss = new_file_ss_info, sheet = "ids from Vessels")
    
    googlesheets4::sheet_relocate(ss = new_file_ss_info,
                                  sheet = "ids from Vessels",
                                  .before = 1)
  }

# uncomment to run
# add_to_google_ss_ids_only()