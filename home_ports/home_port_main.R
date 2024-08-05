# This code cleans homeport city and state from PIMS
# 1 Setup
# 1.1 Install packages if needed
# 1.2 Define dates
# 1.3 Set up paths
# 1.4 Other setup

# 2 Prepare data
# 2.1 Load data
# 2.2 Data cleanup

# 3 Clean home ports
# 3.1 

# Setup ----

## Install packages if needed ----
needed_packages <- c(
  "tidyverse",
  "devtools", # Collection of package development tools
  "Hmisc" #to help with Label Attribute of an Object
)

# Explanations for the following code:
# - `needed_packages %in% rownames(installed.packages())` checks which packages from `needed_packages` are installed:
#   - `installed.packages()` returns a matrix of information about all installed packages.
#   - `rownames(installed.packages())` extracts the names of the installed packages.
#   - `needed_packages %in% ...` checks if each package in `needed_packages` is in the list of installed packages, returning a logical vector indicating the presence of each package.
# - `if (any(installed_packages == FALSE)) { ... }` checks if any package is not installed:
#   - `any(installed_packages == FALSE)` returns `TRUE` if at least one element in `installed_packages` is `FALSE`.
#   - `install.packages(packages[!installed_packages])` installs the packages that are not installed:
#     - `packages[!installed_packages]` selects the packages from `packages` that are not installed.
#     - `install.packages()` installs the selected packages.  

installed_packages <-
  needed_packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(needed_packages[!installed_packages])
}

# Install helper functions for SEFHIER data analysis.
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
            r"(from_PIMS\Vessels - 2024-07-30_1139.xlsx)")

file.exists(vessel_names_file_path)

#### Permits from PIMS ----
permits_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Permits - 2024-07-30_1139.xlsx)")

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
addresses_w_digit <- 
  vessels_from_pims_ok |>
  filter(grepl("\\d", hailing_port)) |> 
  distinct()
   # vessel_official_number hailing_port            
#    <chr>                  <chr>                   
#  1 574500                 HO0MASASSA, FL          
#  2 1040384                2, AL                   
#  3 NC6421AU               FIGURE 8 ISLAND, NC     
#  4 FL3407ML               0,                      
#  5 FL8939JR               00,                     
#  6 925240                 0,                      
#  7 FL5011MX               NAPLE4S, FL             
#  8 SC8023DE               LITTLE RIVERNHV1N4WH, SC
#  9 139403                 0,                      
# 10 DO552832               0,                      
# 11 1301930                22411 GENO LANE, AL     
# 12 GA1769JL               117 HAWK LANDING LN, GA 

#' Extra commas in hailing_port
vessels_from_pims_ok |>
  filter(grepl(",.+,", hailing_port)) |> 
  distinct()
# 1                 945114            REDINGTON SHORES, FL, FL
# 2                 919225                     CHAUVIN, LA, LA
# 3               FL0702JJ              MATLACHA, BOKKELIA, FL
# 4             8811432134                 PEMBROKE, PINES, FL
# 5               FL7047TR JACKSONVILLE, FL, UNITED STATES, FL

## Find not acceptable characters in addresses ----

wrong_chars <- "[^A-Za-z0-9 .=',]"
vessels_from_pims_ok |> 
  filter(grepl(wrong_chars, hailing_port)) |> 
  select(hailing_port) |> 
  distinct()

## Remove html ----
vessels_from_pims_ok_no_html <-
  vessels_from_pims_ok |>
  mutate(hailing_port =
           stringr::str_replace(hailing_port, 'xml:space=\"preserve\">', ""))

## Separate hailing_port into city and state ----
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
#' Explanations:
#' Creating a new column 'city_state' by concatenating trimmed 'city' and 'state' columns, separated by '#'.
vessels_from_pims_split_addr__city_state <-
  vessels_from_pims_split_addr |>
  mutate(city_state =
           paste(
             trimws(city),
             trimws(state),
             sep = "#"
           ))


### check numbers in an address again with the "#" ----
vessels_from_pims_split_addr__city_state |>
  filter(grepl("\\d", city_state)) |> 
  select(city_state) |> 
  distinct()
# 1 HO0MASASSA#FL          
# 2 2#AL                   
# 3 FIGURE 8 ISLAND#NC     
# 4 0#                     
# 5 00#                    
# 6 NAPLE4S#FL             
  # 7 LITTLE RIVERNHV1N4WH#SC (fixed)
# 8 22411 GENO LANE#AL     
# 9 117 HAWK LANDING LN#GA 

#' extra commas in city, state 
vessels_from_pims_split_addr__city_state |>
  filter(grepl(",", city_state)) |> 
  select(city_state)
# REDINGTON SHORES#FL, FL
# CHAUVIN#LA, LA         
# ALEXANDER CITY#AL, AL  (fixed)
# MATLACHA#BOKKELIA, FL  
# PEMBROKE#PINES, FL   
# JACKSONVILLE#FL, UNITED STATES, FL

### Get wrong addresses only ----
#' Explanations:
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
#' Explanations:
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
          print(str_glue("Index: {idx}"))
        }
      )
    good_addr <- names_pair[[2]]
    
    return(good_addr)
  }

#'
#' Explanations:
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

new_file_ss_info <- write_to_google()

# to see the result in the browser
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
#' Explanations:
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
# 0

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

#' check
# vessels_from_pims_split_addr__city_state__fix2_ok |> 
#   filter(!state == state_fixed) |> 
#   View()

vessels_from_pims_split_addr__city_state__fix2_ok |>
  filter(!city == city_fixed) |>
  select(-vessel_official_number) |> 
  distinct() |> 
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
  arrange(vessel_official_number) |>
  glimpse()

# check
vessels_from_pims_split_addr__city_state__fix2_ok_short |>
  filter(vessel_official_number %in% double_ids_ports$vessel_official_number) |>
  arrange(vessel_official_number) |> 
  filter(vessel_official_number %in% double_ports_1$vessel_official_number) |> 
  glimpse()
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

View(vessels_from_pims_split_addr__city_state__fix2_ok_short)

# ===
# Do weird vessels have permits

weird_vessel_ids_only <-
  purrr::map(bad_vessel_ids, bind_rows) |>
  bind_rows(.id = "list_name") |>
  select(vessel_official_number) |>
  distinct()

permits_from_pims__split1 |> 
  filter(vessel_official_number %in% weird_vessel_ids |
           dealer %in% weird_vessel_ids) |> 
  nrow()
# No permits for those vessel ids

# Permits' vessel ids ----
permits_from_pims__split1 |> 
  select(vessel_official_number) |> 
  distinct() |> 
  arrange(vessel_official_number) |> 
  glimpse()

permits_from_pims__split1_short__split2__id_len <-
  permits_from_pims__split1_short__split2 |>
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(id_len = stringr::str_length(vessel_official_number)) |>
  dplyr::ungroup()

# filter weird vessel ids from permits
bad_vessel_ids_from_permits <-
  purrr::map(filter_list, \(curr_filter) {
    permits_from_pims__split1_short__split2__id_len |>
      dplyr::filter(!!curr_filter) |>
      dplyr::select(id_len, vessel_official_number) |>
      dplyr::arrange(dplyr::desc(id_len)) |>
      dplyr::distinct()
  })

glimpse(bad_vessel_ids_from_permits)

# get ids only
weird_vessel_ids_permits_ids_only <-
  purrr::map(bad_vessel_ids_from_permits, bind_rows) |>
  bind_rows(.id = "list_name") |>
  select(vessel_official_number) |>
  distinct()

# View(weird_vessel_ids_permits_ids_only)
