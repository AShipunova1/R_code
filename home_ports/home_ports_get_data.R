# today()
# [1] "2024-02-28"

# 1) "Permits - 2024-02-28_0930.xlsx"
# get it from PIMS
# Menu: permits
# Filter:
# Fishery = RCG - Gulf Charter/headboat For Reef Fish, CHG - Gulf Charter/headboat For Coastal Migratory Pelagic Fish, SC - South Atlantic Charter/headboat For Snapper-grouper, CHS - Atlantic Charter/headboat For Coastal Migratory Pelagics, HCHG - Historical Captain Gulf Charter/headboat For Coastal Migratory Pelagic Fish, HRCG - Historical Captain Gulf Charter/headboat For Reef Fish, CDW - Atlantic Charter/headboat For Dolphin/wahoo
# 
# download
# 
# skip first 5 lines in R)

# 2) same for vessels, skip first 3 lines
# "Vessels - 2024-02-28_0930.xlsx"

library(openxlsx)

# Colored terminal output
library(crayon)

# auxiliary functions ----
get_xlsx_data_pims <-
  function(my_file_path,
           startRow = 1,
           my_sheet = "Sheet 1") {
    
    data_from_pims_raw <-
      openxlsx::read.xlsx(my_file_path,
                sheet = my_sheet,
                startRow = startRow,
                detectDates = TRUE)
    
    # clean_headers
    data_from_pims <-
      data_from_pims_raw %>%
      auxfunctions::clean_headers()
    
    return(data_from_pims)
  }

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

# file.exists(vessel_names_file_path)
vessels_from_pims <- 
  get_xlsx_data_pims(vessel_names_file_path,
                     startRow = 4)

# print_df_names(vessels_from_pims)

# glimpse(vessels_from_pims)
# dim(vessels_from_pims)
# [1] 23107     8

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

# upload permits from pims ----
permits_names_file_path <-
  file.path(my_paths$inputs,
            r"(from_PIMS\Permits - 2024-06-18_0839.xlsx)")

file.exists(permits_names_file_path)
permits_from_pims <- 
  get_xlsx_data_pims(permits_names_file_path,
                     startRow = 5)

# Clean data ----
vessel_or_dealer_col_name <- 
  auxfunctions::find_col_name(permits_from_pims, "vessel", "dealer") |> 
  rlang::sym()
# print_df_names(permits_from_pims)

## permits split vessel_or_dealer ----
permits_from_pims__split1 <-
  permits_from_pims |>
  tidyr::separate(!!vessel_or_dealer_col_name,
           c('vessel_official_number', 'dealer'),
           sep = " / ") |>
  dplyr::mutate(dplyr::across(c('vessel_official_number', 'dealer'),
                stringr::str_squish))

# View(permits_from_pims__split1)
# Expected 2 pieces. Missing pieces filled with `NA` in 3038 rows [229, 244, 294,

# permits_from_pims[229,] |> 
#   glimpse()
# $ vessel_or_dealer <chr> "BARNACLE SEAFOOD INC"

## permits clean and shorten ----
Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

program_start_date <- lubridate::dmy("04/01/2021")

permits_from_pims__split1_short <-
  permits_from_pims__split1 |>
  convert_to_dates() |> 
  dplyr::filter(dplyr::if_any(tidyselect::ends_with("date"), 
                ~ . > program_start_date))

dim(permits_from_pims)
# [1] 53365    11

dim(permits_from_pims__split1_short)
# [1] 31180    12

## permits split permit number ----
permits_from_pims__split1_short__split2 <- 
  permits_from_pims__split1_short |> 
  tidyr::separate(permit_,
           c('permit', 'permit_number'),
           sep = "-") |>
  dplyr::mutate(dplyr::across(tidyselect::starts_with('permit'),
                stringr::str_squish))
# View(permits_from_pims__split1_short__split2)

## vessels clean and shorten  ----
hailing_port_col_name <- 
  auxfunctions::find_col_name(vessels_from_pims, "hailing", "port") |>
  rlang::sym()

vessels_from_pims_short <-
  vessels_from_pims |>
  dplyr::rename("vessel_official_number" = official_) |>
  select(vessel_official_number,
         !!hailing_port_col_name) |>
  distinct()

# print_df_names(vessels_from_pims)

dim(vessels_from_pims_short)
# [1] 22887     2

## vessels remove "NOVESID" ----
vessels_from_pims_short_ok <-
  vessels_from_pims_short |>
  dplyr::filter(!grepl("^NOVESID", vessel_official_number))

dim(vessels_from_pims_short_ok)
# [1] 22510     2

# View(vessels_from_pims_short_ok)

## vessel split double names ----
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

vessels_from_pims_double_1 <-
  vessels_from_pims_short_ok__split1 |> 
  dplyr::select(-vessel_official_number2)

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
# [1] 23086     2

## Clean vessel home port punctuation ----

vessels_from_pims_ok <-
  vessels_from_pims_double_bind |>
  dplyr::mutate(hailing_port =
           stringr::str_replace(!!hailing_port_col_name,
                       ",",
                       ", ")) |>
  dplyr::mutate(hailing_port =
           stringr::str_replace(!!hailing_port_col_name,
                       " ,",
                       ",")) |>
  dplyr::mutate(hailing_port =
           stringr::str_squish(!!hailing_port_col_name)) |> 
  distinct()

dim(vessels_from_pims_ok)
# [1] 23086     2

# check
grep(",[a-zA-Z]",
     vessels_from_pims_ok$hailing_port,
     value = T)
# 0
# [1] "PEMBROKE,PINES, FL"

grep("  +",
     vessels_from_pims_ok$hailing_port,
     value = T)
# 0

