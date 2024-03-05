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

# Colored terminal output
library(crayon)

# auxiliary functions ----
get_xlsx_data_pims <-
  function(my_file_path,
           to_skip = 0,
           my_sheet = "Sheet 1") {
    
    data_from_pims_raw <-
      read_xlsx(my_file_path,
                sheet = my_sheet,
                skip = to_skip)
    
    # clean_headers
    data_from_pims <-
      data_from_pims_raw %>%
      clean_headers()
    
    return(data_from_pims)
  }

convert_to_dates <-
  function(my_df) {
    my_df_w_dates <-
      my_df |>
      mutate(across(
        where(is.character) &
          (ends_with("_date")),
        ~ lubridate::parse_date_time(.x, orders = c("mdY"))
      ))
    return(my_df_w_dates)
  }

# upload vessels from PIMS ----

vessel_names_file_path <-
  file.path(my_paths$inputs,
            r"(from PIMS\Vessels - 2024-02-28_0930.xlsx)")

# file.exists(vessel_names_file_path)
vessels_from_pims <- 
  get_xlsx_data_pims(vessel_names_file_path,
                     to_skip = 3)

# print_df_names(vessels_from_pims)

dim(vessels_from_pims)
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
            r"(from PIMS\Permits - 2024-02-28_0930.xlsx)")

# file.exists(permits_names_file_path)
permits_from_pims <- 
  get_xlsx_data_pims(permits_names_file_path,
                     to_skip = 4)

# Clean data ----
## permits split vessel_or_dealer ----
permits_from_pims__split1 <-
  permits_from_pims |>
  separate(vessel_or_dealer,
           c('vessel_official_number', 'dealer'),
           sep = " / ") |>
  mutate(across(c('vessel_official_number', 'dealer'),
                str_squish))

# Expected 2 pieces. Missing pieces filled with `NA` in 3038 rows [229, 244, 294,

permits_from_pims[229,] |> 
  glimpse()
# $ vessel_or_dealer <chr> "BARNACLE SEAFOOD INC"

## permits clean and shorten ----
program_start_date <- lubridate::dmy("04/01/2021")

permits_from_pims__split1_short <-
  permits_from_pims__split1 |>
  convert_to_dates() |> 
  filter(if_any(ends_with("_date"), 
                ~ . > program_start_date))

dim(permits_from_pims)
# [1] 53365    11

dim(permits_from_pims__split1_short)
# [1] 31180    12

## permits split permit number ----
permits_from_pims__split1_short__split2 <- 
  permits_from_pims__split1_short |> 
  separate(permit__,
           c('permit', 'permit_number'),
           sep = "-") |>
  mutate(across(starts_with('permit'),
                str_squish))
# View(permits_from_pims__split1_short__split2)

## vessels clean and shorten  ----
vessels_from_pims_short <-
  vessels_from_pims |>
  rename("vessel_official_number" = official__) |>
  select(vessel_official_number,
         hailing_port) |>
  distinct()
  # convert_to_dates() |> 

# print_df_names(vessels_from_pims)

dim(vessels_from_pims_short)
# [1] 22887     2

## vessels remove "NOVESID" ----
vessels_from_pims_short_ok <-
  vessels_from_pims_short |>
  filter(!grepl("^NOVESID", vessel_official_number))

dim(vessels_from_pims_short_ok)
# [1] 22510     2

# View(vessels_from_pims_short_ok)

## vessel split double names ----
vessels_from_pims_short_ok__split1 <-
  vessels_from_pims_short_ok |>
  separate(vessel_official_number,
           c('vessel_official_number', 'vessel_official_number2'),
           sep = " / ") |>
  mutate(across(starts_with('vessel_official_number'),
                str_squish))

# Expected 2 pieces. Missing pieces filled with `NA` in 21895 rows [1, 2, 3, 4, 5,

# View(vessels_from_pims_short_ok__split1)

## vessel make one column of double names ----
### split into 2 dataframes and rename the id column ----

# [1] "vessel_official_number, vessel_official_number2, hailing_port"

vessels_from_pims_double_1 <-
  vessels_from_pims_short_ok__split1 |> 
  select(-vessel_official_number2)

vessels_from_pims_double_2 <-
  vessels_from_pims_short_ok__split1 |> 
  select(-vessel_official_number) |>
  rename("vessel_official_number" = vessel_official_number2)

### combine in one df ----
vessels_from_pims_double_bind <-
  rbind(
    vessels_from_pims_double_1,
    vessels_from_pims_double_2
  ) |> 
  distinct() |> 
  filter(!is.na(vessel_official_number))

# View(vessels_from_pims_double_bind)
# [1] 23086     2

## Clean vessel home port punctuation ----

vessels_from_pims_ok <-
  vessels_from_pims_double_bind |>
  mutate(hailing_port =
           str_replace(hailing_port,
                       ",",
                       ", ")) |>
  mutate(hailing_port =
           str_replace(hailing_port,
                       " ,",
                       ",")) |>
  mutate(hailing_port =
           str_squish(hailing_port)) |> 
  distinct()

dim(vessels_from_pims_ok)
# [1] 23086     2

# check
grep(",[a-zA-Z]",
     vessels_from_pims_ok$hailing_port,
     value = T)
# 0

grep("  +",
     vessels_from_pims_ok$hailing_port,
     value = T)
# 0