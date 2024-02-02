# run once to get lat lon and check names with no coords
# 1) add lat/lon
# 2) check names without coordinates
# 3) fix names
# 4) add lat/lon to the fixed names

# print_df_names(vessels_from_pims__vessels_from_metrics_short)
# [1] "vessel_official_number, hailing_port, permit_sa_gom_metr"

# separate hailing_port into city and state ----
vessels_from_pims__vessels_from_metrics_short_addr <-
  vessels_from_pims__vessels_from_metrics_short |>
  tidyr::separate_wider_delim(hailing_port,
                              delim = ",",
                              names = c("city", "state"),
                              too_many = "drop") |> 
    mutate(across(where(is.character), str_trim))

# vessels_from_pims__vessels_from_metrics_short |> 
#   filter(grepl(",.+,", hailing_port)
# )
# 1 AL6468LL               ALEXANDER CITY, AL, AL gom_only          
# Can do too_many = "drop", because it is the only case of double commas

# vessels_from_pims__vessels_from_metrics_short_addr |>
#   filter(vessel_official_number == "AL6468LL")
# 1 AL6468LL               ALEXANDER CITY AL    gom_only          

# 1) add lat/lon ----

get_lat_lon_no_county <-
  function(my_df,
           city_col_name = "city",
           state_col_name = "state") {
    result_coord <-
      my_df |>
      tidygeocoder::geocode(city = "city",
                            state = "state",
                            return_input = TRUE)
    return(result_coord)
  }

tic("vessels_from_pims__vessels_from_metrics_short_addr_coord")
vessels_from_pims__vessels_from_metrics_short_addr_coord_1 <-
  get_lat_lon_no_county(vessels_from_pims__vessels_from_metrics_short_addr)
toc()
# Passing 552 addresses to the Nominatim single address geocoder
# 10 min

# 2) check names without coordinates

# was
n_distinct(vessels_from_pims__vessels_from_metrics_short_addr$vessel_official_number)
# 3387

n_distinct(vessels_from_pims__vessels_from_metrics_short_addr_coord_1$vessel_official_number)
# 3387 the same

vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord <-
  vessels_from_pims__vessels_from_metrics_short_addr_coord_1 |>
  filter(is.na(lat))

nrow(vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord)
# 257

vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord__has_city <-
  vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord |>
  filter(!is.na(city))

nrow(vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord__has_city)
# 56

# glimpse(vessels_from_pims__vessels_from_metrics_short_addr_coord_no_coord__has_city)

# 3) fix home port typos ----

# this list is created manually
to_fix_list <- 
  list(
    c(
      "BAYOU LABATRE#AL",
      "BAYOU LA BATRE#AL"),
    c("CAROLINA BEACH#UN",
      "CAROLINA BEACH#NC"),
    c("CHALESTON#SC",
      "CHARLESTON#SC"),
    c("CHAUVIN, LA#LA",
      "CHAUVIN#LA"),
    c("FERNADINA BCH#FL",
      "FERNANDINA BEACH#FL"),
    c("FORT MORGAN MARINA#AL",
      "FORT MORGAN#AL"),
    c("GALLINANO#LA",
      "GALLIANO#LA"),
    c("GEORGRTOWN#SC",
      "GEORGETOWN#SC"),
    c("GULFSHORES#AL",
      "GULF SHORES#AL"),
    c("HILISBORO INLET#FL",
      "HILLSBORO INLET#FL"),
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
    c("MAYPPORT#FL",
      "MAYPORT#FL"),
    c("MCLELLANVILLE#SC",
      "MCCLELLANVILLE#SC"),
    c("MURELLS INLET#SC",
      "MURRELLS INLET#SC"),
    c("MURRELS INLET#SC",
      "MURRELLS INLET#SC"),
    c("NEW SMYMA BEACH#FL",
      "NEW SMYRNA BEACH#FL"),
    c("NEW SYMRNA BEACH#FL",
      "NEW SMYRNA BEACH#FL"),
    c("OCEEAN CITY#MD",
      "OCEAN CITY#MD"),
    c("POINT PLEASANT NJ#NJ",
      "POINT PLEASANT#NJ"),
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
      "WANCHESE#NC"),
    c("ALEXANDER CITY, AL#AL",
      "ALEXANDER CITY#AL")
  )

# ---
# Explanations:
# - mutate(): Adds a new column 'city_state' to the data frame.
# - paste(): Concatenates the trimmed values of 'city' and
#   'state' columns with '#' as a separator.
# The result is a modified data frame with an additional 'city_state' column
# containing concatenated city and state information.

vessels_from_pims__vessels_from_metrics_short_addr_1 <-
  vessels_from_pims__vessels_from_metrics_short_addr |>
  mutate(city_state =
           paste(
             trimws(city),
             trimws(state),
             sep = "#"
           ))

# ---

# 1. **Column Extraction Using sapply:**
#    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
# 
# 2. **Column Extraction Using '[':**
#    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#    - This operation is used to extract a specific column or element from each list or data frame within 'to_fix_list'.
# 
# 3. **Final Result:**
#    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

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

vessels_from_pims__vessels_from_metrics_short_addr__fixed <-
  vessels_from_pims__vessels_from_metrics_short_addr_1 |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |>
  tidyr::separate_wider_delim(city_state_fixed,
                              delim = "#",
                              names = c("city_fixed",
                                        "state_fixed"))

n_distinct(vessels_from_pims__vessels_from_metrics_short_addr__fixed$vessel_official_number)
# 3387
dim(vessels_from_pims__vessels_from_metrics_short_addr__fixed)
# [1] 4729    8
# [1] 5029    8 with permit region
# [1] 6762    7 not processed db vessel_permits
# [1] 3392    7 (2023)
# duplicated addr

# add new fixes manually ----
fixes_1 <-
  list(
    list("TX9606KA", "HOUSTON", "TX"),
    list("FL5029RM", "KEY WEST", "FL"),
    list("646818", "HOUSTON", "TX"),
    list("1185107", "KEY WEST", "FL"),
    list("FL3119EE", "BOCA GRANDE", "FL"),
    list("FL2615MT", "STUART", "FL"),
    list("581260", "PONCE INLET", "FL"),
    list("531549", "TOWNSEND", "GA")
  )

res <-
  map(fixes_1,
      \(x) {
        vessels_from_pims__vessels_from_metrics_short_addr__fixed |>
          mutate(city_fixed =
                   case_when(is.na(city) &
                               vessel_official_number == x[[1]] ~ x[[2]])) |> 
        mutate(state_fixed =
                 case_when(is.na(state) &
                             vessel_official_number == x[[1]] ~ x[[3]]))
      })


View(res)
# 4) add lat/lon to the fixed names

# glimpse(vessels_from_pims__vessels_from_metrics_short_addr__fixed)

my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_basename,
            paste0(current_project_basename, "fixed_lat_lon_2023.rds"))


# 4) add lat/lon to the fixed names ----
tic("vessels_from_pims__vessels_from_metrics_short_addr_coord")
vessels_from_pims__vessels_from_metrics_short_addr_coord_fixed <-
  read_rds_or_run(
    my_file_path_lat_lon,
    my_data =
      as.data.frame(vessels_from_pims__vessels_from_metrics_short_addr),
    get_lat_lon_no_county
  )
toc()

# old -----

# check home port typos by lat/lon ----

# Explanations:
# The code defines a custom R function 'check_home_port_typos_by_lat_lon':
# - Takes a list of data frames 'df_list_by_reg' as input.
# - Iterates over the names of the list using map() and a lambda function.
# - For each region's data frame:
#   - Filters rows with missing latitude or longitude.
#   - Selects relevant columns for further analysis.
#   - Removes duplicate rows based on selected columns.
#   - Trims leading and trailing whitespaces from city and state columns using mutate().
# - Names the resulting list with region names.
# - Returns the list 'compl_err_db_data_metrics_permit_reg_list_home_port_err'.
# This function is designed to check and clean data related to home ports with missing
# latitude or longitude coordinates within each region's data frame.

# vessels_from_pims__vessels_from_metrics_short_addr__fixed |>
#   filter(is.na(long) | is.na(lat)) |>
#   select(vessel_official_number,
#          city,
#          state) |>
#   distinct() |>
#   mutate(city = trimws(city),
#          state = trimws(state))




# Work with compl_err_db_data_metrics_permit_reg_list_home_port_err_county in excel ----

# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_lat_longs_city_state_err'
# using the pipe operator and dplyr functions:
# - filter(): Selects rows with missing latitude or longitude coordinates.
# - select(): Chooses relevant columns for further analysis.
# - distinct(): Removes duplicate rows based on the selected columns.
# The final result is a data frame containing rows with home ports that have
# missing latitude or longitude coordinates.

vessels_permits_home_port_lat_longs_city_state_err <-
  vessels_permits_home_port_lat_longs_city_state |>
  filter(is.na(long) |
           is.na(lat)) |>
  select(SERO_OFFICIAL_NUMBER,
         city,
         state) |>
  distinct()

dim(vessels_permits_home_port_lat_longs_city_state_err)
# [1] 80  3
# 126 3
# 128 3

vessels_permits_home_port_lat_longs_city_state_err_all <-
  vessels_permits_home_port_lat_longs_city_state |>
  select(city,
         state,
         lat,
         long) |>
  distinct()


# ---
# Explanations:
# The code creates a new data frame 'vessels_permits_home_port_c_st' by using the pipe
# operator and the dplyr mutate() function:
# - mutate(): Adds a new column 'city_state' to the data frame.
# - paste(): Concatenates the trimmed values of 'city' and
#   'state' columns with '#' as a separator.
# The result is a modified data frame with an additional 'city_state' column
# containing concatenated city and state information.

vessels_permits_home_port_c_st <-
  vessels_permits_home_port_short |>
  mutate(city_state =
           paste(
             trimws(city),
             trimws(state),
             sep = "#"
           ))

all_vessels_permits_home_port_clean0 <- 
  all_vessels_permits_home_port |> 
    mutate(city_state = 
           paste(trimws(city), 
                 trimws(state), 
                 sep = "#")) 

# ---

# 1. **Column Extraction Using sapply:**
#    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
# 
# 2. **Column Extraction Using '[':**
#    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#    - This operation is used to extract a specific column or element from each list or data frame within 'to_fix_list'.
# 
# 3. **Final Result:**
#    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

# ---
# Explanations:
# The code defines a custom R function 'get_correct_addr_by_wrong':
# - Takes a 'wrong_addr' as input.
# - Uses grep() to find the index in 'to_fix_list' that contains the wrong address.
# - Uses tryCatch() to handle potential errors and print informative messages.
# - Extracts the corresponding pair of names from 'to_fix_list'.
# - Returns the correct address from the pair.
# This function is designed to find the correct address given a wrong address
# by searching for it in 'to_fix_list'.

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

# ---
# Explanations:
# The code creates a new data frame 'vessels_from_pims__vessels_from_metrics_short_addr__fixed' using
# the pipe operator and dplyr functions:
# - rowwise(): Specifies that operations should be applied row by row.
# - mutate(): Adds a new column 'city_state_fixed':
#   - If 'city_state' is in 'wrong_port_addr', it is replaced with the correct
#     address using get_correct_addr_by_wrong(); otherwise, the original value is kept.
# - ungroup(): Removes the rowwise grouping.
# - tidyr::separate_wider_delim(): Splits 'city_state_fixed' into 'city_fixed' and
#   'state_fixed' columns using '#' as a separator.
# The resulting data frame has fixed and separated city and state columns.

# Have to use "if", because case_when will execute all the LHS and RHS, then keep based on conditions. So get_correct_addr_by_wrong is executed, one time by each row during the RHS evaluation and produces idx = 0 error. 

vessels_from_pims__vessels_from_metrics_short_addr__fixed <-
  vessels_permits_home_port_c_st |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |>
  tidyr::separate_wider_delim(city_state_fixed,
                              delim = "#",
                              names = c("city_fixed",
                                        "state_fixed"))

dim(vessels_from_pims__vessels_from_metrics_short_addr__fixed)
# [1] 4729    8
# [1] 5029    8 with permit region
# [1] 6762    7 not processed db vessel_permits

## The same for the second df ----
all_vessels_permits_home_port_clean0_fixed <-
  all_vessels_permits_home_port_clean0 |>
  rowwise() |>
  mutate(city_state_fixed =
           if (city_state %in% wrong_port_addr)
             get_correct_addr_by_wrong(city_state)
         else
           city_state) |>
  ungroup() |> 
  tidyr::separate_wider_delim(city_state_fixed, 
                              delim = "#", 
                              names = c("city_fixed", "state_fixed"))

vessels_from_pims__vessels_from_metrics_short_addr__fixed |> 
  filter(!city == city_fixed) |> 
  dim()
# [1] 49  7
# [1] 109   8 trimmed
# [1] 115   8 with permit region
# [1] 281   7

# Manually add Bokeelia is located in western Lee County at 26°41′17″N 82°8′43″W (26.687960, -82.145249).[5] It sits at the northern end of Pine Island and is bordered by water on three sides

# View(all_vessels_permits_home_port_clean0_fixed)
all_vessels_permits_home_port_clean0_fixed |> 
  filter(!city == city_fixed) |> 
  dim()
# [1] 60  8

dim(all_vessels_permits_home_port_clean0_fixed)
# [1] 6894    8

cat("Result in vessels_from_pims__vessels_from_metrics_short_addr__fixed",
    "And in all_vessels_permits_home_port_clean0_fixed",
    sep = "\n")
dim(vessels_from_pims__vessels_from_metrics_short_addr__fixed)
# [1] 6845    7

