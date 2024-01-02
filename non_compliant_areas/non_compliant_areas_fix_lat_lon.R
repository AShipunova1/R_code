# run once to get lat lon and check names with no coords

# add lat/lon ----

my_file_path_lat_lon <- 
  file.path(my_paths$outputs, 
            current_project_dir_name,
            paste0(current_project_dir_name, "_no_county_all.rds"))

file.exists(my_file_path_lat_lon)

vessels_permits_home_port_short_trim_no_county <-
  vessels_permits_home_port_short |>
  mutate(
    SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
    SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
  ) |>
  select(-SERO_HOME_PORT_COUNTY)

get_lat_lon_no_county <-
  function(my_df) {
    vessels_permits_home_port_lat_longs <-
      my_df |>
      tidygeocoder::geocode(city = "SERO_HOME_PORT_CITY",
                            state = "SERO_HOME_PORT_STATE",
                            return_input = TRUE)
    return(vessels_permits_home_port_lat_longs)
  }

vessels_permits_home_port_lat_longs_city_state <-
  read_rds_or_run(my_file_path_lat_lon,
                  my_data =
                    as.data.frame(vessels_permits_home_port_short_trim_no_county),
                  get_lat_lon_no_county)

# vessels_permits_home_port_lat_longs_city_state |> 
#   filter(SERO_OFFICIAL_NUMBER %in% compl_vessl_not_in_ves_perm$vessel_official_number) |>
#   glimpse()

# View(vessels_permits_home_port_lat_longs_city_state)
# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_short$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 1

# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 1

# setdiff(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_lat_longs_city_state$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 228

# intersect(tolower(compl_vessl_not_in_ves_perm$vessel_official_number),
#         tolower(vessels_permits_home_port_lat_longs_city_state$SERO_OFFICIAL_NUMBER)) |> 
#   length()
# 0?
#   
# Passing 850 addresses to the Nominatim single address geocoder
# [==================================] 850/850 (100%) Elapsed: 15m Remaining:  0s
# 2024-01-02 run for non_compliant_areas_no_county_all.rds: 881.34 sec elapsed
# Saving new data into a file here: 
# C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/non_compliant_areas/non_compliant_areas_no_county_all.rds
# Warning message:
# In query_api(api_url, api_query_parameters, method = method) :
#   Internal Server Error (HTTP 500).

# Add back lost vessels ----
vessels_permits_home_port_lat_longs_city_state_df <- 
  as.data.frame(vessels_permits_home_port_lat_longs_city_state)

setdiff(tolower(unique(
  vessels_permits_home_port_lat_longs_city_state_df$SERO_OFFICIAL_NUMBER)),
  tolower(unique(
vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER))) |> 
  length()
# 9

# View(vessels_permits_home_port_lat_longs_city_state_df)
setdiff(tolower(unique(vessels_permits_home_port_short_trim_no_county$SERO_OFFICIAL_NUMBER)),
        tolower(unique(vessels_permits_home_port_lat_longs_city_state_df$SERO_OFFICIAL_NUMBER))) |> 
  length()
# 92

str(vessels_permits_home_port_short_trim_no_county)
str(vessels_permits_home_port_lat_longs_city_state_df)

all_vessels_permits_home_port <-
  full_join(
    vessels_permits_home_port_lat_longs_city_state_df,
    vessels_permits_home_port_short_trim_no_county
  )
# Joining with `by = join_by(SERO_OFFICIAL_NUMBER, SERO_HOME_PORT_CITY,
# SERO_HOME_PORT_STATE)`

dim(all_vessels_permits_home_port)
# [1] 6894    5
# data_overview(all_vessels_permits_home_port)
# SERO_OFFICIAL_NUMBER 6854
# SERO_HOME_PORT_CITY   840
# SERO_HOME_PORT_STATE   32
# lat                   695
# long                  695

# TODO: Use all_vessels_permits_home_port later to add states back if missing

# check home port typos by lat/lon ----
check_home_port_typos_by_lat_lon <- 
  function(df_list_by_reg) {
    compl_err_db_data_metrics_permit_reg_list_home_port_err <-
      names(df_list_by_reg) |>
      map(\(curr_permit_reg_name) {
        browser()
        df_list_by_reg[[curr_permit_reg_name]] |>
          filter(is.na(long) |
                   is.na(lat)) |>
          select(
            vessel_official_nbr,
            SERO_HOME_PORT_CITY,
            # SERO_HOME_PORT_COUNTY,
            SERO_HOME_PORT_STATE
          ) |>
          distinct() |>
          mutate(
            SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
            # SERO_HOME_PORT_COUNTY = trimws(SERO_HOME_PORT_COUNTY),
            SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
          )
      })
    
    names(compl_err_db_data_metrics_permit_reg_list_home_port_err) <-
      names(df_list_by_reg)
    
    return(compl_err_db_data_metrics_permit_reg_list_home_port_err)
  }

# vessels_permits_home_port_lat_longs_city_state |>
#   filter(is.na(long) |
#            is.na(lat)) |>
#   select(SERO_OFFICIAL_NUMBER,
#          SERO_HOME_PORT_CITY,
#          SERO_HOME_PORT_STATE) |>
#   distinct() |>
#   mutate(
#     SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
#     SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
#   )
# Rows: 80

# View(compl_err_db_data_metrics_permit_reg_list_home_port_err)

# compl_err_db_data_metrics_permit_reg_list_home_port_err_county <- 
    # check_home_port_typos_by_lat_lon(compl_err_db_data_metrics_permit_reg_list)

  # check_home_port_typos_by_lat_lon(compl_err_db_data_metrics_permit_reg_list_home_port)

# Work with compl_err_db_data_metrics_permit_reg_list_home_port_err_county in excel ----

# print_df_names(vessels_permits_home_port_lat_longs_city_state)
vessels_permits_home_port_lat_longs_city_state_err <-
  vessels_permits_home_port_lat_longs_city_state |>
  filter(is.na(long) |
           is.na(lat)) |>
  select(SERO_OFFICIAL_NUMBER,
         SERO_HOME_PORT_CITY,
         SERO_HOME_PORT_STATE) |>
  distinct()

dim(vessels_permits_home_port_lat_longs_city_state_err)
# [1] 80  3
# 126 3
# 128 3

vessels_permits_home_port_lat_longs_city_state_err_all <-
  vessels_permits_home_port_lat_longs_city_state |>
  select(SERO_HOME_PORT_CITY,
         SERO_HOME_PORT_STATE,
         lat,
         long) |>
  mutate(
    SERO_HOME_PORT_CITY = trimws(SERO_HOME_PORT_CITY),
    SERO_HOME_PORT_STATE = trimws(SERO_HOME_PORT_STATE)
  ) |> 
  distinct()

vessels_permits_home_port_lat_longs_city_state_err_all |> 
  dim()
  # [1] 648   4
# [1] 851   4

csv_file_path <-
  file.path(
    my_paths$outputs,
    current_project_dir_name,
    stringr::str_glue(
      "{current_project_dir_name}_vessels_permits_home_port_lat_longs_city_state_err_all1.csv"
    )
  )

file.exists(csv_file_path)   

# uncomment and run once
# vessels_permits_home_port_lat_longs_city_state_err_all |>
  # write_csv(file = csv_file_path)

# fix home port typos ----
# the list is created manuall from the csv
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
    c("TAVENIER#FL",
      "TAVERNIER#FL"),
    c("WANCHEESE#NC",
      "WANCHESE#NC"),
    c("ALEXANDER CITY, AL#AL",
      "ALEXANDER CITY#AL")
  )

vessels_permits_home_port_c_st <- 
  vessels_permits_home_port_short |> 
  mutate(city_state = 
           paste(trimws(SERO_HOME_PORT_CITY), 
                 trimws(SERO_HOME_PORT_STATE), 
                 sep = "#")) 

# ---

# 1. **Column Extraction Using sapply:**
#    - The variable 'wrong_port_addr' is created by applying the 'sapply' function to 'to_fix_list'.
#    - The `sapply` function applies the '[' function to each element of 'to_fix_list' using the index 1.
# 
# 2. **Column Extraction Using '[':**
#    - The '[' function is used to extract the first element (index 1) from each element of 'to_fix_list'.
#    - This operation is likely used to extract a specific column or element from each list or data frame within 'to_fix_list'.
# 
# 3. **Final Result:**
#    - 'wrong_port_addr' holds the result of extracting the first element from each element within 'to_fix_list'.

wrong_port_addr <-
  sapply(to_fix_list, "[", 1)

get_correct_addr_by_wrong <-
  function(wrong_addr) {
    idx <- grep(wrong_addr, to_fix_list)
    
    names_pair <- 
      tryCatch(to_fix_list[[idx]],
               error = function(e) {
                 print(e)
                 print(str_glue("Index: {idx}"))
                 })
    good_addr <- names_pair[[2]]

    return(good_addr)
  }

# Have to use "if", because case_when will execute all the LHS and RHS, then keep based on conditions. So get_correct_addr_by_wrong is executed, one time by each row during the RHS evaluation and produces idx = 0 error. 

vessels_permits_home_port_c_st_fixed <-
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
                              names = c("city_fixed", "state_fixed"))

dim(vessels_permits_home_port_c_st_fixed)
# [1] 4729    8
# [1] 5029    8 with permit region
# [1] 6762    7 not processed db vessel_permits

vessels_permits_home_port_c_st_fixed |> 
  filter(!SERO_HOME_PORT_CITY == city_fixed) |> 
  dim()
# [1] 49  7
# [1] 109   8 trimmed
# [1] 115   8 with permit region
# [1] 281   7

cat("Result in vessels_permits_home_port_c_st_fixed")
# vessels_permits_home_port_c_st_fixed

# vessels_permits_home_port_c_st_fixed |> View()
