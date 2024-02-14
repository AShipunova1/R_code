# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col

# run once to get lat lon and check names with no coords
# 1) add lat/lon
# 2) check names without coordinates
# 3) fix names

# print_df_names(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col)
# [1] "vessel_official_number, permit_sa_gom_dual, compliant_, year, hailing_port, year_permit_sa_gom_dual"

# separate hailing_port into city and state ----

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr' is created by:
# 1. Separating the 'hailing_port' column into two columns ('city' and 'state') using a comma as the delimiter with 'tidyr::separate_wider_delim'.
# 2. Dropping any additional columns created during the separation.
# 3. Trimming leading and trailing whitespaces from all character columns using 'mutate(across(where(is.character), str_trim))'.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |>
  tidyr::separate_wider_delim(hailing_port,
                              delim = ",",
                              names = c("city", "state"),
                              too_many = "drop") |> 
    mutate(across(where(is.character), str_trim))

# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col |>
#   filter(grepl(",.+,", hailing_port))
# 1 AL6468LL               ALEXANDER CITY, AL, AL gom_only          
# Can do too_many = "drop", because it is the only case of double commas

# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr |> View()
#   filter(vessel_official_number == "AL6468LL")
# 1 AL6468LL               ALEXANDER CITY AL    gom_only  

# add lat/lon ----
# Uses 'tidygeocoder::geocode' to obtain latitude and longitude for the given city and state columns.

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

my_file_path_lat_lon_1 <- 
  file.path(my_paths$outputs, 
            current_project_basename,
            paste0(current_project_basename,
                   "2024-02-09",
                   "_lat_lon_2023.rds"))

tic("compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord")
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_1 <-
  read_rds_or_run(
    my_file_path_lat_lon_1,
    my_data =
      as.data.frame(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr),
    get_lat_lon_no_county
  )
toc()
# Passing 552 addresses to the Nominatim single address geocoder
# ~10 min

# File: non_compliant_areas2024-02-09_lat_lon_2023.rds modified Fri Feb  9 15:43:14 2024
# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord: 633.06 sec elapsed

# 2) check names without coordinates

n_distinct(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr$vessel_official_number)
# 4017

n_distinct(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_1$vessel_official_number)
# 4017 the same

# Explanations:
# Filtering rows from 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_1' where the latitude ('lat') is missing using 'filter(is.na(lat))'.

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_1 |>
  filter(is.na(lat))

## unique vessels_addr ----

## fewer columns ----
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord__u_vessels_addr <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord |>
  select(vessel_official_number, city, state, lat, long) |>
  distinct()

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord__u_vessels_addr |> 
  nrow()
# 288

# check no city
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord__has_city <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord__u_vessels_addr |>
  filter(!is.na(city))

nrow(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_coord_no_coord__has_city)
# 71

# fix home port typos ----

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
# Creating a new column 'city_state' by concatenating trimmed 'city' and 'state' columns, separated by '#'.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_1 <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr |>
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

# Explanations:
# The function 'get_correct_addr_by_wrong' takes a 'wrong_addr' as input and performs the following steps:
# 1. Finds the index of 'wrong_addr' in the 'to_fix_list'.
# 2. Uses 'tryCatch' to handle errors, printing information about the error and the index if one occurs.
# 3. Extracts the correct address from the pair.
# 4. Returns the correct address.
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

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed' is created by:
# 1. Creating a new column 'city_state_fixed' by replacing wrong addresses using 'get_correct_addr_by_wrong' for rows where 'city_state' is in 'wrong_port_addr'.
# 2. Separating the 'city_state_fixed' column into two columns ('city_fixed' and 'state_fixed') using '#' as the delimiter.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr_1 |>
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

n_distinct(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed$vessel_official_number)
# 4017

dim(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed)
# [1] 9362   10

# duplicated addr

# add new fixes manually ----
manual_fixes <-
  list(
    list("TX9606KA", "HOUSTON", "TX"),
    list("FL5029RM", "KEY WEST", "FL"),
    list("646818", "HOUSTON", "TX"),
    list("1185107", "KEY WEST", "FL"),
    list("FL3119EE", "BOCA GRANDE", "FL"),
    list("FL2615MT", "STUART", "FL"),
    list("581260", "PONCE INLET", "FL"),
    list("531549", "TOWNSEND", "GA"),
    list("FL0146BH", "MIAMI", "FL"),
    list("FL1431JU", "MARATHON", "FL"),
    list("FL1553TM", "BILOXI", "MS"),
    list("FL3976FH", "PONCE INLET", "FL"),
    list("FL7549PJ", "KEY LARGO", "FL"),
    list("FL5011MX", "NAPLES", "FL"),
    list("139403", "MIAMI", "FL"),
    list("FL8252JK", "MIAMI", "FL")
  )

# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1' is created by:
# 1. Applying mutations to the 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed' data frame for each set of manual fixes using 'map_df'.
# - Creating new columns 'city_fixed1' and 'state_fixed1' using case_when for each manual fix.
# 
# 2. Returning the result of the mutations.
# 3. Keeping only distinct rows in the final result.

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1 <-
  map_df(manual_fixes,
         \(x) {
           # browser()
           res <-
             compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed |>
             mutate(
               city_fixed1 =
                 case_when(vessel_official_number == x[[1]] ~ x[[2]]),
               state_fixed1 =
                 case_when(vessel_official_number == x[[1]] ~ x[[3]])
             )
           return(res)
         }) |>
  distinct()

dim(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1)
# [1] 9382   12

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1 |>
  filter(vessel_official_number == "FL1431JU") |>
  glimpse()
# $ city_fixed             <chr> "KEY WEST", "MARATHON", "KEY WEST", "MARATHON"
# $ state_fixed            <chr> "FL", "FL", "FL", "FL"
# $ city_fixed1            <chr> NA, NA, "MARATHON", "MARATHON"
# $ state_fixed1           <chr> NA, NA, "FL", "FL"

# check
new_f_vsl <-
  sapply(manual_fixes, "[", 1) |> 
  unlist()

both <-
  intersect(
    compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed$vessel_official_number,
    new_f_vsl
  )
length(both)
# 5
# [1] "FL1431JU" "FL3976FH" "FL0146BH" "FL7549PJ" "FL1553TM"

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1 |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         permit_sa_gom_dual,
         city_fixed1,
         state_fixed1) |>
  filter(!is.na(city_fixed1) & !is.na(city_fixed1)) |>
  distinct() |>
  glimpse()
# 5 ok

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1 |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         permit_sa_gom_dual,
         city_fixed,
         state_fixed,
         city_fixed1,
         state_fixed1) |>
  filter(!is.na(city_fixed1) & !is.na(city_fixed1)) |>
  distinct() |>
  glimpse()

## replace duplicated values ----
# Explanations:
# The variable 'compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2' is created by:
# 1. Updating 'city_fixed' and 'state_fixed' columns based on conditions using 'case_when':
#     - If 'city_fixed1' is not NA, update 'city_fixed' with 'city_fixed1'; otherwise, keep the existing value in 'city_fixed'.
#     - If 'state_fixed1' is not NA, update 'state_fixed' with 'state_fixed1'; otherwise, keep the existing value in 'state_fixed'.
# 2. Filtering rows where 'vessel_official_number' is not in 'both' or 'state_fixed1' is not missing.
# 3. Selecting all columns except "city_fixed1" and "state_fixed1".
# 4. Keeping only distinct rows in the final result to avoid duplications.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2 <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_1 |>
  mutate(
    city_fixed =
      case_when(!is.na(city_fixed1) ~ city_fixed1,
                .default = city_fixed),
    state_fixed =
      case_when(!is.na(state_fixed1) ~ state_fixed1,
                .default = state_fixed)
  ) |> 
  filter((!vessel_official_number %in% both) |
           !is.na(state_fixed1)) |> 
  select(-c("city_fixed1", "state_fixed1")) |> 
  distinct()

dim(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2)
# [1] 9362   10

# check
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2 |>
  filter(vessel_official_number %in% both) |>
  select(vessel_official_number,
         permit_sa_gom_dual,
         city_fixed,
         state_fixed) |> 
# ,
#          # city_fixed1,
#          state_fixed1) |>
  # filter(!is.na(city_fixed1) & !is.na(city_fixed1)) |>
  distinct() |>
  glimpse()
# 5

## no address ----
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_no_addr <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2 |>
  filter(is.na(city))

nrow(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_no_addr)
# 467

# write_csv(
#   compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_no_addr,
#   file.path(my_paths$outputs,
#             current_project_basename,
#             "no_addr.csv")
# )

# vessels_from_pims |>
#   filter(official__ == "504660")
# not in pims vessels

# vessels with NA fixed state in vessels_from_pims_double ----

names(vessels_from_pims_double)
# [1] "vessel_official_number1" "vessel_official_number2" "hailing_port"           

# glimpse(vessels_from_pims_double)
# 652

## make one column of double names ----
### split into 2 dataframes and rename the id column ----
vessels_from_pims_double_1 <-
  vessels_from_pims_double |>
  select(vessel_official_number1, hailing_port) |>
  rename("vessel_official_number" = vessel_official_number1)

vessels_from_pims_double_2 <-
  vessels_from_pims_double |>
  select(vessel_official_number2, hailing_port) |> 
  rename("vessel_official_number" = vessel_official_number2)

### combine in one df ----
vessels_from_pims_double_bind <-
  rbind(
    vessels_from_pims_double_1,
    vessels_from_pims_double_2
  ) |> 
  distinct() |> 
  rename("hailing_port_2" = hailing_port)

dim(vessels_from_pims_double_bind)
# 1205 2

# write_csv(
#   na_vessel_states,
#   file.path(current_output_dir,
#             "na_vessel_states.csv")
# )

# vessels_from_pims_double_short__na_vessel_states <-
#   vessels_from_pims_double_bind |>
#   filter(vessel_official_number %in% compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_no_addr$vessel_official_number) |> 
#   distinct()

dim(vessels_from_pims_double_bind)
# 1205    2

# check
# vessels_from_pims_double_short__na_vessel_states |>
#   filter(vessel_official_number == "1173297")
# 1 1173297                CAROLINA BEACH, NC

# compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2 |> 
#     filter(vessel_official_number == "1173297") |> distinct() |> 
#   glimpse()
# $ year_permit_sa_gom_dual <chr> "2022 sa_only", "2023 sa_only"
# $ city_state              <chr> "NA#NA", "NA#NA"
# $ city_fixed              <chr> "NA", "NA"
# $ state_fixed             <chr> "NA", "NA"

## clean addresses ----
# Explanations:
# Creating a new column 'city_state' by replacing commas in 'hailing_port_2' with '#' using 'str_replace'.
vessels_from_pims_double_bind__city_state <-
  vessels_from_pims_double_bind |>
  mutate(city_state =
           str_replace(hailing_port_2, " *, *", "#"))

# Explanations:
# The variable 'vessels_from_pims_double_bind__city_state__fixed' is created by:
# 1. Creating a new column 'city_state_fixed' by replacing wrong addresses using 'get_correct_addr_by_wrong' for rows where 'city_state' is in 'wrong_port_addr'.
# 2. Separating the 'city_state_fixed' column into two columns ('city_fixed' and 'state_fixed') using '#' as the delimiter.
# 3. Keeping only distinct rows in the final result to avoid duplications.
vessels_from_pims_double_bind__city_state__fixed <-
  vessels_from_pims_double_bind__city_state |>
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
                                        "state_fixed")) |>
  distinct()

# glimpse(vessels_from_pims_double_bind__city_state__fixed)
# 1205

## unique fixed double names ----
vessels_from_pims_double_bind__city_state__fixed_1 <-
  vessels_from_pims_double_bind__city_state__fixed |>
  select(-c(hailing_port_2, city_state)) |>
  distinct()

nrow(vessels_from_pims_double_bind__city_state__fixed_1)
# 1202

## add more home ports ----
# Explanations:
# 1. Joining two data frames ('compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2' and 'vessels_from_pims_double_bind__city_state__fixed_1') on 'vessel_official_number'.
# 2. Adding suffixes '.orig' and '.double_names' to the overlapping column names from the original and double names data frames.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3 <-
  left_join(
    compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2,
    vessels_from_pims_double_bind__city_state__fixed_1,
    join_by(vessel_official_number),
    suffix = c(".orig", ".double_names")
  )

# dim(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3)
# [1] 9362   12

# check
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3 |>
  filter(vessel_official_number == "563752") |>
  glimpse()

### Combine state fixed names ----
# Explanations:
# 1. Trimming leading and trailing whitespaces from all character columns using 'mutate(across(where(is.character), str_trim))'.
# 2. Creating a new column 'state_fixed1' based on conditions using 'case_when':
#     - If 'state_fixed.orig' is NA or "NA" and 'state_fixed.double_names' is not NA, update 'state_fixed1' with 'state_fixed.double_names'.
#     - Otherwise, keep the existing value in 'state_fixed.orig'.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports1 <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3 |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(state_fixed1 =
           case_when((is.na(state_fixed.orig) |
                        state_fixed.orig == "NA") &
                       !is.na(state_fixed.double_names) ~
                       state_fixed.double_names,
                     .default = state_fixed.orig
           ))

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports1 |> 
  filter(!state_fixed.orig == state_fixed1) |> 
  glimpse()
# $ city_state               <chr> "NA#NA", "NA#NA", "NA#NA", "NA#NA", "NA#NA", …
# $ city_fixed.orig          <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA…
# $ state_fixed.orig         <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA…
# $ city_fixed.double_names  <chr> "FREEPORT", "FREEPORT", "WILMINGTON", "WILMIN…
# $ state_fixed.double_names <chr> "TX", "TX", "DE", "DE", "DE", "MD", "MD", "MD…
# $ state_fixed1             <chr> "TX", "TX", "DE", "DE", "DE", "MD", "MD", "MD…

### rename the state column ----
# Explanations:
# 1. Removing the 'state_fixed.orig' column using 'select(-state_fixed.orig)'.
# 2. Renaming the 'state_fixed1' column to 'state_fixed' using 'rename("state_fixed" = state_fixed1)'.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports_more_ports <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports1 |>
  select(-state_fixed.orig) |>
  rename("state_fixed" = state_fixed1)

# check
dim(compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports_more_ports)
# 9362   12

compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports_more_ports |>
  filter(year_permit_sa_gom_dual == "2023 sa_only") |> 
  filter(state_fixed == "NA" | is.na(state_fixed)) |> 
  nrow()
# 0

# Print results ----
cat("compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_2_more_ports_more_ports",
    sep = "\n")
