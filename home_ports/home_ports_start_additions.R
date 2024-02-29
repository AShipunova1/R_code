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

# vessels with NA fixed state in vessels_from_pims_double ----

names(vessels_from_pims_double)
# [1] "vessel_official_number1" "vessel_official_number2" "hailing_port"           

# glimpse(vessels_from_pims_double)
# 652


# vessels_from_pims_double_short__na_vessel_states <-
#   vessels_from_pims_double_bind |>
#   filter(vessel_official_number %in% vessels_from_pims_split_addr__city_state__fix2_ok__no_addr$vessel_official_number) |> 
#   distinct()

dim(vessels_from_pims_double_bind)
# 1205    2

# check
# vessels_from_pims_double_short__na_vessel_states |>
#   filter(vessel_official_number == "1173297")
# 1 1173297                CAROLINA BEACH, NC

# vessels_from_pims_split_addr__city_state__fix2_ok |> 
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
# 1. Joining two data frames ('vessels_from_pims_split_addr__city_state__fix2_ok' and 'vessels_from_pims_double_bind__city_state__fixed_1') on 'vessel_official_number'.
# 2. Adding suffixes '.orig' and '.double_names' to the overlapping column names from the original and double names data frames.
compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3 <-
  left_join(
    vessels_from_pims_split_addr__city_state__fix2_ok,
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
vessels_from_pims_split_addr__city_state__fix2_ok_more_ports1 <-
  compl_err_db_data_metrics_2022_23_clean__ports_short__comb_col_addr__fixed_3 |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(state_fixed1 =
           case_when((is.na(state_fixed.orig) |
                        state_fixed.orig == "NA") &
                       !is.na(state_fixed.double_names) ~
                       state_fixed.double_names,
                     .default = state_fixed.orig
           ))

vessels_from_pims_split_addr__city_state__fix2_ok_more_ports1 |> 
  filter(!state_fixed.orig == state_fixed1) |> 
  glimpse()
# $ city_state               <chr> "NA#NA", "NA#NA", "NA#NA", "NA#NA", "NA#NA", …
# $ city_fixed.orig          <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA…
# $ state_fixed.orig         <chr> "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA…
# $ city_fixed.double_names  <chr> "FREEPORT", "FREEPORT", "WILMINGTON", "WILMIN…
# $ state_fixed.double_names <chr> "TX", "TX", "DE", "DE", "DE", "MD", "MD", "MD…
# $ state_fixed1             <chr> "TX", "TX", "DE", "DE", "DE", "MD", "MD", "MD…

### the same for cities ----
vessels_from_pims_split_addr__city_state__fix2_ok_more_ports2 <-
  vessels_from_pims_split_addr__city_state__fix2_ok_more_ports1 |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(city_fixed1 =
           case_when((is.na(city_fixed.orig) |
                        city_fixed.orig == "NA") &
                       !is.na(city_fixed.double_names) ~
                       city_fixed.double_names,
                     .default = city_fixed.orig
           ))

dim(vessels_from_pims_split_addr__city_state__fix2_ok_more_ports2)
# [1] 9362   14

### rename the state column ----
# Explanations:
# 1. Removing the 'state_fixed.orig' column using 'select(-state_fixed.orig)'.
# 2. Renaming the 'state_fixed1' column to 'state_fixed' using 'rename("state_fixed" = state_fixed1)'.
vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports1 <-
  vessels_from_pims_split_addr__city_state__fix2_ok_more_ports2 |>
  select(-state_fixed.orig) |>
  rename("state_fixed" = state_fixed1)

### the same for cities ----
vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports <-
  vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports1 |>
  select(-city_fixed.orig) |>
  rename("city_fixed" = city_fixed1)

# check
dim(vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports)
# 9362   12

vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports |>
  filter(year_permit_sa_gom_dual == "2023 sa_only") |> 
  # filter(state_fixed == "NA" | is.na(state_fixed)) |> 
  filter(city_fixed == "NA" | is.na(city_fixed)) |> 
  nrow()
# 0

# Print results ----
cat("vessels_from_pims_split_addr__city_state__fix2_ok_more_ports_more_ports",
    sep = "\n")

# Jenny's check ----
#just keep vessel official number and state
ports_vsls_cnts <- 
  vessels_from_pims_split_addr__city_state__fix2_ok_short |> 
  select(vessel_official_number, state_fixed) |> 
  distinct() |> 
  count(vessel_official_number)

double_ports <-
  ports_vsls_cnts |>
  filter(n > 1)

dim(double_ports)
# 17 no uniqued
# 5 uniqued
   # vessel_official_number     n
#    <chr>                  <int>
#  1 1112053                    2
#  2 1166732                    2
#  3 596153                     2
#  4 671353                     2
#  5 FL                         8
#  6 FL1862SU                   2
#  7 FL5262LD                   2
#  8 FL8000NR                   2
#  9 FLORIDA                    2
# 10 LA4017BH                   2
# 11 LA6968EP                   2
# 12 MD                         2
# 13 N/A                        2
# 14 NA                         9
# 15 NC6164CW                   2
# 16 NO                         2
# 17 NONE                       3

# double_ports
# 1 1112053                    2 NEW BERN, NC
# 2 596153                     2 NEW BERN, NC
# 3 671353                     2 SWANSBORO, NC
# 4 N/A                        2
# 5 NA                         5

# vessels_from_pims_split_addr__city_state__fix2_ok_short |> 
#   filter(vessel_official_number %in% double_ports$vessel_official_number) |> 
#   View()

# check empty vessel ids ----
is_empty <- c(NA, "NA", "", "UN", "N/A")

vessels_from_pims_split_addr__city_state__fix2_ok_short__good_ids <-
  vessels_from_pims_split_addr__city_state__fix2_ok_short |>
  filter(!vessel_official_number %in% is_empty)
  
dim(vessels_from_pims_split_addr__city_state__fix2_ok_short)
# [1] 23073     3
dim(vessels_from_pims_split_addr__city_state__fix2_ok_short__good_ids)
# [1] 23062     3

# vessels_from_pims |> 
#   filter(grepl("LIGHTHOUSE", hailing_port)) |>
#   filter(!grepl("NOVESID", official__)) |> 
#   View()
# LIGHTHOUSE

# grep("NOVES", vessels_from_pims_split_addr__city_state__fix2_ok_short$vessel_official_number, value = T)
# [1] "NOVESSEL"

vessels_from_pims_split_addr__city_state__fix2_ok_short__good_ids |> 
  select(vessel_official_number, state_fixed) |> 
  distinct() |> 
  count(vessel_official_number) |> 
  filter(n > 1)
# 0! fixed