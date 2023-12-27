#' %%%%% Prepare data
#'
#' Questions:
#' How many SEFHIER vessels have a different start port county than end port county?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port state than end port state?
#'   Numbers, by quarter (1-4):
#' How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)?
#' Numbers, by quarter (1-4):
#' How many Gulf permitted SEFHIER vessels fish in both the Gulf and South Atlantic?
#'   Numbers, by quarter (1-4):

# setup current project ----
library(zoo)
# Determine the path of the executing script
library(this.path)

# Prints an R object in markdown, needed to print pretty table from list of dfs.
library(pander)

# maps:
library(mapview)
library(sf)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Get the current project directory name using the 'this.path' package.
current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

## additional data ----
# for qmd use #' {{< include .qmd >}} instead of source()

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
source(misc_info_path)

#' {{< include misc_info.qmd >}}
#'

boats_number_get_data_info_path <-
  file.path(current_project_dir_name,
            r"(boats_number_get_data.R)")

source(boats_number_get_data_info_path)

## processed_logbooks ----

# names(processed_logbooks_clean_names) |>
#   sort() |>
#   cat(sep = ", ")

# to use in lists
start_end_words <-
  c("start", "end")

port_fields_short <-
  c(
    "vessel_official_number",
    "end_port_name",
    "end_port_state",
    "end_port_county",
    "end_port",
    "permit_region",
    "start_port_name",
    "start_port_state",
    "start_port_county",
    "start_port",
    "trip_id",
    "trip_end_date",
    "trip_start_date",
    "latitude",
    "longitude"
  )

# grep("state", names(processed_logbooks_clean_names), value = T, ignore.case = T)
processed_logbooks_short <-
  processed_logbooks_clean_names |>
  dplyr::select(dplyr::all_of(port_fields_short)) |>
  remove_empty_cols() |>
  dplyr::distinct()

dim(processed_logbooks_short)
# [1] 3011    7
# [1] 2475    6 (no overridden)
# [1] 66641     8 with start and end dates
# [1] 72246    14 with lat/long
# [1] 73368    15 with trip_id

### add date related columns ----
tic("processed_logbooks_short_dates")
processed_logbooks_short_dates <-
  processed_logbooks_short |>
  dplyr::mutate(
    trip_start_week_num =
      strftime(trip_start_date, format = "%u"),
    trip_end_week_num =
      strftime(trip_end_date, format = "%u"),
    trip_start_y =
      lubridate::year(trip_start_date),
    trip_end_y =
      lubridate::year(trip_end_date),
    trip_start_m =
      zoo::as.yearmon(trip_start_date),
    trip_end_m =
      zoo::as.yearmon(trip_end_date),
    trip_start_year_quarter = zoo::as.yearqtr(trip_start_date),
    trip_start_quarter_num =
      format(trip_start_year_quarter, "%q"),
    trip_end_year_quarter = zoo::as.yearqtr(trip_end_date),
    trip_end_quarter_num =
      format(trip_end_year_quarter, "%q")
  )
toc()
# processed_logbooks_short_dates: 2.94 sec elapsed
# processed_logbooks_short_dates: 4.28 sec elapsed

## Prepare home_port data ----
# all_get_db_data_result_l |>
#   print_df_names()

# all_get_db_data_result_l$vessels_permits |>
  # print_df_names()

vessel_permit_port_info <-
  all_get_db_data_result_l$vessels_permits |>
  # active permits in 2022
  dplyr::filter(
    LAST_EXPIRATION_DATE > "2022-12-31" |
      END_DATE > "2022-12-31" |
      EXPIRATION_DATE > "2022-12-31"
  )

dim(vessel_permit_port_info)
# [1] 68113    51
# SERO_OFFICIAL_NUMBER  5220
# SERO_HOME_PORT_CITY    809
# SERO_HOME_PORT_COUNTY   320
# SERO_HOME_PORT_STATE     28

### remove unused columns ----

vessel_permit_port_info_short <-
  vessel_permit_port_info |>
  select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    # PORT_CODE, mostly empty
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER
  ) |>
  dplyr::ungroup() |>
  remove_empty_cols() |>
  dplyr::distinct()

n_distinct(vessel_permit_port_info_short$VESSEL_VESSEL_ID)
# VESSEL_VESSEL_ID      5220

vessel_permit_port_info_short_clean <-
  clean_headers(vessel_permit_port_info_short)

## add vessel_permit home port information to trip (logbook) information ----

# print_df_names(processed_logbooks_short_port_states_fl_reg_one_marker)
# print_df_names(vessel_permit_port_info_short_clean)

join_trip_and_vessel <-
  dplyr::left_join(
    processed_logbooks_short_dates,
    vessel_permit_port_info_short_clean,
    dplyr::join_by(vessel_official_number ==
                     sero_official_number)
  )

dim(join_trip_and_vessel)
# [1] 3011   20
# [1] 2475   19 (processed logbooks)
# [1] 66641    35
# [1] 72246    29 with lat/long
# [1] 73368    30 with trip_id

# with overridden
# vessel_id             1876
# vessel_official_number   1876
# permit_sa_gom            4
# SERO_OFFICIAL_NUMBER  1785

# w/o overridden
# vessel_official_number 1629
# permit_region             2
# PERMIT_VESSEL_ID        1562
# VESSEL_VESSEL_ID        1562
# SERO_HOME_PORT_CITY      361
# SERO_HOME_PORT_COUNTY    145
# SERO_HOME_PORT_STATE      20
# latitude                52887
# longitude               53423

# remove trailing spaces

join_trip_and_vessel_trim <-
  join_trip_and_vessel |>
  mutate_if(is.character, str_trim)

diffdf::diffdf(join_trip_and_vessel,
               join_trip_and_vessel_trim)

## Lower case of all data ----
join_trip_and_vessel_low <-
  join_trip_and_vessel_trim |>
  mutate_if(is.character, tolower)

# remove not a-z in strings (e.g. "St. John" or
# "miami dade" vs. "miami-dade")

join_trip_and_vessel_clean <-
  join_trip_and_vessel_low |>
  mutate_if(is.character,
         ~str_replace_all(., "[^a-z0-9]+", " "))

# diffdf::diffdf(join_trip_and_vessel_low,
#                join_trip_and_vessel_clean)

# An aux function to add regions to states ----

# join_trip_and_vessel_clean |>
#   select(start_port_state) |>
#   distinct()

add_region_to_state <-
  function(my_df, start_or_end) {
    result_column_name <-
      str_glue("{start_or_end}_state_region")

    port_state_column <-
      sym(str_glue("{start_or_end}_port_state"))

    port_county_column <-
      sym(str_glue("{start_or_end}_port_county"))

    is_st_florida <-
      rlang::quo(!!port_state_column == "fl")

    is_gom_state <-
      rlang::quo(my_state_name[[!!port_state_column]]
                 %in% east_coast_states$gom)

    is_gom_fl_county <-
      rlang::quo(!!port_county_column %in% tolower(fl_counties$gom))

    new_df <-
      my_df |>
      rowwise() |>
      mutate(
        !!result_column_name :=
          case_when(
            !(!!is_st_florida) & !!is_gom_state ~ "gom",

            !!is_st_florida & !!is_gom_fl_county ~ "gom",

            is.na(!!port_state_column) ~ NA,

            .default = "sa"
          )
      ) |>
      ungroup()

    return(new_df)
  }

# TODO: Why there is no home port?
join_trip_and_vessel_clean |>
  filter(is.na(sero_home_port_state)) |>
  select(contains("port")) |>
  distinct() |>
  glimpse()

# Add port state regions ----
# ?? Use a start port state instead of filter(!is.na(sero_home_port_state))
tic("join_trip_and_vessel_clean_state_regions")
join_trip_and_vessel_clean_state_regions <-
  join_trip_and_vessel_clean |>
  filter(!is.na(sero_home_port_state)) |>
  add_region_to_state("sero_home") |>
  add_region_to_state("end")
toc()
# join_trip_and_vessel_clean_state_regions: 33.63 sec elapsed

# Split by home port regions ----
join_trip_and_vessel_clean_state_regions_l <-
  join_trip_and_vessel_clean_state_regions |>
  split(as.factor(
    join_trip_and_vessel_clean_state_regions$sero_home_state_region
  ))

map(join_trip_and_vessel_clean_state_regions_l,
    count_uniq_by_column)

# $gom
# vessel_official_number    944
# $sa
# vessel_official_number    617

# dim(join_trip_and_vessel_clean_state_regions_l$gom)
# [1] 53582    32

## Short GOM df ----
columns_to_keep <- c(
  "vessel_official_number",
  "trip_id",
  "sero_home_port_county",
  "sero_home_port_state",
  "end_port_county",
  "end_port_state"
)

short_port_gom <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(all_of(columns_to_keep),
         contains("region"),
         contains("quarter")) |>
  # can use distinct, because we are not interested in the number of such trips, just the number of vessel
  distinct()

#' %%%%% Boat movement numbers for GOM
#'

# How many GOM SEFHIER vessels have a different start port county than end port county? ----

## different counties ----
start_end_county_diff_gom <-
  short_port_gom |>
  group_by(vessel_official_number,
           trip_id,
           permit_region,
           trip_end_year_quarter) |>
  filter(!sero_home_port_county == end_port_county) |>
  ungroup() |>
  select(-trip_id) |>
  distinct()

### check different start and end quarters ----
join_trip_and_vessel_clean |>
  select(trip_start_year_quarter, trip_end_year_quarter) |>
  distinct() |>
  filter(!trip_start_year_quarter == trip_end_year_quarter)
  # trip_start_year_quarter trip_end_year_quarter
# 1                 2022 Q1               2022 Q2
# 2                 2022 Q2               2022 Q3

start_end_county_diff_gom |>
  filter(vessel_official_number %in% c("al4295ak", "1270320")) |>
  glimpse()

join_trip_and_vessel_trim |>
  filter(grepl("monroe", sero_home_port_county, ignore.case = T)) |>
  filter(grepl("baldwin", end_port_county, ignore.case = T)) |>
  select(
    vessel_official_number,
    sero_home_port_county,
    sero_home_port_state,
    end_port_county,
    end_port_state,
    # trip_end_quarter_num,
    trip_end_year_quarter
  ) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  glimpse()

## count different counties ----
# start_end_county_diff |> print_df_names()

start_end_county_diff_gom_num <-
  start_end_county_diff_gom |>
  add_count(permit_region, sero_home_port_county,
            end_port_county, trip_end_year_quarter,
            name = "cnt_diff_county")

dim(start_end_county_diff_gom_num)
# [1] 250   5
# [1] 575  11
# [1] 291  13 gom only

### spot check counts ----
join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# [1] 2

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  head()
#   vessel_official_number trip_end_year_quarter
# 1                 958876               2022 Q1
# 2                 958876               2022 Q2
# 3               fl5321kd               2022 Q2
# 4               fl5321kd               2022 Q3

# join_trip_and_vessel_clean |>
#   filter(sero_home_port_county == "santa rosa" &
#            end_port_county == "escambia") |>
#   select(trip_end_year_quarter, vessel_official_number) |>
#   distinct() |>
#   arrange(trip_end_year_quarter, vessel_official_number) |>
#   View()

# vessels by quarter, correct
start_end_county_diff_gom_num |>
  filter(sero_home_port_county == "collier" &
           end_port_county == "lee") |>
  arrange(trip_end_quarter_num) |>
  glimpse()
# $ trip_end_quarter_num    <chr> "1", "2", "2", "3"
# $ cnt_diff_county         <int> 1, 2, 2, 1

## Result table for GOM permit region (and the state region) ----
start_end_county_diff_gom_num_gom_permit_only <-
  start_end_county_diff_gom_num |>
  filter(permit_region == "gom")

# check
dim(start_end_county_diff_gom_num_gom_permit_only)
# [1] 270  13

# state == gom, permit gom or sa
start_end_county_diff_gom_num |>
# select(sero_home_port_state) |>
# distinct() |> # 5 states
dim()
# [1] 291  13

#### make the result table ----
# print_df_names(start_end_county_diff_gom_num_gom_permit_only)

start_end_county_diff_gom_num_gom_permit_only_res <-
  start_end_county_diff_gom_num_gom_permit_only |>
  select(-permit_region) |>
  rowwise() |>
  mutate(home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  mutate(
    home_port_county = str_to_title(sero_home_port_county),
    end_port_county = str_to_title(end_port_county)
  ) |>
  ungroup() |>
  select(
    -c(
      sero_home_port_county,
      sero_home_port_state,
      trip_start_year_quarter,
      trip_start_quarter_num,
      trip_end_quarter_num,
      vessel_official_number
    )
  ) |>
  relocate(
    trip_end_year_quarter,
    home_port_state,
    home_port_county,
    end_port_state,
    end_port_county,
    diff_county_num_of_vessels = cnt_diff_county
  ) |>
  distinct()
# Can use distinct here, because we only look at county to county vessels, not trips

# spot check
start_end_county_diff_gom_num_gom_permit_only_res |>
    filter(home_port_county == "Pasco" &
           end_port_county == "Pinellas" &
           trip_end_year_quarter == "2022 Q1" ) |>
  glimpse()

join_trip_and_vessel_clean |>
  filter(sero_home_port_county == "pasco" &
           end_port_county == "pinellas" &
           trip_end_year_quarter == "2022 Q1" &
           permit_region == "gom") |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # arrange(vessel_official_number) |>
  count(vessel_official_number)
#   vessel_official_number  n
# 1                1070441 11
# 2               fl2045nb 59
# 2 vessels, ok

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    # home_port_county == "Brazoria" &
    #   end_port_county == "Galveston" &
      trip_end_year_quarter == "2022 Q4"
  ) |>
  count(wt = diff_county_num_of_vessels)
# 47 tot Q4
# 2 for Brazoria - Galveston

join_trip_and_vessel_clean |>
  filter(
    sero_home_port_county == "brazoria" &
      end_port_county == "galveston" &
      trip_end_year_quarter == "2022 Q4" &
      permit_region == "gom"
  ) |>
  select(vessel_official_number, trip_end_year_quarter) |>
  # distinct()
# 970060
# tx2118fj
  count(vessel_official_number) |>
  count()
# 2, correct


# join_trip_and_vessel_clean |>
#   filter(
#     sero_home_port_county == "pinellas" &
#       end_port_county == "brunswick"
#   ) |>
#   select(vessel_official_number, trip_end_year_quarter) |>
# View()
# 1 vessel, correct

join_trip_and_vessel_trim |>
  filter(grepl("monroe", sero_home_port_county, ignore.case = T)) |>
  filter(grepl("baldwin", end_port_county, ignore.case = T)) |>
  select(
    vessel_official_number,
    sero_home_port_county,
    sero_home_port_state,
    end_port_county,
    end_port_state,
    # trip_end_quarter_num,
    trip_end_year_quarter
  ) |>
  distinct() |>
  arrange(trip_end_year_quarter) |>
  glimpse()
# Q2 2
# Q3 1

start_end_county_diff_gom_num_gom_permit_only_res |>
  filter(
    home_port_county == "Monroe" &
      end_port_county == "Baldwin"
  ) |>
  count(trip_end_year_quarter,
        wt = diff_county_num_of_vessels)
# 1 2022 Q2                   2
# 2 2022 Q3                   1
# ok

##### Write out ----

write_csv(
  start_end_county_diff_gom_num_gom_permit_only_res,
  file.path(
    curr_proj_output_path,
    "start_end_county_diff_gom_num_gom_permit_only_res.csv"
  )
)

#### Diff county numbers, by quarter (1-4) ----
start_end_county_diff_gom_num_gom_permit_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_county_num_of_vessels_tot")

head(start_end_county_diff_gom_num_gom_permit_only_res_quarter)
#   trip_end_year_quarter diff_county_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                           24
# 2 2022 Q2                                          103
# 3 2022 Q3                                           96
# 4 2022 Q4                                           47

# How many SEFHIER vessels have a different start port state than end port state? ----

## different start and end states in one trip ----

start_end_state_diff_num_gom_only_res <-
  short_port_gom |>
  filter(permit_region == "gom") |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state,
    trip_end_year_quarter
  ) |>
  distinct() |>
  filter(!sero_home_port_state == end_port_state) |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        end_port_state,
        name = "diff_states_num_of_vessels")

# View(start_end_state_diff_num_gom_only_res)

### spot check ----

start_end_state_diff_num_gom_only_res |>
  filter(trip_end_year_quarter == "2022 Q1") |>
  glimpse()

start_end_state_diff_num_gom_only_res |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_year_quarter == "2022 Q2") |>
  glimpse()
# 2022 Q2
# Florida
# Alabama
# 5

join_trip_and_vessel_clean |>
  filter(sero_home_port_state == "fl" &
           end_port_state == "al" &
           trip_end_quarter_num == 2) |>
  select(vessel_official_number,
         trip_end_year_quarter,
         sero_home_port_state,
         end_port_state) |>
  distinct() |>
  nrow()
# 5
# OK

## Diff states numbers, by quarter (1-4) ----

start_end_state_diff_num_gom_only_res_quarter <-
  start_end_state_diff_num_gom_only_res |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_tot") |>
  ungroup()

head(start_end_state_diff_num_gom_only_res_quarter)
#   trip_end_year_quarter diff_states_num_of_vessels_tot
#   <yearqtr>                                      <int>
# 1 2022 Q1                                            3
# 2 2022 Q2                                           30
# 3 2022 Q3                                           26
# 4 2022 Q4                                           10

### spot check ----
# 2022 Q4
join_trip_and_vessel_clean_state_regions |>
  filter(sero_home_state_region == "gom") |>
  filter(permit_region == "gom") |>
  filter(trip_end_quarter_num == 4) |>
  filter(!sero_home_port_state == end_port_state) |>
  select(
    vessel_official_number,
    sero_home_port_state,
    end_port_state
  ) |>
  distinct() |>
  arrange(sero_home_port_state,
          end_port_state) |>
  # View()
  count(sero_home_port_state,
        end_port_state) |>
  glimpse()
# ok

## save results to csv ----
start_end_state_diff_num_gom_only_res |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  mutate(end_port_state =
           my_state_name[[end_port_state]]) |>
  ungroup() |>
  write_csv(file.path(curr_proj_output_path,
                      "start_end_state_diff_num_gom_only_res.csv"),)


start_end_state_diff_num_gom_only_res_cnts_by_home <-
  start_end_state_diff_num_gom_only_res |>
  count(
    trip_end_year_quarter,
    sero_home_port_state,
    end_port_state,
    wt = diff_states_num_of_vessels,
    name = "states_cnt"
  ) |>
  mutate(
    sero_home_port_state = toupper(sero_home_port_state),
    end_port_state = toupper(end_port_state)
  )

start_end_state_diff_num_gom_only_res_cnts_by_home_sum <-
  start_end_state_diff_num_gom_only_res_cnts_by_home |>
  add_count(trip_end_year_quarter,
            sero_home_port_state,
            wt = states_cnt,
            name = "sum_by_q_and_home")

start_end_state_diff_num_gom_only_res_cnts_by_home_sum |>
  write_csv(
    file.path(
      curr_proj_output_path,
      "start_end_state_diff_num_gom_only_res_cnts_by_home_sum.csv"
    )
  )

## State to state by state and quarter res table ----
start_end_state_diff_num_gom_only_res_home <-
  start_end_state_diff_num_gom_only_res |>
  count(trip_end_year_quarter,
        sero_home_port_state,
        wt = diff_states_num_of_vessels,
        name = "diff_states_num_of_vessels_home") |>
  rowwise() |>
  mutate(sero_home_port_state =
           my_state_name[[sero_home_port_state]]) |>
  ungroup()

write_csv(
  start_end_state_diff_num_gom_only_res_home,
  file.path(curr_proj_output_path,
            "start_end_state_diff_num_gom_only_res_home.csv"),
)


# How many SEFHIER vessels have a different start port region (Gulf) than end port region (South Atlantic)? ----

start_end_state_region_diff_num_gom_only_res_quarter <-
  start_end_county_diff_gom_num_gom_permit_only_res |>
  select(
    trip_end_year_quarter,
    sero_home_state_region,
    end_state_region,
    diff_county_num_of_vessels
  ) |>
  distinct() |>
  filter(!sero_home_state_region == end_state_region) |>
  group_by(trip_end_year_quarter) |>
  count(wt = diff_county_num_of_vessels,
        name = "diff_port_regions_num_of_vessels_tot")

# result for state region to region
# 3 in the whole year (for GOM home port, and GOM permit)
#   trip_end_year_quarter diff_port_regions_num_of_vessels_tot
#   <yearqtr> <int>
# 1 2022 Q1   1
# 2 2022 Q2   1
# 3 2022 Q4   1

#' Nothing to show, only 1 vessel has trips starting in Fl and ending in North Carolina in Q2 and 1 vessel in Q4.
#' Plus 1 vessel in Q1 from Sarasota, Florida to Duval, Florida

# Quantify the # of vessels who fish in both the gulf and S Atl ----
# Home ports are in GOM

## prep fishing locations ----
# [1] "vessel_official_number, end_port_name, end_port_state, end_port_county, end_port, permit_region, start_port_name, start_port_state, start_port_county, start_port, trip_id, trip_end_date, trip_start_date, latitude, longitude, trip_start_week_num, trip_end_week_num, trip_start_y, trip_end_y, trip_start_m, trip_end_m, trip_start_year_quarter, trip_start_quarter_num, trip_end_year_quarter, trip_end_quarter_num, permit_vessel_id, vessel_vessel_id, sero_home_port_city, sero_home_port_county, sero_home_port_state, sero_home_state_region, end_state_region"

lat_lon_gom_state <-
  join_trip_and_vessel_clean_state_regions_l$gom |>
  select(vessel_official_number,
         permit_region,
         latitude,
         longitude,
         trip_end_year_quarter) |>
  filter(!is.na(latitude) &
           !is.na(longitude)) |>
  distinct()

# dim(lat_lon_gom_state)
# [1] 46181     5

lat_lon_gom_state_cnt <-
  lat_lon_gom_state |>
  mutate(latitude = abs(latitude),
         longitude = -abs(longitude)) |>
  add_count(latitude, longitude,
            name = "cnt_v_coords_by_y") |>
  add_count(latitude,
            longitude,
            trip_end_year_quarter,
            name = "cnt_v_coords_by_q")

# View(lat_lon_gom_state_cnt)
my_crs <- 4326

# Create a new object 'lat_lon_gom_state_cnt_sf' by piping the data frame
# 'lat_lon_gom_state_cnt' into the st_as_sf function from the sf package

# The st_as_sf function is used to convert a data frame with latitude and
# longitude columns into an sf (simple feature) object
  # Specify the latitude and longitude columns for the sf object
    # Set the coordinate reference system (CRS) for the sf object
    # Keep the original columns in the resulting sf object

lat_lon_gom_state_cnt_sf <-
  lat_lon_gom_state_cnt |>
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = my_crs,
    remove = FALSE
  )

# str(lat_lon_gom_state_cnt_sf)
# lat_lon_gom_state_cnt_sf |>
#   mapview(
#     zcol = "permit_region",
#     cex = "cnt_v_coords_by_y",
#     alpha = 0.3,
#     col.regions = viridisLite::turbo,
#     # legend = FALSE
#     layer.name = "GOM home port trips"
#   )

## split by region using shape files ----
waters_shape_prep_path <-
  file.path(my_paths$git_r,
            r"(get_data\waters_shape_prep.R)")

# file.exists(waters_shape_prep_path)

source(waters_shape_prep_path)

## sa fishing ----
### state waters, Monroe in GOM ----
tic("sa_lat_lon_gom_state_cnt_sf_state_w")
sa_lat_lon_gom_state_cnt_sf_state_w <-
  st_intersection(sa_only_fl_state_waters_shp,
                  lat_lon_gom_state_cnt_sf)
toc()
# sa_lat_lon_gom_state_cnt_sf_state_w: 0.18 sec elapsed

# mapview(sa_lat_lon_gom_state_cnt_sf_state_w)

# SA fed waters ----
get_sa_lat_lon_gom_state_cnt_sf_fed_w <-
  function(sa_shp_4326,
           lat_lon_gom_state_cnt_sf) {
    sa_lat_lon_gom_state_cnt_sf_fed_w <-
      st_intersection(sa_shp_4326,
                      lat_lon_gom_state_cnt_sf)
    return(sa_lat_lon_gom_state_cnt_sf_fed_w)
  }
# sa_lat_lon_gom_state_cnt_sf_fed_w: 84.14 sec elapsed

sa_lat_lon_gom_state_cnt_sf_fed_w_file_path <-
  file.path(curr_proj_output_path,
            "sa_lat_lon_gom_state_cnt_sf_fed_w.rds")

# file.exists(sa_lat_lon_gom_state_cnt_sf_fed_w_file_path)
# readr::write_rds(sa_lat_lon_gom_state_cnt_sf_fed_w,
#                  sa_lat_lon_gom_state_cnt_sf_fed_w_file_path)

sa_lat_lon_gom_state_cnt_sf_fed_w <-
  read_rds_or_run_no_db(
    sa_lat_lon_gom_state_cnt_sf_fed_w_file_path,
    list(sa_shp_4326,
         lat_lon_gom_state_cnt_sf),
    get_sa_lat_lon_gom_state_cnt_sf_fed_w
  )

# mapview(sa_lat_lon_gom_state_cnt_sf_fed_w)

# str(sa_lat_lon_gom_state_cnt_sf_fed_w)

## GOM fishing ----

get_gom_lat_lon_gom_state_cnt_sf_fed_w <-
  function(GOMsf,
           lat_lon_gom_state_cnt_sf) {
    gom_lat_lon_gom_state_cnt_sf_fed_w <-
      st_intersection(GOMsf,
                      lat_lon_gom_state_cnt_sf)
    return(gom_lat_lon_gom_state_cnt_sf_fed_w)
  }

gom_lat_lon_gom_state_cnt_sf_fed_w_file_path <-
  file.path(curr_proj_output_path,
            "gom_lat_lon_gom_state_cnt_sf_fed_w.rds")

# file.exists(gom_lat_lon_gom_state_cnt_sf_fed_w_file_path)

gom_lat_lon_gom_state_cnt_sf_fed_w <-
  read_rds_or_run_no_db(
    gom_lat_lon_gom_state_cnt_sf_fed_w_file_path,
    list(GOMsf,
         lat_lon_gom_state_cnt_sf),
    get_gom_lat_lon_gom_state_cnt_sf_fed_w
  )
# run the function: 56.91 sec elapsed

str(gom_lat_lon_gom_state_cnt_sf_fed_w)

## join by vessel ----
### back to dfs for join ----
gom_lat_lon_gom_state_cnt_fed_w_df <-
  st_drop_geometry(gom_lat_lon_gom_state_cnt_sf_fed_w)
# str(gom_lat_lon_gom_state_cnt_fed_w_df)

sa_lat_lon_gom_state_cnt_sf_fed_w_df <-
  st_drop_geometry(sa_lat_lon_gom_state_cnt_sf_fed_w)
# str(sa_lat_lon_gom_state_cnt_sf_fed_w_df)

sa_lat_lon_gom_state_cnt_sf_state_w_df <-
  st_drop_geometry(sa_lat_lon_gom_state_cnt_sf_state_w)
# str(sa_lat_lon_gom_state_cnt_sf_state_w_df)

keep_sa_fields <-
  intersect(
    names(sa_lat_lon_gom_state_cnt_sf_fed_w_df),
    names(sa_lat_lon_gom_state_cnt_sf_state_w_df)
  )

all_points_sa <-
  list(sa_lat_lon_gom_state_cnt_sf_fed_w_df,
       sa_lat_lon_gom_state_cnt_sf_state_w_df) |>
  map_df(\(curr_df) {
    curr_df |>
      select(all_of(keep_sa_fields))
  })

# View(all_dots_sa)
# check 994360 in state waters

all_fish_points <-
  full_join(
    gom_lat_lon_gom_state_cnt_fed_w_df,
    all_points_sa,
    join_by(vessel_official_number,
            permit_region,
            trip_end_year_quarter),
    relationship = "many-to-many",
    suffix = c(".gom", ".sa")
  )

# ?? join by trip_end_year_quarter?
# View(all_fish_points)
# grep("\\.x", names(all_dots), value = T)
# [1] "permit_region.x"         "latitude.x"              "longitude.x"
# [4] "trip_end_year_quarter.x" "cnt_v_coords_by_y.x"     "cnt_v_coords_by_q.x"

all_fish_points_reg_y <-
  all_fish_points |>
  group_by(vessel_official_number,
           permit_region) |>
  mutate(has_gom_point_y =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_y =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

all_fish_points_reg_both_y <-
  all_fish_points_reg_y |>
  filter(has_gom_point_y & has_sa_point_y)

# data_overview(all_fish_points_reg_both_y)
# [1] 8524   18
# vessel_official_number   76

### same by quarter ----
all_fish_points_reg_q <-
  all_fish_points |>
  group_by(vessel_official_number,
           permit_region,
           trip_end_year_quarter) |>
  mutate(has_gom_point_q =
           any(!is.na(latitude.gom), na.rm = TRUE),
         has_sa_point_q =
           any(!is.na(latitude.sa), na.rm = TRUE)) |>
  ungroup()

all_fish_points_reg_both_q <-
  all_fish_points_reg_q |>
  filter(has_gom_point_q & has_sa_point_q)

# View(all_fish_points_reg_both_q)
# [1] 7145   18

n_distinct(all_fish_points_reg_both_q$vessel_official_number)
# [1] 74

all_fish_points_reg_both_q |>
  select(trip_end_year_quarter, vessel_official_number) |>
  distinct() |>
  count(trip_end_year_quarter)
#   trip_end_year_quarter     n
#   <yearqtr>             <int>
# 1 2022 Q1                  35
# 2 2022 Q2                  31
# 3 2022 Q3                  35
# 4 2022 Q4                  24

all_fish_points_reg_both_q
