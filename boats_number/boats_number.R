library(zoo)
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_dir_path <-
  get_current_file_directory()
current_project_dir_name <- basename(current_project_dir_path)

source(file.path(my_paths$git_r,
                 r"(get_data\misc_info.R)"))

source(file.path(my_paths$git_r,
                 r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)"))

# all_get_db_data_result_l
# all_logbooks_db_data_2022_short_p_region
# ls()

print_df_names(all_logbooks_db_data_2022_short_p_region)
# [1] "trip_id, trip_type_name, vessel_id, vessel_official_nbr, vessel_name, trip_start_date, trip_end_date, state, state_name, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, activity_type_name, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit, vendor_app_name, vendor_platform, trip_de, trip_ue, trip_dc, trip_uc, area_code, sub_area_code, distance_code, distance_code_name, local_area_code, latitude, longitude, effort_de, effort_ue, effort_dc, effort_uc, catch_uc, user_app, notif_seq, notif_type, notif_accsp_system_id, notif_accsp_permit_id, notif_trip_type, notif_trip_type_name, notif_trip_start_date, notif_trip_start_time, notif_trip_end_date, notif_trip_end_time, notif_start_port, notif_start_port_name, notif_start_port_county, notif_start_port_state, notif_end_port, notif_end_port_name, notif_end_port_county, notif_end_port_state, notif_cancel_flag, notif_email_sent, notif_intended_fishing_flag, notif_gear_type, notif_landing_location, notif_landing_location_name, notif_landing_location_city, notif_landing_location_county, notif_landing_location_state, notif_stat_zone, notif_ue, notif_de, notif_uc, notif_dc, permit_region"

# shorten ----
port_fields_short <-
  c(
    "vessel_id",
    "vessel_official_nbr",
    "permit_region",
    "start_port_name",
    "end_port_name",
    "start_port",
    "end_port"
  )

all_logbooks_db_data_2022_short_p_region_short <-
  all_logbooks_db_data_2022_short_p_region |>
  select(all_of(port_fields_short)) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_short)
# [1] 3011    7

all_logbooks_db_data_2022_short_p_region_short |>
  filter(!start_port == end_port &
           start_port_name == end_port_name) |>
  glimpse()
# 2
# $ vessel_id           <int> 326764, 254794
# $ vessel_official_nbr <chr> "NC8438DJ", "FL0291MX"
# $ permit_region       <chr> "sa_only", "sa_only"
# $ start_port_name     <chr> "WRIGHTSVILLE BEACH", "KEYS FISHERIES"
# $ end_port_name       <chr> "WRIGHTSVILLE BEACH", "KEYS FISHERIES"
# $ start_port          <chr> "361133", "118530"
# $ end_port            <chr> "362333", "111949"

# all_logbooks_db_data_2022_short_p_region_short |>
#   filter(start_port == end_port &
#            !start_port_name == end_port_name) |>
# str()
# 0
my_vars <- function() {
  c(any_of(c("name", "species")), ends_with("color"))
}

port_fields_all <-
  function(variables) {
    c(all_of(c(
      "vessel_id",
      "vessel_official_nbr",
      "permit_region"
    )),
    contains("port")
    # ,
    # -starts_with("notif")
    )
  }

all_logbooks_db_data_2022_short_p_region_port_fields_all <-
  all_logbooks_db_data_2022_short_p_region |>
  select(port_fields_all(), -starts_with("notif")) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_port_fields_all)
# [1] 3011   11

## add date columns ----
tic("all_logbooks_db_data_2022_short_p_region_dates")
all_logbooks_db_data_2022_short_p_region_dates <-
  all_logbooks_db_data_2022_short_p_region |>
  dplyr::mutate(
    trip_start_week_num =
      strftime(trip_start_date, format = "%u"),
    trip_end_week_num =
      strftime(trip_end_date, format = "%u"),
    trip_start_y =
      year(trip_start_date),
    trip_end_y =
      year(trip_end_date),
    trip_start_m =
      zoo::as.yearmon(trip_start_date),
    trip_end_m =
      zoo::as.yearmon(trip_end_date),
    trip_start_year_quarter = as.yearqtr(trip_start_date),
    trip_start_quarter_num =
      format(trip_start_year_quarter, "%q")
  )
toc()
# all_logbooks_db_data_2022_short_p_region_dates: 2.94 sec elapsed

all_logbooks_db_data_2022_short_p_region_dates_trip_port <-
  all_logbooks_db_data_2022_short_p_region_dates |>
  select(port_fields_all(),
         -starts_with("notif"),
         starts_with("trip_")) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_dates_trip_port)
# [1] 94366    27

# how many SEFHIER vessels start at a different location than they end; ----
all_logbooks_db_data_2022_short_p_region_short |>
  # filter(!start_port == end_port) |>
  filter(!start_port_name == end_port_name) |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region) |>
  distinct() |>
  # dim()
  # Rows: 397
  count(permit_region)
# 1  gom_and_dual 198
# 2       sa_only 199
# by name:
# 1  gom_and_dual 198
# 2       sa_only 197

# if keep ports:
# dim()
# 499
# 1  gom_and_dual 273
# 2       sa_only 226

# how many vessels have variable landing locations (i.e., in the winter they are in one state while in the summer they fish in another); ----

add_all_port_string <-
  function(my_df,
           group_by_vector = c("vessel_id", "vessel_official_nbr")) {
  my_df |>
    group_by_at(group_by_vector) |>
    mutate(all_start_ports = toString(unique(start_port)),
           all_end_ports   = toString(unique(end_port))) |>
    mutate(
      all_start_ports_num = length(str_split(all_start_ports, ",")),
      all_end_ports_num   = length(str_split(all_end_ports, ","))
    ) |>
    ungroup() %>% #can't be |> , doesn't work with return()
    return()
}

all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl <-
  add_all_port_string(all_logbooks_db_data_2022_short_p_region_short)

dim(all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl)
# [1] 3011   11

add_all_port_name_string <-
  function(my_df,
           group_by_vector = c("vessel_id", "vessel_official_nbr")) {
    my_df |>
      group_by_at(group_by_vector) |>
      mutate(
        all_start_port_names = toString(unique(start_port_name)),
        all_end_port_names   = toString(unique(end_port_name))
      ) |>
      mutate(
        all_start_port_names_num = length(str_split(all_start_port_names, ",")),
        all_end_port_names_num   = length(str_split(all_end_port_names, ","))
      ) |>
      ungroup() %>%
      return()
  }

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl <-
  add_all_port_name_string(all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl)

dim(all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl)
# [1] 3011   15

all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  # View()
  filter(all_start_ports_num > 1) |>
  dim()
# [1] 1890    11

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  filter(!all_end_ports_num == all_end_port_names_num) |>
  dim()
# 0

all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  filter(vessel_official_nbr == 1000042) |>
  glimpse()

all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl_gom_mult_port <-
  all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  filter(permit_region == "gom_and_dual" &
           all_end_ports_num > 1)

# dim(all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl_gom_mult_port)
# [1] 1890   11
# [1] 1030   11

## we should look at this by quarter, to start - for some seasonality. ----
### trips and quarter fields ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port |>
  select(
    port_fields_all(),
    -starts_with("notif"),
    c(trip_start_year_quarter,
      trip_start_quarter_num)
  ) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short)
# [1] 6604   13

# a quarter is the same, a port - diff
### lists of start and end ports by quarter ----
group_by_vector <-
  c("vessel_id",
    "vessel_official_nbr",
    "trip_start_quarter_num")

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
    group_by_at(group_by_vector) |>
    mutate(all_start_ports_by_q = toString(unique(sort(start_port))),
           all_end_ports_by_q   = toString(unique(sort(end_port)))) |>
  ungroup()

# test
# all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q |>
#   filter(vessel_official_nbr == "1057052") |>
#   glimpse()

### count port groups (lists) ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q |>
  group_by(vessel_official_nbr) |>
  mutate(
    count_start_ports_by_q = n_distinct(all_start_ports_by_q),
    count_end_ports_by_q   = n_distinct(all_end_ports_by_q)
  ) |>
  ungroup()

# test
# all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt |>
#     filter(vessel_official_nbr == "1057052") |>
#   glimpse()
#
# all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt |>
#   filter(count_start_ports_by_q > 1) |>
#   arrange(trip_start_year_quarter) |>
#   filter(permit_region == "gom_and_dual") |>
#   filter(vessel_id == "328032") |>
#   glimpse()

### if a num of lists of ports by quarter > 1, than ports are different from Q to Q ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt_w_diff_ports_by_quarter <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt |>
  filter(count_start_ports_by_q > 1 |
           count_end_ports_by_q > 1) |>
  arrange(vessel_official_nbr, trip_start_year_quarter) |>
  filter(permit_region == "gom_and_dual")

# glimpse(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt_w_diff_ports_by_quarter)

## different locations with a combine table ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3 <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  select(vessel_official_nbr,
         trip_start_year_quarter,
         start_port_name) |>
  distinct()

# View(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3)
# [1] 6317    3
# vessel_official_nbr     1876
# trip_start_year_quarter    4
# start_port_name          531

### melt and decast the table ----
# one row per vessel
# In summary, the code transforms the data from a long to a wide format, spreading the values from the 'start_port_name' column across columns named by 'trip_start_year_quarter', and aggregating multiple values into a comma-separated string.

# The pivot_wider() function from the tidyr package is used to reshape the data.
# It takes the 'vessel_official_nbr' column as the identifier columns,
# and then it spreads the values from the 'trip_start_year_quarter' column into
# separate columns, with the corresponding values being taken from the
# 'start_port_name' column.
# The values_fn parameter is specified to define how to handle multiple
# values that may exist for a combination of 'vessel_official_nbr' and
# 'trip_start_year_quarter'. In this case, unique values are sorted and
# concatenated into a comma-separated string.

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3 |>
  pivot_wider(
    id_cols = vessel_official_nbr,
    names_from = trip_start_year_quarter,
    values_from = start_port_name,
    values_fn = ~ paste(unique(sort(.x)), collapse = ",")
  )

### add column for the same or diff ----
# It starts by using the rowwise() function to apply subsequent operations
# to each row individually, ensuring that calculations are row-wise.

# The mutate() function is then used to create a new column 'same'.
# This column is assigned the result of a logical comparison:
# It checks if the number of distinct values in all columns that start with '2022'
# is equal to 1, indicating that all these columns have the same value for a given row.
# The ungroup() function is then applied to remove the grouping structure
# introduced by rowwise().

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider_diff <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider |>
  rowwise() |>
  mutate(same = n_distinct(unlist(across(
    starts_with('2022'),
    ~ as.character(.x)
  ))) == 1) |>
  ungroup

# vessel_official_nbr 1876
# 2022 Q3              484
# 2022 Q4              356
# 2022 Q2              488
# 2022 Q1              290

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider_diff |>
  select(vessel_official_nbr, same) |>
  count(same)
# 1 FALSE  1421 (incl. NAs)
# 2 TRUE    455

# quantify the # of vessels who fish in both the gulf and S Atl.  ----
ports_path <- file.path(my_paths$outputs,
                        r"(from_db\ports.csv)")
# file.exists(ports_path)
all_ports <-
  readr::read_csv(ports_path)
# Rows: 159 Columns: 15
# print_df_names(all_ports)

get_lat_lon_by_addr <-
  function(input_df, method = "osm") {
    input_data_raw_nominatim <- input_df %>%
      tidygeocoder::geocode(
        method = method,
        street = "ADD_STR1",
        city = "CITY",
        state = "STATE",
        postalcode = "ZIP",
        return_addresses = TRUE,
        full_results = TRUE
      )
    return(input_data_raw_nominatim)
  }

current_project_dir_name_input_dir <-
  file.path(my_paths$inputs,
            current_project_dir_name)
create_dir_if_not(current_project_dir_name_input_dir)

nominatim_rds_file_path <-
  file.path(current_project_dir_name_input_dir,
            "port_addr_to_coords.rds")

ports_nominatim <-
  read_rds_or_run(nominatim_rds_file_path,
                  my_data = all_ports,
                  get_lat_lon_by_addr_nominatim)
# Passing 157 addresses to the Nominatim single address geocoder
# [===================================] 157/157 (100%) Elapsed:  3m Remaining:  0s
# 2023-11-15 run for port_addr_to_coords.rds: 162.14 sec elapsed


plots_nominatim_sf <-
  ports_nominatim |>
  # mutate(year_fct = factor(USER_NYEAR)) |>
    sf::st_as_sf(
      coords = c("long", "lat"),
      crs = 4326,
      na.fail = FALSE
    )

plots_nominatim_sf_short <-
  plots_nominatim_sf |>
  select(PORT_ID, PORT_NUM, STATE, display_name, geometry)

# PORT_ID, PORT_NUM, PORT_NAME, ADD_STR1, ADD_STR2, ADD_STR3, CITY, STATE, ZIP, PHONE1, PHONE2, IS_ACTIVE, LU_USER_CODE, LU, AREA_ID, street, city, state, postalcode, place_id, licence, osm_type, osm_id, class, type, place_rank, importance, addresstype, name, display_name, boundingbox, geometry

plots_nominatim_sf_short |>
  filter(STATE == "FL") |>
  mapview::mapview()
# 99 5

## by census ----
nominatim_rds_file_path_census <-
  file.path(current_project_dir_name_input_dir,
            "port_addr_to_coords_census.rds")

plots_census <-
  get_lat_lon_by_addr(all_ports, 'census')
# Passing 157 addresses to the US Census batch geocoder
# Query completed in: 2.1 seconds
glimpse(plots_census)

plots_census_short <-
  plots_census |>
  select(PORT_ID, PORT_NUM, STATE, matched_address, long, lat)

plots_census_sf <-
  plots_census_short |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326,
               na.fail = FALSE)

plots_census_sf |>
  filter(STATE == "FL") |>
  mapview::mapview()
# [1] 99  5


# install.packages("MazamaLocationUtils")
# add counties to port info
library(MazamaLocationUtils)

# test
# input_data_raw_census |> head(1) |>
#   select(lat, long)
# location_getCensusBlock(
#   longitude = -81.8,
#   latitude = 24.6 ,
#   censusYear = 2020,
#   verbose = TRUE
# )

tic("plots_census_short_c_all")
plots_census_short_c_all <-
  plots_census_short |>
  # filter(STATE == "FL") |>
  filter(!is.na(long) &
           !is.na(lat)) |>
  # head() |>
  rowwise() |>
  mutate(rr =
           location_getCensusBlock(
             longitude = as.double(long),
             latitude = as.double(lat),
             censusYear = 2020,
             verbose = TRUE
           )[[2]]) |>
  ungroup()
toc()

dim(plots_census_short_c)
# $stateCode
# [1] "FL"
#
# $countyName
# [1] "Monroe"
#
# $censusBlock
# [1] "120879721001001"

ports_nominatim_short <-
  ports_nominatim |>
  select(PORT_ID, PORT_NUM, STATE, display_name, long, lat)

dim(ports_nominatim_short)
# [1] 159   6

tic("ports_nominatim_c_all")
ports_nominatim_short_c_all <-
  ports_nominatim_short |>
  filter(!is.na(long) &
           !is.na(lat)) |>
  rowwise() |>
  mutate(county =
           location_getCensusBlock(
             longitude = as.double(long),
             latitude = as.double(lat),
             censusYear = 2020,
             verbose = TRUE
           )[[2]]) |>
  ungroup()
toc()
# ports_nominatim_c_all: 7.44 sec elapsed
dim(ports_nominatim_short_c_all)
# [1] 119   7

ports_nominatim_short_c_all_mapv <-
  ports_nominatim_short_c_all |>
  filter(STATE == "FL" &
           county == "Monroe County") |>
  sf::st_as_sf(coords = c("long", "lat"),
               crs = 4326,
               na.fail = FALSE) |>
  mapview::mapview()

## read in GOM shp ----
## Create a file path using 'file.path' by combining elements from 'my_paths' and specifying a shapefile path.
GOM_400fm_path <-
  file.path(my_paths$inputs,
                      r"(shapefiles\GOM_400fm\GOM_400fm.shp)")

# file.exists(GOM_400fm_path)
# T

## Read a shapefile from the specified file path using 'sf::read_sf'.
## Then, group the resulting data by 'StatZone' and summarize it.
GOMsf_all <-
  sf::read_sf(GOM_400fm_path)

mapview::mapview(GOMsf_all) +
  ports_nominatim_short_c_all_mapv


# ---

dim(all_logbooks_db_data_2022_short_p_region_port_fields_all)
# [1] 3011   11

all_logbooks_db_data_2022_short_p_region_port_fields_all |>
  select(start_port_state) |>
  distinct() |>
  head(2)
# "FL", "DE"

names(state.abb) <- state.name
names(state.name) <- state.abb

# my_state_name[tolower("FL")]
# "Florida"

all_logbooks_db_data_2022_short_p_region_port_states <-
  all_logbooks_db_data_2022_short_p_region_port_fields_all |>
  mutate(
    start_port_state_name = my_state_name[tolower(start_port_state)],
    end_port_state_name   = my_state_name[tolower(end_port_state)]
  ) |>
  mutate(
    start_port_reg =
      case_when(
        tolower(start_port_state_name) %in% tolower(sa_council_states) ~
          "sa_council_state",
        tolower(end_port_state_name) %in% tolower(east_coat_states$gom) ~
          "gom_state",
        .default = "sa_state"
      )
    # diff_reg = case_when(!start_port_state == end_port_state)
  )

glimpse(all_logbooks_db_data_2022_short_p_region_port_states)

# check
# all_logbooks_db_data_2022_short_p_region_port_states |>
#   filter(tolower(start_port_state_name) == "florida") |>
#   select(start_port_county) |>
#   distinct() |>
#   paste(sep = ",\n")


all_logbooks_db_data_2022_short_p_region_port_states_fl_reg <-
  all_logbooks_db_data_2022_short_p_region_port_states |>
  mutate(
    start_port_fl_reg =
      case_when(
        tolower(start_port_state_name) == "florida" &
          tolower(start_port_county) %in% tolower(fl_counties$gom) ~
          "gom_county",
        .default = "sa_county"
      )
  )

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg |>
  filter(start_port_fl_reg == "gom_county") |>
  View()

View(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg)

# look at permit home port vs where they take trip. ----

