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
# one port in one q and diff in another
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

# m4_daily %>%
#     group_by(id) %>%
#     summarise_by_time(
#         .date_var = date,
#         .by       = "month", # Setup for monthly aggregation
#         # Summarization
#         value  = first(value)
#     )

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  dplyr::group_by(vessel_id,
                  vessel_official_nbr,
                  start_port,
                  trip_start_quarter_num) |>
  dplyr::arrange(trip_start_quarter_num,
                 .by_group = TRUE) %>%
  dplyr::summarize(cnt_start_port_qu = n()) |>
  dplyr::filter(cnt_start_port_qu > 1) |>
  dplyr::arrange(vessel_official_nbr) |>
  dplyr::ungroup() |>
  glimpse()

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  filter(vessel_official_nbr == "1055255") |>
  head(2)

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  dplyr::group_by(vessel_id,
                  vessel_official_nbr,
                  start_port,
                  trip_start_quarter_num) |>
  dplyr::arrange(trip_start_quarter_num,
                 .by_group = TRUE) %>%
  dplyr::mutate(cnt_start_port_qu = n()) |>
  dplyr::filter(cnt_start_port_qu > 1) |>
  dplyr::arrange(vessel_official_nbr, trip_start_quarter_num) |>
  dplyr::ungroup() |>
  filter(vessel_official_nbr == "1055255") |>
  View()



all_logbooks_db_data_2022_short_p_region_dates_trip_port_mult_port <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  add_all_port_string() |>
  add_all_port_name_string() |>
  filter(permit_region == "gom_and_dual" &
           all_end_ports_num > 1)

dim(all_logbooks_db_data_2022_short_p_region_dates_trip_port_mult_port)
# [1] 3078   21

# all_logbooks_db_data_2022_short_p_region_dates_trip_port_mult_port |>
# count(start_port_name, trip_start_year_quarter) |>
#   distinct() |>
#   head()
# 1 AAND B MARINA   2022 Q1                     1
# 2 AAND B MARINA   2022 Q2                     1
# 3 AAND B MARINA   2022 Q3                     1
# 4 AAND B MARINA   2022 Q4                     2
# 5 APALACHICOLA    2022 Q2                     2
# 6 APALACHICOLA    2022 Q3                     2

group_by_vector <- c("vessel_id")

all_logbooks_db_data_2022_short_p_region_dates_trip_port_mult_port |>
  group_by_at(group_by_vector) |>
  mutate(
    start_port_names_by_q = toString(unique(start_port_name)),
    end_port_names_by_q   = toString(unique(end_port_name))
  ) |>
  ungroup() |>
  View()


# quantify the # of vessels who fish in both the gulf and S Atl. ;
all_logbooks_db_data_2022_short_p_region_port <-
  all_logbooks_db_data_2022_short_p_region |>
  select(vessel_id,
         vessel_official_nbr,
         permit_region,
         contains("port"),
         -starts_with("notif")) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_port)
# [1] 3579   19
# [1] 3011   11 -starts_with("notif")

all_logbooks_db_data_2022_short_p_region_port |>
  select(start_port_state) |>
  distinct() |>
  head(2)

names(state.abb) <- state.name
names(state.name) <- state.abb

# my_state_name[tolower("FL")]
# "Florida"

all_logbooks_db_data_2022_short_p_region_port_states <-
  all_logbooks_db_data_2022_short_p_region_port |>
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

all_logbooks_db_data_2022_short_p_region_port_states |>
  filter(tolower(start_port_state_name) == "florida") |>
  select(start_port_county) |>
  distinct() |>
  paste(sep = ",\n")


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
  glimpse()



# look at permit home port vs where they take trip. ----

