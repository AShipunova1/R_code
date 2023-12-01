# setup current project ----
library(zoo)
# Determine the path of the executing script
library(this.path)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

misc_info_path <-
  file.path(my_paths$git_r,
            r"(get_data\misc_info.R)")
source(misc_info_path)

all_logb_path <-
  file.path(my_paths$git_r,
            r"(get_data\all_logbooks_db_data_2022_short_p_region_prep.R)")
source(all_logb_path)

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
# my_vars <- function() {
#   c(any_of(c("name", "species")), ends_with("color"))
# }

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
    mutate(all_start_ports = toString(unique(sort(start_port))),
           all_end_ports   = toString(unique(sort(end_port)))) |>
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
        all_start_port_names = toString(unique(sort(start_port_name))),
        all_end_port_names   = toString(unique(sort(end_port_name)))
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

multiple_start_ports <-
  all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  select(vessel_official_nbr,
         start_port_name) |>
  distinct() |>
  add_count(vessel_official_nbr, name = "start_port_name_cnt") |>
  filter(start_port_name_cnt > 1) |>
  arrange(vessel_official_nbr)

dim(multiple_start_ports)
# [1] 1530    3

multiple_end_ports <-
  all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  select(vessel_official_nbr,
         end_port_name) |>
  distinct() |>
  add_count(vessel_official_nbr, name = "end_port_name_cnt") |>
  filter(end_port_name_cnt > 1) |>
  arrange(vessel_official_nbr)

dim(multiple_end_ports)
# [1] 874   3
all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  select(vessel_official_nbr,
         end_port_name) |>
  distinct() |>
  add_count(vessel_official_nbr, name = "start_port_name_cnt") |>
  filter(vessel_official_nbr %in% c('944064',
                                   '934665')) |>
  arrange(vessel_official_nbr) |>
  glimpse()

## different start ports ----
all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  # View()
  filter(all_start_ports_num > 1) |>
  dim()
# [1] 1890    11

## different end ports ----
all_logbooks_db_data_2022_short_p_region_short_all_ports_by_vsl |>
  filter(all_end_ports_num > 1) |>
  View()
# [1] 1890   11


all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  filter(!all_end_ports_num == all_end_port_names_num) |>
  dim()
# 0

data_overview(all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl)
# vessel_official_nbr      1876
# permit_region               2
# start_port_name           531


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

## by quarter, to start - for some seasonality. ----
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
# vessel_official_nbr     1876
# permit_region              2
# start_port               536
# start_port_name          531

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
  select(vessel_official_nbr,
         permit_region,
         trip_start_year_quarter,
         all_start_ports_by_q,
         all_end_ports_by_q) |>
  distinct() |>
  group_by(vessel_official_nbr, permit_region) |>
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

### if a num of lists of ports by quarter grater than 1, then ports are different from Q to Q ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt_w_diff_ports_by_quarter <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt |>
  mutate(same_start_p =
           case_when(count_start_ports_by_q > 1 ~
                       "no",
                     .default = "yes")) |>
  mutate(same_end_p =
           case_when(count_end_ports_by_q > 1 ~
                       "no",
                     .default = "yes")) |>
  arrange(vessel_official_nbr, trip_start_year_quarter)
# filter(permit_region == "gom_and_dual")

# glimpse(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt_w_diff_ports_by_quarter)
# [1] 6604   19
# all
# vessel_official_nbr     1876
# permit_region              2
# start_port               536
# start_port_name          531

# gom
# vessel_official_nbr     498
# permit_region             2
# start_port              364
# start_port_name         360

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short_by_q_cnt_w_diff_ports_by_quarter |>
  select(vessel_official_nbr,
         permit_region,
         same_start_p,
         same_end_p) |>
  distinct() |>
  count(permit_region, same_start_p)
#   permit_region same_start_p     n
#   <chr>         <chr>        <int>
# 1 gom_and_dual  no            1079
# 2 gom_and_dual  yes           2070
# 3 sa_only       no             932
# 4 sa_only       yes           2523

# distinct()
# 1 gom_and_dual  no             182
# 2 gom_and_dual  yes            609
# 3 sa_only       no             191
# 4 sa_only       yes            894

## different locations with a combine table ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3 <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  select(vessel_official_nbr,
         permit_region,
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
    id_cols = c(vessel_official_nbr, permit_region),
    names_from = trip_start_year_quarter,
    values_from = start_port_name,
    values_fn = ~ paste(unique(sort(.x)), collapse = ",")
  )
# glimpse(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider)

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
  mutate(all_start_ports_num =
           n_distinct(unlist(across(
             starts_with('2022')
           ))),
         all_start_ports =
           list(paste(unique(sort(unlist(across(
             starts_with('2022')
           )))),
           sep = ","))) |>
  mutate(same = n_distinct(unlist(across(
    starts_with('2022'),
    ~ as.character(.x)
  ))) == 1) |>
  ungroup()

# View(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider_diff)
# vessel_official_nbr 1876
# 2022 Q3              484
# 2022 Q4              356
# 2022 Q2              488
# 2022 Q1              290

### count same or diff by permit_region ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider_diff |>
  count(permit_region, same)
#   permit_region same      n
#   <chr>         <lgl> <int>
# 1 gom_and_dual  FALSE   569
# 2 gom_and_dual  TRUE    222
# 3 sa_only       FALSE   852
# 4 sa_only       TRUE    233

all_logbooks_db_data_2022_short_p_region_dates_trip_port_short3_wider_diff |>
  select(vessel_official_nbr, same) |>
  count(same)
# 1 FALSE  1421 (incl. NAs)
# 2 TRUE    455

# quantify the # of vessels who fish in both the gulf and S Atl.  ----
## add a gom vs sa marker to ports ----
all_logbooks_db_data_2022_short_p_region_port_region <-
  all_logbooks_db_data_2022_short_p_region |>
  select(
      vessel_id,
      vessel_official_nbr,
      start_port,
      start_port_name,
      start_port_county,
      start_port_state,
      end_port,
      end_port_name,
      end_port_county,
      end_port_state,
      permit_region
    ) |>
  remove_empty_cols() |>
  distinct()

# dim(all_logbooks_db_data_2022_short_p_region_port_region)
# [1] 3011   11

### add full state name ----
all_logbooks_db_data_2022_short_p_region_port_region |>
  select(start_port_state) |>
  distinct() |>
  head(2)
# "FL", "DE"

names(state.abb) <- state.name
names(state.name) <- state.abb

# my_state_name[tolower("FL")]
# "Florida"

all_logbooks_db_data_2022_short_p_region_port_states <-
  all_logbooks_db_data_2022_short_p_region_port_region |>
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

dim(all_logbooks_db_data_2022_short_p_region_port_states)
# [1] 3011   14

### if FL divide by county ----
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
  dim()
# [1] 844  15

glimpse(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg)

### create one_port_marker ----
# if Monroe, FL divide by vessel permit_region
all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start <-
  all_logbooks_db_data_2022_short_p_region_port_states_fl_reg |>
  mutate(
    one_start_port_marker =
      case_when(
        start_port_county == "MONROE" &
          permit_region == "gom_and_dual" ~
          "gom",
        start_port_county == "MONROE" &
          permit_region == "sa_only" ~
          "sa",
        start_port_state == "FL" &
          start_port_fl_reg == "gom_county" ~
          "gom",
        start_port_state == "FL" &
          start_port_fl_reg == "sa_county" ~
          "sa",
        start_port_reg %in% c("gom_council_state",
                              "gom_state") ~
          "gom",
        start_port_reg %in% c("sa_council_state",
                              "sa_state") ~
          "sa",
        .default = NA
      )
  )

dim(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start)
# [1] 3011   16

# check
# all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start |>
#   filter(is.na(one_start_port_marker)) |>
#   # select(start_port_reg) |>
#   # distinct() |>
#   glimpse()
# 0

# all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start |>
#     filter(one_start_port_marker == "gom") |>
#     select(vessel_official_nbr, contains("start")) |>
#     distinct() |>
#     glimpse()

# filter(start_port_county == "MONROE") |>

### count vessels having both one_start_port_markers to find the # of vessels who fish in both the gulf and S Atl.  ----
#### rm start_port_reg and start_port_fl_reg ----

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short <-
  all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start |>
  select(
    vessel_id,
    vessel_official_nbr,
    start_port,
    start_port_name,
    start_port_county,
    start_port_state,
    end_port,
    end_port_name,
    end_port_county,
    end_port_state,
    permit_region,
    start_port_state_name,
    end_port_state_name,
    one_start_port_marker
  ) |>
  distinct()

# data_overview(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short)
# [1] 3011   14
# vessel_official_nbr   1876
# start_port             536
# start_port_name        531


all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short |>
  select(vessel_official_nbr, one_start_port_marker) |>
  distinct() |>
  count(one_start_port_marker)
# 1                   gom 1000
# 2                    sa 1029

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short |>
  select(vessel_official_nbr,
         permit_region,
         one_start_port_marker) |>
  distinct() |>
  count(permit_region, one_start_port_marker)
# 1  gom_and_dual                   gom  784
# 2  gom_and_dual                    sa   16
# 3       sa_only                   gom  216
# 4       sa_only                    sa 1013

find_multi_region_vessels <-
  function(my_df,
           select_vector,
           group_by_vector = c("vessel_official_nbr")) {
    my_df |>
      select(all_of(select_vector)) |>
      distinct() |>
      group_by_at(group_by_vector) |>
      mutate(
        vessel_one_start_port_marker =
          toString(unique(sort(
            one_start_port_marker
          ))),
        vessel_one_start_port_marker_num =
          length(str_split(vessel_one_start_port_marker, ","))
      ) |>
      ungroup() %>%
      return()
  }

select_vessel_mark_only <-
  c("vessel_official_nbr",
    "one_start_port_marker")

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt <-
  find_multi_region_vessels(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short, select_vessel_mark_only)

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt_short <-
  all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt |>
  select(
    vessel_official_nbr,
    vessel_one_start_port_marker_num
  ) |>
  distinct()

# vessel_official_nbr              1876
all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt_short |>
  count(multi_start = vessel_one_start_port_marker_num > 1)
#   multi_start     n
#   <lgl>       <int>
# 1 FALSE        1723
# 2 TRUE          153

# 1723 + 153
# 1876

### count multi start by vessel permit ----
all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt_p <-
  find_multi_region_vessels(
    all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short,
    select_vector = c(select_vessel_mark_only, "permit_region"),
    group_by_vector = c("vessel_official_nbr", "permit_region")
  )

all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short_cnt_p |>
  # glimpse()
  select(
    vessel_official_nbr,
    permit_region,
    vessel_one_start_port_marker_num
  ) |>
  distinct() |>
# vessel_official_nbr              1876
  count(permit_region,
        multi_start = vessel_one_start_port_marker_num > 1)
#   count(wt = n)
# 1  1876

#   permit_region multi_start     n
#   <chr>         <lgl>       <int>
# 1 gom_and_dual  FALSE         782
# 2 gom_and_dual  TRUE            9
# 3 sa_only       FALSE         941
# 4 sa_only       TRUE          144

# look at permit home port vs where they take trip ----
## prepare home_port data ----
# all_get_db_data_result_l |>
#   print_df_names()

# all_get_db_data_result_l$vessels_permits |>
  # print_df_names()

vessel_permit_port_info <-
  all_get_db_data_result_l$vessels_permits |>
  # active permits in 2022
  filter(
    LAST_EXPIRATION_DATE > "2022-12-31" |
      END_DATE > "2022-12-31" |
      EXPIRATION_DATE > "2022-12-31"
  )

# [1] 16143     8
# SERO_OFFICIAL_NUMBER  6762
# PORT_CODE              158
# SERO_HOME_PORT_CITY    941
# with exp_date:
# [1] 12238     8
# SERO_OFFICIAL_NUMBER  5220
# SERO_HOME_PORT_CITY    809


### add permit and vessel info ----
# should do here, before the join, bc if there are empty rows after merge sa_only is wrongly assigned

vessel_permit_port_info_perm_reg <-
  vessel_permit_port_info |>
  group_by(VESSEL_VESSEL_ID) |>
  mutate(all_permits = toString(unique(sort(TOP)))) |>
  separate_permits_into_3_groups(permit_group_field_name = "all_permits") |>
    select(
    PERMIT_VESSEL_ID,
    VESSEL_VESSEL_ID,
    # PORT_CODE, mostly empty
    permit_sa_gom,
    SERO_HOME_PORT_CITY,
    SERO_HOME_PORT_COUNTY,
    SERO_HOME_PORT_STATE,
    SERO_OFFICIAL_NUMBER
  ) |>
  ungroup() |>
  remove_empty_cols() |>
  distinct()

# data_overview(vessel_permit_port_info_perm_reg)
# [1] 5220    7
# VESSEL_VESSEL_ID      5220

# vessel_permit_port_info_perm_reg |>
#   filter(permit_sa_gom == "sa_only") |>
#   distinct() |>
#   View()

# print_df_names(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start)

join_vessel_and_trip <-
  left_join(
    all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short,
    vessel_permit_port_info_perm_reg,
    join_by(vessel_id == VESSEL_VESSEL_ID)
  )

dim(join_vessel_and_trip)
# [1] 3011   20

# vessel_id             1876
# vessel_official_nbr   1876
# permit_sa_gom            4
# SERO_OFFICIAL_NUMBER  1785

dim(all_logbooks_db_data_2022_short_p_region_port_states_fl_reg_start_short)
# [1] 3011   14

### check permit_regions ----
join_vessel_and_trip |>
  filter(!permit_region == permit_sa_gom) |>
  select(permit_region, permit_sa_gom) |>
  distinct() |>
  arrange(permit_region)
#   permit_region permit_sa_gom
# 1  gom_and_dual          dual
# 2  gom_and_dual      gom_only

# 3  gom_and_dual       sa_only
# 4       sa_only          dual
# 5       sa_only      gom_only

# TODO: compare regions, why diff
join_vessel_and_trip |>
  filter(permit_region == "gom_and_dual" &
           permit_sa_gom == "sa_only") |>
  glimpse()
# 6

join_vessel_and_trip |>
  filter(permit_region == "sa_only" &
           permit_sa_gom == "dual") |>
  dim()
# 40

join_vessel_and_trip |>
  filter(permit_region == "sa_only" &
           permit_sa_gom == "gom_only") |>
  glimpse()
# FL1921PM
# PIMS:
# Home port
# PENSACOLA, FL
# dual

# vessel_id, vessel_official_nbr, start_port, start_port_name, start_port_county, start_port_state, end_port, end_port_name, end_port_county, end_port_state, permit_region, start_port_state_name, end_port_state_name, one_start_port_marker, PERMIT_VESSEL_ID, permit_sa_gom, SERO_HOME_PORT_CITY, SERO_HOME_PORT_COUNTY, SERO_HOME_PORT_STATE, SERO_OFFICIAL_NUMBER

tic("join_vessel_and_trip_port_diff")
join_vessel_and_trip_port_diff <-
  join_vessel_and_trip |>
  group_by(vessel_official_nbr) |>
  mutate(
    diff_start_port_state =
      case_when(
        !tolower(start_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_county =
      case_when(
        !tolower(start_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_start_port_name_or_city =
      case_when(
        !tolower(start_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  mutate(
    diff_end_port_state =
      case_when(
        !tolower(end_port_state) == tolower(SERO_HOME_PORT_STATE) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_county =
      case_when(
        !tolower(end_port_county) == tolower(SERO_HOME_PORT_COUNTY) ~
          "yes",
        .default = "no"
      ),
    diff_end_port_name_or_city =
      case_when(
        !tolower(end_port_name) == tolower(SERO_HOME_PORT_CITY) ~
          "yes",
        .default = "no"
      )
  ) |>
  ungroup()
toc()
# join_vessel_and_trip_port_diff: 1.74 sec elapsed

join_vessel_and_trip_port_diff |>
  select(vessel_official_nbr,
         starts_with("diff")) |>
  distinct() |>
  dim()
# [1] 2591    7


join_vessel_and_trip_port_diff_short <-
  join_vessel_and_trip_port_diff |>
  select(vessel_official_nbr,
         permit_region,
         starts_with("diff")) |>
  distinct()

join_vessel_and_trip_port_diff_short |>
  count(diff_start_port_state)
# 1 no                     2501
# 2 yes                      90

my_col_names <- names(join_vessel_and_trip_port_diff_short)

combs1 <-
  combn(my_col_names, 2) |>
  as.data.frame()

str(combs1)
combs1_short <-
  combs1[1:ncol(join_vessel_and_trip_port_diff_short) - 1]

combs1_short_cnts <-
  combs1_short |>
  map(\(curr_col_names) {
    # browser()
    join_vessel_and_trip_port_diff_short |>
      select(paste(curr_col_names, sep = ",")) |>
      count(!!sym(curr_col_names[[2]]))
  })

combs1_short_cnts

## the same with permit region ----
join_vessel_and_trip_port_diff_short_perm <-
  join_vessel_and_trip_port_diff |>
  select(vessel_official_nbr,
         permit_region,
         starts_with("diff")) |>
  distinct()

join_vessel_and_trip_port_diff_short_perm |>
  count(diff_start_port_state)
# 1 no                     2501
# 2 yes                      90

my_col_names <- names(join_vessel_and_trip_port_diff_short_perm)

combs2 <-
  combn(my_col_names, 3) |>
  as.data.frame()

dif_cols_num <-
  grep("diff", my_col_names) |>  length()

combs2_short <-
  combs2[1:dif_cols_num]

# View(combs2_short)

combs2_short_cnts <-
  combs2_short |>

  # Use 'map' to apply a function to each element of 'combs2_short'
  map(\(curr_col_names) {

    # Use 'join_vessel_and_trip_port_diff_short' as the data source
    join_vessel_and_trip_port_diff_short |>

      # Select columns specified by 'curr_col_names' and separate them with ","
      select(paste(curr_col_names, sep = ",")) |>

      # Count occurrences of unique combinations of the second and third columns
      count(!!sym(curr_col_names[[2]]),
            !!sym(curr_col_names[[3]]))
  })


combs2_short_cnts

# how many vessels used different landing locations (simple count per vessel) ----
# how many of those cross states ----
