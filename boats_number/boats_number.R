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
      format(trip_start_year_quarter, "%q"),
    trip_end_year_quarter = as.yearqtr(trip_end_date),
    trip_end_quarter_num =
      format(trip_end_year_quarter, "%q")
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
# [1] 94366    29

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

## multiple_start_ports ----
multiple_start_ports <-
  all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  select(vessel_official_nbr,
         permit_region,
         start_port_name) |>
  distinct() |>
  add_count(vessel_official_nbr,
            permit_region,
            name = "start_port_name_cnt") |>
  filter(start_port_name_cnt > 1) |>
  arrange(vessel_official_nbr)

count_uniq_by_column(multiple_start_ports)
# vessel_official_nbr 675

### test multiple_start_ports ----
multiple_start_ports |>
  filter(vessel_official_nbr %in% c('944064',
                                    '934665')) |>
  glimpse()
# 2,2

## multiple_end_ports ----
multiple_end_ports <-
  all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
  select(vessel_official_nbr,
         permit_region,
         end_port_name) |>
  distinct() |>
  add_count(vessel_official_nbr,
            permit_region,
            name = "end_port_name_cnt") |>
  filter(end_port_name_cnt > 1) |>
  arrange(vessel_official_nbr)

### test multiple_end_ports ----
multiple_end_ports |>
  filter(vessel_official_nbr %in% c('944064',
                                    '934665')) |>
  glimpse()
# 2

count_uniq_by_column(multiple_end_ports)
# vessel_official_nbr 374

## multiple_end_port_states ----
# View(all_logbooks_db_data_2022_short_p_region_port_fields_all)
multiple_end_port_states <-
  all_logbooks_db_data_2022_short_p_region_port_fields_all |>
  select(vessel_official_nbr,
         permit_region,
         end_port_state) |>
  distinct() |>
  add_count(vessel_official_nbr,
            permit_region,
            name = "end_port_state_cnt") |>
  filter(end_port_state_cnt > 1) |>
  arrange(vessel_official_nbr)

multiple_end_port_states |>
  count_uniq_by_column()
# vessel_official_nbr 76

## by quarter ----
#
# all_logbooks_db_data_2022_short_p_region_short_all_port_names_by_vsl |>
#   filter(vessel_official_nbr == 1000042) |>
#   glimpse()

### trips and quarter fields only ----
all_logbooks_db_data_2022_short_p_region_dates_trip_port_short <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port |>
  select(
    port_fields_all(),
    -starts_with("notif"),
    c(trip_start_year_quarter,
      trip_start_quarter_num,
      trip_end_year_quarter,
      trip_end_quarter_num,)
  ) |>
  remove_empty_cols() |>
  distinct()

dim(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short)
# [1] 6639   15

count_uniq_by_column(all_logbooks_db_data_2022_short_p_region_dates_trip_port_short)
# vessel_official_nbr     1876
# permit_region              2
# start_port_name          531
# end_port_name            529

### a quarter is the same, a port - diff ----
# test
# all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
#   filter(vessel_official_nbr == "1057052") |>
#   arrange(trip_start_quarter_num) |>
#   glimpse()

start_ports_q_short <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  select(vessel_official_nbr,
         permit_region,
         trip_start_year_quarter,
         start_port_name) |>
  distinct()

end_ports_q_short <-
  all_logbooks_db_data_2022_short_p_region_dates_trip_port_short |>
  select(vessel_official_nbr,
         permit_region,
         trip_end_year_quarter,
         end_port_name) |>
  distinct()

dim(start_ports_q_short)
# [1] 6317    4
dim(end_ports_q_short)
# [1] 5845    4

count_uniq_by_column(start_ports_q_short)
# vessel_official_nbr     1876
# start_port_name          531

count_uniq_by_column(end_ports_q_short)
# end_port_name          529

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

each_quarter_a_col <-
  function(my_df,
           start_or_end = "start") {

    quarter_field_name <-
      stringr::str_glue("trip_{start_or_end}_year_quarter")
    port_field_name <-
      stringr::str_glue("{start_or_end}_port_name")

    ports_q_short_wider <-
      my_df |>
      pivot_wider(
        id_cols = c(vessel_official_nbr, permit_region),
        names_from = !!sym(quarter_field_name),
        values_from = !!sym(port_field_name),
        values_fn = ~ paste(unique(sort(.x)), collapse = ",")
      )

    return(ports_q_short_wider)

  }

ports_q_short_wider_list <-
  list(c(start_ports_q_short, "start"),
           c(end_ports_q_short, "end")) |>
  map(\(one_df_l) {
    # browser()
    my_df <- one_df_l[1:length(one_df_l) - 1] |>
      as.data.frame()
    my_col_name <- one_df_l[[length(one_df_l)]]
    each_quarter_a_col(my_df, my_col_name)
  })

# glimpse(ports_q_short_wider_list)

# start_ports_q_short_wider <-
#   start_ports_q_short |>
#   pivot_wider(
#     id_cols = c(vessel_official_nbr, permit_region),
#     names_from = trip_start_year_quarter,
#     values_from = start_port_name,
#     values_fn = ~ paste(unique(sort(.x)), collapse = ",")
#   )

### add column for the same or diff ----
# It starts by using the rowwise() function to apply subsequent operations
# to each row individually, ensuring that calculations are row-wise.

# The mutate() function is then used to create a new column 'same'.
# This column is assigned the result of a logical comparison:
# It checks if the number of distinct values in all columns that start with '2022'
# is equal to 1, indicating that all these columns have the same value for a given row.
# The ungroup() function is then applied to remove the grouping structure
# introduced by rowwise().

make_ports_q_short_wider_diff <-
  function(my_df,
           start_or_end = "start") {

    ports_num_field_name <-
      stringr::str_glue("all_{start_or_end}_ports_num")

    ports_field_name <-
      stringr::str_glue("all_{start_or_end}_ports")

    ports_q_short_wider_diff <-
      my_df |>
      rowwise() |>
      mutate(!!ports_num_field_name :=
               n_distinct(unlist(across(
                 starts_with('2022')
               ))),
             !!ports_field_name :=
               list(paste(unique(sort(
                 unlist(across(starts_with('2022')))
               )),
               sep = ","))) |>
      mutate(same = n_distinct(unlist(across(
        starts_with('2022'),
        ~ as.character(.x)
      ))) == 1) |>
      ungroup()

    return(ports_q_short_wider_diff)
  }

# aa <- make_ports_q_short_wider_diff(start_ports_q_short_wider)
diffdf::diffdf(start_ports_q_short_wider_diff, ports_q_short_wider_list_diff[[1]])
# View(aa)

# View(ports_q_short_wider_list)
tic("ports_q_short_wider_list_diff")
ports_q_short_wider_list_diff <-
  list(c(ports_q_short_wider_list[[1]], "start"),
       c(ports_q_short_wider_list[[2]], "end")) |>
  map(\(one_df_l) {
    # browser()
    my_df_names <-
      names(one_df_l)[1:length(one_df_l) - 1]
    my_df <- one_df_l[1:length(one_df_l) - 1] |>
      as.data.frame()
    names(my_df) <- my_df_names
    my_col_name <- one_df_l[[length(one_df_l)]]
    make_ports_q_short_wider_diff(my_df, my_col_name)
  })
toc()
# ports_q_short_wider_list_diff: 9.93 sec elapsed

# View(ports_q_short_wider_list_diff)

ports_q_short_wider_list <-
  list(c(start_ports_q_short_wider, "start"),
       c(end_ports_q_short_wider, "end")) |>
  map(\(one_df_l) {
    my_df <- one_df_l[1:length(one_df_l) - 1] |>
      as.data.frame()
    my_col_name <- one_df_l[[length(one_df_l)]]
    make_ports_q_short_wider_diff(my_df, my_col_name)

  })

# ---
start_ports_q_short_wider_diff <-
  start_ports_q_short_wider |>
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

start_ports_q_short_wider_diff |>
  filter(vessel_official_nbr == "1171256") |>
  glimpse()
# $ all_start_ports     <list> <"BARNEGAT,MORRISON'S MARINA AND  SHIPS STORE", "M…
# $ same                <lgl> FALSE

count_uniq_by_column(start_ports_q_short_wider_diff)
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
