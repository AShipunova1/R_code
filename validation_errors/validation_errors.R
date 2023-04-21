library(data.table)

source("~/R_code_github/useful_functions_module.r")
source("~/R_code_github/validation_errors/validation_errors_get_data.r")

## From DB ====
### From db by year ====
by_year <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(arr_year) %>%
    summarise(n = n()) %>%
    return()
}
# TODO: Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
# my_vars <- function() {
  # c(any_of(c("name", "species")), ends_with("color"))
# }
# dplyr::select(starwars, my_vars())

by_year(dat_pending_date, c("trip_report_id", "arr_year"))
# by year
# A tibble: 4 × 2
#   arr_year     n
#   <chr>    <int>
# 1 2021        41
# 2 2022     45376
# 3 2023      2827
# 4 NA         175

dat_pending_date %>%
  select(trip_report_id, overridden, arr_year) %>%
  group_by(overridden, arr_year) %>%
  summarise(n = n())

### From db by year_month ====
by_year_month <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(arr_year_month) %>%
    summarise(n = n()) %>%
    return()
}

db_pending_by_year_month <-
  by_year_month(dat_pending_date, c("trip_report_id", "arr_year_month"))

# View(db_pending_by_year_month)
# A tibble: 17 × 2

by_year_month_wide <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    group_by(overridden, arr_year_month) %>%
    summarise(n = n()) %>%
    # A tibble: 23 × 3
    # ungroup() %>%
    pivot_wider(names_from = overridden, values_from = n) %>%
    # NAs to 0
    mutate(pending = coalesce(pending, 0),
           overridden = coalesce(overridden, 0)) %>%
    arrange(arr_year_month) %>%
    # tail()
    mutate(total = overridden + pending) %>%
    return()
}

dat_pending_date_by_ym <-
  by_year_month_wide(dat_pending_date,
                     c("trip_report_id", "overridden", "arr_year_month"))

# all.equal(dat_pending_date_by_ym, dat_pending_date_by_ym1)
# View(dat_pending_date_by_ym)

### Repeat for Unassigned only ====
data_overview(dat_pending_date)

dat_pending_data_unas <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01" &
           asg_info == "Unassigned")

by_year(dat_pending_data_unas, c("trip_report_id", "arr_year"))

db_unas_by_year_month_wide <-
  by_year_month_wide(dat_pending_data_unas,
                     c("trip_report_id", "overridden", "arr_year_month"))

# tail(db_unas_by_year_month_wide)

### Repeat for Unassigned & System error ====

dat_pending_data_unas_sys_err <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01" &
           (
             asg_info == "Unassigned" |
               startsWith(asg_info, 'System: Error fixed on')
           ))

by_year(dat_pending_data_unas_sys_err,
        c("trip_report_id", "arr_year"))

db_unas_sys_err_by_year_month_wide <-
  by_year_month_wide(
    dat_pending_data_unas_sys_err,
    c("trip_report_id", "overridden", "arr_year_month")
  )

## From FHIER ====
data_overview(from_fhier_data_22)

# dim(from_fhier_data_22)
# [1] 4184   21
# from_fhier_data_22 %>% select(arrival) %>% arrange(arrival) %>% head(2)
# 1 2022-01-01 00:00:00

### FHIER by year ----
# names(from_fhier_data)
from_fhier_data %>%
  select(edit_trip, overridden, arr_year) %>%
  group_by(arr_year) %>%
  summarise(n = n())
# arr_year     n
#   <chr>    <int>
# 1 2021       866
# 2 2022      3536
# 3 2023       295

# todo: add comments
### FHIER by year and month ----

from_fhier_data_by_ym_22 <-
  from_fhier_data_22 %>%
  select(edit_trip, overridden, arr_year_month) %>%
  mutate(overridden = case_when(overridden == "N" ~ "pending",
                                overridden == "Y" ~ "overridden")) %>%
  group_by(overridden, arr_year_month) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = overridden, values_from = n) %>%   # A tibble: 21 × 3
  # NAs to 0
  mutate(pending = coalesce(pending, 0)) %>%
  mutate(total = overridden + pending)
# %>% glimpse()

## Join db and fhier data ----
# make the same types
dat_pending_data <-
  dat_pending_date %>%
  mutate(trip_report_id = as.character(trip_report_id),
         trip_length = as.character(trip_length))

db_n_fhier_data_ok <-
  left_join(
    unique(dat_pending_data),
    unique(from_fhier_data),
    by = join_by(
      trip_report_id == edit_trip,
      res_msg == message,
      trip_length == trip_length,
      arr_year == arr_year,
      arr_year_month == arr_year_month,
      vessel_name == vessel_name
      # ,
      # official_number == vesselofficialnumber
    )
  )

dim(db_n_fhier_data_ok)
# [1] 47569    55

# ---
# dim(unique(dat_pending_data))
# [1] 47569    41
# not unique
# [1] 48440    41
# dim(from_fhier_data_22)
# [1] 4184   21
# dim(unique(from_fhier_data_22))
# [1] 4183   21
# dim(from_fhier_data)
# [1] 5050   21

## Query parameterization ====

make_sql_parameters <- function(my_param_df, sql_text) {
  param_list <- paste0("(",
                       paste0("?parameter", seq_along(my_param_df),
                              collapse = ",\n  "),
                       ")")
  sql_text <- paste0(sql_text_in, param_list)
  cat(sql_text)
  
  sql_params <-
    setNames(as.list(my_param_df), paste0("parameter", seq_along(my_param_df)))
  str(sql_params)
  
  sql <- sqlInterpolate(ANSI(),
                        sql = sql_text,
                        .dots = sql_params)
  
  return(sql)
}

## Compare by year_month ====
all.equal(dat_pending_date_by_ym, from_fhier_data_by_ym_22)
# View(dat_pending_date_by_ym)
# View(from_fhier_data_by_ym_22)

both_ym <-
  left_join(dat_pending_date_by_ym,
            from_fhier_data_by_ym_22,
            by = join_by(arr_year_month))

# View(both_ym)

## Diff between DB and FHIER ====

both_ym %>% filter(arr_year_month == "Nov 2022")
# arr_year_month overridden.x pending.x total.x overridden.y pending.y total.y
# 1 Nov 2022               1006         0    1006           78         1      79

# View(db_n_fhier_data_ok)
dim(db_n_fhier_data_ok)
data_overview(db_n_fhier_data_ok)
# rows 47596
# val_tr_res_id  45961

db_n_fhier_data_ok_short1 <-
  db_n_fhier_data_ok %>%
  select(
    val_param_yr,
    val_param_name,
    res_msg,
    ovr_flag,
    asg_info,
    val_is_ovr,
    is_enabled,
    arr_year_month
  )
dim(db_n_fhier_data_ok_short1)
# [1] 47596    8

db_n_fhier_data_ok_short1 %>%
  # data_overview()
  filter(is_enabled == 1) %>%
  select(asg_info) %>% unique() %>% arrange(asg_info)

### check assignments ----
# grep("ass", names(from_fhier_data_22), value = T)
# singleassignment
from_fhier_data_22 %>%
  select(singleassignment) %>% unique() %>% glimpse()
# Validation trip Assignment

# View(db_n_fhier_data_ok_short1)

db_n_fhier_data_ok_short1 %>%
  mutate(assignment = case_when(
    startsWith(asg_info, 'System: Error fixed on') ~ 'System: Error fixed',
    .default = asg_info
  )) %>%
  select(assignment) %>%
  unique() %>%
  arrange(assignment)
#           assignment
# 1       ALICIA BRETON
# 2          CHRIS ISOM
# 3    KENDALL BRANCART
# 4  LEEANNE DELROSARIO
# 5        SABRINA COBB
# 6     SHANNON STOTLER
# 7 System: Error fixed
# 8          Unassigned


db_n_fhier_data_ok %>%
  filter(asg_info == "Unassigned" &
           arr_year_month > "Jan 2022") %>% glimpse()
# %>% dim()
# [1] 4824   56

source("~/R_code_github/validation_errors/validation_errors_one_vsl.r")

### Repeat for db unassigned ====
db_unas_f_ym <-
  inner_join(db_unas_by_year_month_wide,
             from_fhier_data_by_ym_22,
             by = join_by(arr_year_month))

# names(db_unas_f_ym) %>% paste0(sep = "', '", collapse = "") %>% cat()

setnames(
  db_unas_f_ym,
  old = c(
    'arr_year_month',
    'overridden.x',
    'pending.x',
    'total.x',
    'overridden.y',
    'pending.y',
    'total.y'
  ),
  new = c(
    'arr_year_month',
    'overridden_db',
    'pending_db',
    'total_db',
    'overridden_fhier',
    'pending_fhier',
    'total_fhier'
  )
)
# View(db_unas_f_ym)

### Repeat for db unassigned and "is_enabled" ====
dat_pending_data_unas_en <- dat_pending_data_unas %>%
  filter(is_enabled == 1)

setdiff(dat_pending_data_unas, dat_pending_data_unas_en)
# 1

db_unas_enabled_by_year_month_wide <-
  by_year_month_wide(dat_pending_data_unas_en,
                     c("trip_report_id", "overridden", "arr_year_month"))

# View(db_unas_enabled_by_year_month_wide)

db_unas_en_f_ym <-
  inner_join(
    db_unas_enabled_by_year_month_wide,
    from_fhier_data_by_ym_22,
    by = join_by(arr_year_month)
  ) %>%
  arrange(arr_year_month)

# names(db_unas_f_ym) %>% paste0(sep = "', '", collapse = "") %>% cat()

setnames(
  db_unas_en_f_ym,
  old = c(
    'arr_year_month',
    'overridden.x',
    'pending.x',
    'total.x',
    'overridden.y',
    'pending.y',
    'total.y'
  ),
  new = c(
    'arr_year_month',
    'overridden_db',
    'pending_db',
    'total_db',
    'overridden_fhier',
    'pending_fhier',
    'total_fhier'
  )
)
# View(db_unas_en_f_ym)

### Repeat for Unassigned & System error ====
db_unas_sys_err_f_ym <-
  inner_join(
    db_unas_sys_err_by_year_month_wide,
    from_fhier_data_by_ym_22,
    by = join_by(arr_year_month)
  )

# View(db_unas_sys_err_f_ym)

# TODO: repeat for captain_name

### Split by assignment and validation error ====

#### Split from DB ====
dat_pending_data_22 <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01") %>%
  mutate(trip_report_id = as.character(trip_report_id),
         trip_length = as.character(trip_length))

names(dat_pending_data_22)

glimpse(dat_pending_data_22)

db_by_y_m_asg_param_overr <-
  dat_pending_data_22 %>%
  select(asg_info, overridden, arr_year, arr_year_month, val_param_name) %>%
  group_by(arr_year_month, asg_info, val_param_name, overridden) %>%
  summarise(n = n()) %>%
  ungroup()
# %>%
#   str()
# tibble [3,931 × 5] (S3: tbl_df/tbl/data.frame)

db_by_y_m_param_overr <-
  dat_pending_data_22 %>%
  select(asg_info, overridden, arr_year, arr_year_month, val_param_name) %>%
  group_by(arr_year_month, val_param_name, overridden) %>%
  summarise(n = n()) %>%
  ungroup()
# %>%
# str()
# tibble [254 × 4] (S3: tbl_df/tbl/data.frame)

#### Split from both ====
# View(from_fhier_data_22)

db_n_fhier_data_22_ok <-
  full_join(
    unique(dat_pending_data_22),
    unique(from_fhier_data_22),
    by = join_by(
      trip_report_id == edit_trip,
      res_msg == message,
      trip_length == trip_length,
      arr_year == arr_year,
      arr_year_month == arr_year_month,
      vessel_name == vessel_name,
      official_number == vesselofficialnumber,
      overridden == overridden1
    )
  )

glimpse(db_n_fhier_data_22_ok)
# Rows: 47,724
# Columns: 55

# fields_to_select_list = c("trip_report_id",
#                           "val_param_name",
#                           # "captain_name",
#                           # "asg_info",
#                           "arr_year_month")
# # group_by(across(variables))
# # by_year_month(db_n_fhier_data_22_ok, fields_to_select_list)
# select(all_of(fields_to_select_list)) %>%
#   group_by(arr_year_month) %>%
#   summarise(n = n()) %>%
#   return()
