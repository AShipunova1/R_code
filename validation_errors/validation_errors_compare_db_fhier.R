library(data.table)
# install.packages("xlsx")
library(xlsx)
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
source("~/R_code_github/validation_errors/validation_errors_get_data.r")

## From DB ====
### From db by year ====
by_year <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    dplyr::group_by(arr_year) %>%
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
  dplyr::group_by(overridden, arr_year) %>%
  summarise(n = n())

### From db by year_month ====
by_year_month <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    dplyr::group_by(arr_year_month) %>%
    summarise(n = n()) %>%
    return()
}

db_pending_by_year_month <-
  by_year_month(dat_pending_date, c("trip_report_id", "arr_year_month"))

View(db_pending_by_year_month)
# A tibble: 17 × 2

by_year_month_wide <- function(my_df, fields_to_select_list) {
  my_df %>%
    select(all_of(fields_to_select_list)) %>%
    dplyr::group_by(overridden, arr_year_month) %>%
    summarise(n = n()) %>%
    # A tibble: 23 × 3
    # dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = overridden, values_from = n) %>%
    # NAs to 0
    mutate(pending = coalesce(pending, 0),
           overridden = coalesce(overridden, 0)) %>%
    dplyr::arrange(arr_year_month) %>%
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
# from_fhier_data_22 %>% select(arrival) %>% dplyr::arrange(arrival) %>% head(2)
# 1 2022-01-01 00:00:00

### FHIER by year ----
# names(from_fhier_data)
from_fhier_data %>%
  select(edit_trip, overridden, arr_year) %>%
  dplyr::group_by(arr_year) %>%
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
  dplyr::group_by(overridden, arr_year_month) %>%
  summarise(n = n()) %>%
  tidyr::pivot_wider(names_from = overridden, values_from = n) %>%   # A tibble: 21 × 3
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
# F
# View(dat_pending_date_by_ym)
# View(from_fhier_data_by_ym_22)

both_ym <-
  left_join(dat_pending_date_by_ym,
            from_fhier_data_by_ym_22,
            by = join_by(arr_year_month))

# View(both_ym)

## Diff between DB and FHIER ====
### for one month
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
  select(asg_info) %>% unique() %>% dplyr::arrange(asg_info)

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
  dplyr::arrange(assignment)
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
           arr_year_month >= "Jan 2022") %>% glimpse()
# %>% dim()
# [1] 4962   56

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
  dplyr::arrange(arr_year_month)

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

View(db_unas_sys_err_f_ym)

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
  dplyr::group_by(arr_year_month, asg_info, val_param_name, overridden) %>%
  summarise(n = n()) %>%
  dplyr::ungroup()
# %>%
#   str()
# tibble [3,931 × 5] (S3: tbl_df/tbl/data.frame)

db_by_y_m_param_overr <-
  dat_pending_data_22 %>%
  select(asg_info, overridden, arr_year, arr_year_month, val_param_name) %>%
  dplyr::group_by(arr_year_month, val_param_name, overridden) %>%
  summarise(n = n()) %>%
  dplyr::ungroup()
# %>%
# str()
# tibble [254 × 4] (S3: tbl_df/tbl/data.frame)

#### Split from both ====
#### Full join with a "data_from" column ====

# View(from_fhier_data_22)

dat_pending_data_22_s <-
  dat_pending_data_22 %>%
  mutate(data_from = "db")

from_fhier_data_22_s <-
  from_fhier_data_22 %>%
  mutate(data_from = "fhier")

db_n_fhier_data_22_ok <-
  full_join(
    unique(dat_pending_data_22_s),
    unique(from_fhier_data_22_s),
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

# Rows: 47,924
# Columns: 55

fields_to_select_list3 = (
  c(
    "val_param_name",
    "overridden",
    "overridden.y",
    "overrideuser",
    # "captain_name",
    "asg_info",
    "data_from.x",
    "data_from.y",
    "arr_year_month"
  )
)

# names(db_n_fhier_data_22_ok) %>% as.data.frame() %>% View()
# grep("overr", names(db_n_fhier_data_22_ok), value = T)

#### full_join with counts ====
db_n_fhier_data_22_ok_cnts <-
  db_n_fhier_data_22_ok %>%
  select(trip_report_id, all_of(fields_to_select_list3)) %>%
  dplyr::arrange(arr_year_month) %>%
  dplyr::group_by(across(all_of(fields_to_select_list3))) %>%
  summarise(n = n()) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(arr_year_month)

#### In FHIER too ====
# filter(db_n_fhier_data_22_ok_cnts, !is.na(overrideuser)) %>% View()
View(db_n_fhier_data_22_ok_cnts)
# tail()

param_in_both <-
  db_n_fhier_data_22_ok_cnts %>%
  filter(!is.na(data_from.x) &
           !is.na(data_from.y)) %>%
  # View()
  select(val_param_name) %>% unique()
# 12

param_in_db_only <-
  db_n_fhier_data_22_ok_cnts %>%
  filter(!is.na(data_from.x) &
           is.na(data_from.y)) %>%
  # View()
  select(val_param_name) %>% unique()
# 16

param_in_fh_only <-
  db_n_fhier_data_22_ok_cnts %>%
  filter(is.na(data_from.x) &
           !is.na(data_from.y)) %>%
  # View()
  select(val_param_name) %>% unique()
# 1 NA

setdiff(param_in_db_only, param_in_both)
# 1 Unusual End Port
# 2 Warning for Depth information = 0
# 3 Invalid Start and/or End Date
# 4 Unusual Gear for Vessel    

names(from_fhier_data_22_s)
from_fhier_data_22_s %>%
  # select(res_message, message) %>% head()
  select(message) %>% unique() %>%
  filter(
    # grepl("nusual", message, ignore.case = TRUE) &
# 161
  # grepl("port", message, ignore.case = TRUE)
# 0
grepl("Invalid", message, ignore.case = TRUE)
# 0
  ) %>% str()

### repeat diff with fewer param names ====
# 1 Unusual End Port
# 2 Warning for Depth information = 0
# 3 Invalid Start and/or End Date
# 4 Unusual Gear for Vessel    

dat_pending_data_unas_param <- dat_pending_data_22_s %>%
  filter(
    asg_info == "Unassigned" &
      !val_param_name %in% c(
        "Unusual End Port",
        "Warning for Depth information = 0",
        "Invalid Start and/or End Date",
        "Unusual Gear for Vessel"
      )
  )

by_year(dat_pending_data_unas_param,
        c("trip_report_id", "arr_year"))

db_unas_param_err_by_year_month_wide <-
  by_year_month_wide(
    dat_pending_data_unas_param,
    c("trip_report_id", "overridden", "arr_year_month")
  )

db_unas_param_f_ym <-
  inner_join(db_unas_param_err_by_year_month_wide,
             from_fhier_data_by_ym_22,
             by = join_by(arr_year_month))

setnames(
  db_unas_param_f_ym,
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
View(db_unas_param_f_ym)

### Apr 2022 ====
apr_unas_both <-
  db_n_fhier_data_22_ok_cnts %>%
  filter(arr_year_month == "Apr 2022" &
           asg_info == "Unassigned")
# View(apr_unas_both)

write.xlsx(
  as.data.frame(apr_unas_both),
  file.path(
    my_paths$inputs,
    "validation_errors",
    "apr22_db_n_fhier_data_22_ok_cnts.xlsx"),
    sheetName = "apr_Unassigned",
    row.names = FALSE,
    append = TRUE
  )

db_apr_22 <- dat_pending_date %>%
  filter(arr_year_month == "Apr 2022")

# write_csv(
#   db_apr_22,
#   file.path(
#     my_paths$outputs,
#     "validation_errors",
#     "db_apr_22.csv"
#     # ,
#     # sheetName = "db_apr_22"
#   ))

write.xlsx(
  as.data.frame(db_apr_22),
  file.path(
    my_paths$inputs,
    "validation_errors",
    "apr22_db_n_fhier_data_22_ok_cnts.xlsx"),
    sheetName = "db_apr_22",
    row.names = FALSE,
    append = TRUE
  )

fh_apr_22 <- from_fhier_data_22 %>%
  filter(arr_year_month == "Apr 2022")

# write_csv(
#   fh_apr_22,
#   file.path(
#     my_paths$outputs,
#     "validation_errors",
#     "fh_apr_22.csv"
#     # ,
#     # sheetName = "db_apr_22"
#   ))

write.xlsx(
  as.data.frame(fh_apr_22),
  file.path(
    my_paths$inputs,
    "validation_errors",
    "apr22_db_n_fhier_data_22_ok_cnts.xlsx"),
    sheetName = "fh_apr_22",
    row.names = FALSE,
    append = TRUE
  )

# === db
# asg_info
# trip_report_id
# val_param_name
# grep("enab", names(dat_pending_date), value = T)

dat_pending_date %>%
  filter(arr_year_month >= "Jan 2022" &
           is_enabled == 1) %>%
  select(trip_report_id,
         val_param_name,
         asg_info,
         overridden,
         arr_year_month) %>%
  write_csv(file.path(my_paths$outputs,
                      "validation_errors",
                      "db_22.csv"))

# fhier
from_fhier_data_22 %>%
  filter(arr_year_month >= "Jan 2022") %>%
  mutate(message_no_d = str_replace_all(message, "\\d+", "")) %>%
  mutate(message_no_d = str_replace_all(message_no_d, "-", "")) %>%
  mutate(
    message_no_d = str_replace_all(
      message_no_d,
      "Start time inconsistency with trip start time.+",
      "Start time inconsistency with trip start time"
    )
  ) %>%
  # select(message_no_d) %>% unique() %>% View()
  mutate(message_no_d = str_replace(message_no_d, "Trip.+no catch", "Trips with no catch")) %>%
  select(edit_trip, message_no_d, overridden1, arr_year_month) %>%
  write_csv(file.path(my_paths$outputs,
                      "validation_errors",
                      "fh_22.csv"))
