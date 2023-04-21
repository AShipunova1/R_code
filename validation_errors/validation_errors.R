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
  dat_pending_date %>%
  select(trip_report_id, arr_year_month) %>%
  group_by(arr_year_month) %>%
  summarise(n = n())

b1 <- by_year_month(dat_pending_date, c("trip_report_id", "arr_year_month"))
identical(db_pending_by_year_month, b1)

View(db_pending_by_year_month)
# A tibble: 17 × 2

# todo: add comments
dat_pending_date_by_ym <-
  dat_pending_date %>%
  select(trip_report_id, overridden, arr_year_month) %>%
  # add_count(trip_report_id, sort = TRUE)
  group_by(overridden, arr_year_month) %>%
  summarise(n = n()) %>%
  # A tibble: 23 × 3
  pivot_wider(names_from = overridden, values_from = n) %>%
  # NAs to 0
  mutate(pending = coalesce(pending, 0)) %>%
  mutate(total = sum(overridden + pending))

View(db_pending_by_year_month)

### Repeat for uassigned only ====
data_overview(dat_pending_date)

dat_pending_data_unas <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01" &
           asg_info == "Unassigned")


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
from_fhier_data_by_ym <-
  from_fhier_data_22 %>%
  select(edit_trip, overridden, arr_year_month) %>%
  mutate(overridden = case_when(overridden == "N" ~ "pending",
                                overridden == "Y" ~ "overridden")) %>%
  group_by(overridden, arr_year_month) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = overridden, values_from = n) %>%   # A tibble: 21 × 3
  # NAs to 0
  mutate(pending = coalesce(pending, 0)) %>%
  mutate(total = sum(overridden + pending))
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

## === Query parameterization ====

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
all.equal(dat_pending_date_by_ym, from_fhier_data_by_ym)
View(dat_pending_date_by_ym)
View(from_fhier_data_by_ym)

both_ym <-
  left_join(dat_pending_date_by_ym,
            from_fhier_data_by_ym,
            by = join_by(arr_year_month))

View(both_ym)

## Diff between DB and FHIER ====

both_ym %>% filter(arr_year_month == "Nov 2022")
# arr_year_month overridden.x pending.x total.x overridden.y pending.y total.y
# 1 Nov 2022               1006         0    1006           78         1      79

View(db_n_fhier_data_ok)
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

View(db_n_fhier_data_ok_short1)

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
filter(asg_info == "Unassigned" & arr_year_month > "Jan 2022") %>% glimpse()
# %>% dim()
# [1] 4824   56

source("~/R_code_github/validation_errors/validation_errors_one_vsl.r")
