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
  by_year_month(dat_pending_date, c("trip_report_id", "arr_year_month"))

View(db_pending_by_year_month)
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
View(dat_pending_date_by_ym)

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

### Split by assignment and validation error ====

#### Split from DB ====
dat_pending_data_22_plus <- dat_pending_date %>%
  filter(departure_date >= "2022-01-01" &
           is_enabled == 1)

# names(dat_pending_data_22)

# View(dat_pending_data_22)

fields_to_keep <-
  c(
    "val_param_name",
    "trip_report_id",
    "ovr_flag",
    "vessel_name",
    "official_number",
    "departure_date",
    "trip_start_time",
    "arrival_date",
    "start_date",
    "end_date",
    "val_param_yr",
    "val_is_ovr",
    "val_err_type",
    "program_type",
    "arr_year",
    "arr_year_month",
    "overridden",
    "asg_info"
  )

db_data_22_plus <-
  dat_pending_data_22_plus %>%
  select(all_of(fields_to_keep))

## db_data_22_plus to xls ---- 
# do once
# db_data_22_plus %>%
#   # dim()
#   # [1] 48571    18
#   count(val_param_name, arr_year_month) %>%
#   arrange(arr_year_month) %>%
#   pivot_wider(names_from = arr_year_month, values_from = n) %>%
#   as.data.frame() %>%
#   write.xlsx(
#     file.path(
#       my_paths$inputs,
#       "validation_errors",
#       "validation_errors.xlsx"
#     )
#     ,
#     sheetName = "is_enabled, 2022_",
#     row.names = FALSE,
#     append = TRUE
#   )

  # View()

# ====

db_data_22_plus_overr <-
  db_data_22_plus %>%
  count(arr_year_month, val_param_name, overridden)

names(db_data_22_plus_overr)

db_data_22_plus_overr %>%
  pivot_longer(c(val_param_name, overridden)) %>%
  #   dplyr::group_by(n, name, arr_year_month) %>%
  # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # dplyr::filter(n > 1L)
  pivot_wider(names_from = arr_year_month,
              values_from = value) %>%
    View()


  # pivot_longer(-c(Species,num,ID)) %>%
  # pivot_wider(names_from = ID,values_from=value)

db_data_22_plus_overr_wide <-
  db_data_22_plus_overr %>%
  pivot_wider(names_from = c(arr_year_month, overridden),
              values_from = n) %>%
  as.data.frame()

# run once
# db_data_22_plus_overr_wide %>%#   
#   write.xlsx(
#       file.path(
#         my_paths$inputs,
#         "validation_errors",
#         "db_data_22_plus_overr.xlsx"
#       ),
#       sheetName = "month_overr",
#       row.names = FALSE,
#       append = TRUE
#     )
# 
  # View()

# plots ----
# iris %>% mutate(sumVar = rowSums(.[1:4]))
# str(db_data_22_plus_overr_wide[,2:23])

db_data_22_plus_overr_wide %>% 
  mutate(total_by_param = rowSums(.[2:23], na.rm = TRUE)) %>%
  arrange(desc(total_by_param))
# [1,]
