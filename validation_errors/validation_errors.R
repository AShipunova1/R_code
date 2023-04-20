source("~/R_code_github/useful_functions_module.r")
source("~/R_code_github/validation_errors_get_data.r")

## From DB ====
### From db by year ====
dat_pending_date %>%
  select(trip_report_id, arr_year) %>%
  group_by(arr_year) %>%
  summarise(n = n())
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
db_pending_by_year_month <-
  dat_pending_date %>%
  select(trip_report_id, arr_year_month) %>%
  group_by(arr_year_month) %>%
  summarise(n = n())

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

### test one vessel ----
data_overview(dat_pending_date)
dat_pending_date$val_param_yr %>% unique()
# [1] 2021

dat_pending_date_od_all <-
  dat_pending_date %>%
  filter(official_number == 'FL4673SY') %>%
  arrange(arrival_date)
# 73

dat_pending_date_od_all %>%
  select(arr_year, is_enabled) %>%
  # unique()
  #   2022          1
  #   2023          1
  group_by(arr_year) %>%
  summarise(n = n())
# 1 2022        78
# 2 2023        13

dat_pending_date_od_apr <-
  dat_pending_date %>%
  filter(official_number == 'FL4673SY',
         arrival_date > '2023-04-04') %>%
  arrange(arrival_date)
# %>%

# write_csv(dat_pending_date_od_apr, "~/dat_pending_date_od_apr.csv")

## From FHIER ====

data_overview(from_fhier_data_22)

# dim(from_fhier_data_22)
# [1] 4184   21
# from_fhier_data_22 %>% select(arrival) %>% arrange(arrival) %>% head(2)
# 1 2022-01-01 00:00:00

# ==== combine db and fhier ===
dat_pending_data <-
  dat_pending_date %>%
  mutate(trip_report_id = as.character(trip_report_id),
         trip_length = as.character(trip_length))
#
# db_n_fhier_data_0 <-
#   left_join(dat_pending_data, from_fhier_data,
#           by = join_by(trip_report_id == edit_trip))
# grep("trip", names(dat_pending_data), value = T)
# grep("trip", names(from_fhier_data), value = T)
# #   Detected an unexpected many-to-many relationship between `x` and `y`.
# # ℹ Row 44 of `x` matches multiple rows in `y`.
# db_mult <- dat_pending_data[44,]  %>% select(trip_report_id)
# # ℹ Row 4736 of `y` matches multiple rows in `x`.
# fh_mult <- from_fhier_data[4736,] %>% select(edit_trip)
# # 65334545
#
# identical(fh_mult$edit_trip, db_mult$trip_report_id)
# # TRUE
# fh_mult1 <- from_fhier_data %>%
#   filter(edit_trip == db_mult$trip_report_id)
#
# View(fh_mult1)
#
# all.equal(fh_mult1[1,], fh_mult1[2,])
# # [1] "Component “res_message”: 1 string mismatch"
# # [2] "Component “message”: 1 string mismatch"
# # [3] "Component “overridecomments”: 1 string mismatch"
# # [4] "Component “singleassignment”: 1 string mismatch"
#
# db_mult1 <- dat_pending_data %>%
#   filter(trip_report_id == fh_mult$edit_trip)
#
# View(db_mult1)
# all.equal(db_mult1[1,], db_mult1[2,])
#
# # ---
# db_n_fhier_data <-
#   left_join(
#     dat_pending_data,
#     from_fhier_data,
#     by = join_by(trip_report_id == edit_trip, res_msg == message)
#   )
# # ℹ Row 34599 of `x` matches multiple rows in `y`.
# # ℹ Row 5050 of `y` matches multiple rows in `x`.
#
# dat_pending_data_mult1 <-
#   dat_pending_data %>%
#   # [34599,] %>% View()
#   filter(trip_report_id == from_fhier_data[5050, ]$edit_trip &
#            res_msg == from_fhier_data[5050, ]$message)
#
# all.equal(dat_pending_data_mult1[1,], dat_pending_data_mult1[2,])
# # [2] "Component “trip_length”: Mean relative difference: 0.5"
# from_fhier_data_mult1 <-
#   from_fhier_data %>%
#   filter(edit_trip == dat_pending_data[34599, ]$trip_report_id &
#            message == dat_pending_data[34599, ]$res_msg)
#
# View(from_fhier_data_mult1)
# all.equal(from_fhier_data_mult1[1,], from_fhier_data_mult1[2,])
# row.names(from_fhier_data_mult1[1,])
# # T
#
# db_n_fhier_data_3 <-
#   left_join(
#     dat_pending_data,
#     from_fhier_data,
#     by = join_by(
#       trip_report_id == edit_trip,
#       res_msg == message,
#       trip_length == trip_length
#     )
#   )
#
# dat_pending_data_mult2 <-
#   dat_pending_data %>%
#   # [34599,] %>% View()
#   filter(trip_report_id == from_fhier_data[5046, ]$edit_trip &
#            res_msg == from_fhier_data[5046, ]$message)
#
# # View(dat_pending_data_mult2)
# all.equal(dat_pending_data_mult2[1,], dat_pending_data_mult2[2,])
# # [1] "Attributes: < Component “row.names”: Mean relative difference: 1 >"
# # duplicate
#
# from_fhier_data_mult2 <-
#   from_fhier_data %>%
#   filter(edit_trip == dat_pending_data[34599, ]$trip_report_id &
#            message == dat_pending_data[34599, ]$res_msg)
# dim(from_fhier_data_mult2)

# ---- left join ----

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

# --- full join
# db_n_fhier_data_all <-
#   full_join(
#     unique(dat_pending_data),
#     unique(from_fhier_data),
#     by = join_by(
#       trip_report_id == edit_trip,
#       res_msg == message,
#       trip_length == trip_length,
#       arr_year == arr_year,
#       arr_year_month == arr_year_month
#       # ,
#       # vessel_name == vessel_name
#     )
#   )
#
# dim(db_n_fhier_data_all)
# glimpse(db_n_fhier_data_all)
# full_join 48468    57

# ==== from FHIER test ====
from_fhier_data_od_all <-
  from_fhier_data %>%
  filter(vesselofficialnumber == 'FL4673SY') %>%
  # select(arrival) %>% unique() %>%
  arrange(arrival)
# 68
# Feb-22

from_fhier_data_od_apr <-
  from_fhier_data %>%
  filter(vesselofficialnumber == 'FL4673SY',
         arrival > '2023-04-04')
# 3

# compare test ====
names(dat_pending_date_od_all)
db_to_compare <-
  dat_pending_date_od_all %>%
  select(
    official_number,
    res_msg,
    arr_year_month,
    overridden,
    departure_date,
    arrival_date,
    trip_length
  )
# 91 7

# names(from_fhier_data_od_all)
fhier_to_compare <-
  from_fhier_data_od_all %>%
  select(
    vesselofficialnumber,
    message,
    arr_year_month,
    overridden1,
    departure,
    arrival,
    trip_length
  )
# 68 7

names(fhier_to_compare) <- names(db_to_compare)

all.equal(db_to_compare, fhier_to_compare)

setdiff(db_to_compare$arrival_date, fhier_to_compare$arrival_date) %>%
  length()
# 42

setdiff(fhier_to_compare$official_number,
        db_to_compare$official_number)

write_csv(db_to_compare, "~/db_to_compare.csv")
write_csv(fhier_to_compare, "~/fhier_to_compare.csv")

dat_pending_date_od_all %>%
  filter(arr_year_month == 'Feb 2022')

not_in_fhier_od <- c(
  '2022-02-16',
  '2022-03-25',
  '2022-04-02',
  '2022-04-06',
  '2022-04-12',
  '2022-04-13',
  '2022-04-19',
  '2022-04-26',
  '2022-05-17',
  '2022-06-24',
  '2022-06-25',
  '2022-08-03',
  '2022-08-17',
  '2022-08-26',
  '2022-09-05',
  '2022-09-10',
  '2022-09-22',
  '2022-09-24',
  '2022-10-06',
  '2022-10-11',
  '2022-10-12',
  '2022-10-13',
  '2022-10-15',
  '2022-10-17',
  '2022-11-06',
  '2022-11-30',
  '2022-12-07',
  '2023-01-25',
  '2023-01-26',
  '2023-03-01',
  '2023-03-02',
  '2023-04-05',
  '2023-04-15'
)
not_in_fhier_od_day <- as.Date(not_in_fhier_od) %>%
  as.data.frame() %>% set_names("arr_date_only")
str(not_in_fhier_od_day)

db_not_in_fhier_od_day_all <-
  dat_pending_date_od_all %>%
  mutate(arr_date_only = as.Date(arrival_date))

db_not_in_fhier_od_day_all1 <-
  dat_pending_date_od_all %>%
  mutate(arr_date_only = as.Date(arrival_date)) %>%
  filter(arr_date_only %in% not_in_fhier_od_day$arr_date_only)

str(db_not_in_fhier_od_day_all1)
intersect(db_not_in_fhier_od_day_all$arr_date_only,
          not_in_fhier_od_day$arr_date_only) %>% str()
33

# names(not_in_fhier_od_day)
# ij <-
#   inner_join(db_not_in_fhier_od_day_all,
#              not_in_fhier_od_day,
#              by = join_by(arr_date_only))
# 'data.frame':	50 obs. of  28 variables:
# same as wiht %in%

View(dat_pending_date_od_all)

not_in_fhier_od_day %<>%
  mutate(not_in_fhier = "DB")

db_dat_od1 <-
  left_join(db_not_in_fhier_od_day_all,
            not_in_fhier_od_day,
            by = join_by(arr_date_only))
View(db_dat_od1)

db_dat_od1 %<>%
  mutate(db_dat_od1, not_in_fhier = replace_na(not_in_fhier, 'F'))
# %>% tail()

db_dat_od1 %>%
  filter(trimws(captain_name) == "VMS") %>%
  select(not_in_fhier) %>%
  unique()
# both
# str()
val_param_id_vec <-
  db_dat_od1 %>%
  filter(not_in_fhier == "DB") %>%
  select(val_param_id) %>% unique()
View()

#?#
vec <- val_param_id_vec$val_param_id
vec1 <- paste(vec, collapse = ",")
qmarks <- paste(rep("?", length(vec)), collapse = ",")

# val_param_id IN ('?','?','?','?','?','?','?','?','?')"),
# VAL_PARAM_ID, VAL_PARAM_YR, IS_ENABLED, VAL_PARAM_TABLE
# val_param_id_res <- dbGetQuery(
#   con,
#   paste(
#     "
#     SELECT
#     *
# FROM
#   srh.val_param@secapxdv_dblk
# WHERE
# val_param_id = ?"
#   ),
# params = "380798"
# )

# get_val_year_sql <- "
#     SELECT
#     *
# FROM
#   srh.val_param@secapxdv_dblk
# WHERE
# val_param_id in (?val_param_id_list)"
#
# get_val_year_sql_val <- sqlInterpolate(con,
#                                        get_val_year_sql
#                                        ,
#                                        val_param_id_list = as.list(val_param_id_vec$val_param_id))
# print(dbGetQuery(con, query))


# DBI::dbGetQuery(con, "select 1 from dual where 1 in (1,2)")

# airport <- dbSendQuery(con, "SELECT 1 FROM dual WHERE 1 = ?")

# sql <- "select 1 from dual where 1 in (?num1, ?num2)"
# "SELECT * FROM X WHERE name = ?name"
# sqlInterpolate(ANSI(), sql, num1 = 1, num2 = 2)

# sql1 <- "select 1 from dual where 1 in (?my_list)"
# my_list = c(1,2)
# sqlInterpolate(ANSI(), sql1, .dots = my_list)
# sql <- sqlInterpolate(
#   conn = conn,
#   sql = sql_txt,
#   .dots = sql_params
# )

# ---
# a <- letters[1:10]
# b <- 11:20
# a <- 1:2
# sql_txt_a <- paste0("select 1 from dual where 1 in (",
# paste0("?parameter", seq_along(a),
# collapse = ",\n  "),
# ")")

# paste0(
# "?my_list)",
# ,
# "(?parameter", seq_along(a), ", ?actualVal", seq_along(b), ")",
# collapse = ",\n  "

# cat(sql_txt_a)

# sql_params <- append(
#   setNames(as.list(a), paste0("parameter", seq_along(a))),
#   setNames(as.list(b), paste0("actualVal", seq_along(b)))
# )
# sql_params <-
#   setNames(as.list(a), paste0("parameter", seq_along(a)))
# str(sql_params)
#
# sql <- sqlInterpolate(ANSI(),
#                       sql = sql_txt_a,
#                       .dots = sql_params)
#
# DBI::dbExecute(con, sql)
#
# sql_text_temp <- paste0("
#     SELECT
#     *
# FROM
#   srh.val_param@secapxdv_dblk
# WHERE
#   val_param_id in (", param_list, ")")

sql_text_in <- "SELECT
    *
FROM
  srh.val_param@secapxdv_dblk
WHERE
  val_param_id in "

my_param_df <- val_param_id_vec$val_param_id

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

my_val_sql_text <- "SELECT
    VAL_PARAM_ID, VAL_PARAM_YR, IS_ENABLED, VAL_PARAM_TABLE
FROM
  srh.val_param@secapxdv_dblk
WHERE
  val_param_id in "

my_val_sql <- make_sql_parameters(my_param_df, my_val_sql_text)
my_val_res <- DBI::dbGetQuery(con, my_val_sql)
View(my_val_res)
# all 2021

val_param_id_vec1 <-
  db_dat_od1 %>%
  # filter(not_in_fhier == "DB") %>%
  select(val_param_id) %>% unique()
str(val_param_id_vec1)

# ---
# DBI::dbGetQuery(con, "select 1 from dual where 1 in ('?','?')", params = list(1, 2))
#
# sql <- "SELECT ?value from dual"
# query <- sqlInterpolate(con, sql, value = 3)
# print(dbGetQuery(con, query))
# #
# # 1 1
# DBI::dbGetQuery(con, "select 1 from dual where 1 in (?,?)", params = list(2, 3))
#
#
# dbGetQuery(con,
#            "SELECT COUNT(*) FROM mtcars WHERE cyl = ?",
#            params = list(1:8))
#
# params = as.list(vec))
# val_param_id IN (?,381044,380797,383782,380799,401206,623412,630838,381040)"),
# params = list(380798))


# ===
# FHIER by year ----
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
## FHIER by year and month ----
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
# %>% View()

## compare by year_month
all.equal(dat_pending_date_by_ym, from_fhier_data_by_ym)
View(dat_pending_date_by_ym)
View(from_fhier_data_by_ym)

both_ym <-
  left_join(dat_pending_date_by_ym,
            from_fhier_data_by_ym,
            by = join_by(arr_year_month))

View(both_ym)

# === test db_n_fhier_data_ok ====
# db_n_fhier_data_ok %>% filter(!(vessel_name.x == vessel_name.y)) %>%
#   select(trip_report_id, vessel_id, vessel_name.x, vessel_name.y, vesselofficialnumber, official_number) %>% str()
# 'data.frame':	124 obs. of  57 variables:

grep("x", names(db_n_fhier_data_ok), value = T)
# "overridden.x"

db_n_fhier_data_all_od <-
  db_n_fhier_data_ok %>%
  filter(official_number == 'FL4673SY') %>%
  # select(arrival) %>% unique() %>%
  arrange(arrival)

dim(db_n_fhier_data_all_od)
# [1] 91 55
# 'data.frame':	43 obs. of  55 variables:

db_n_fhier_data_all_od %>%
  View()

# === Diff between DB and FHIER ====
# Nov 2022
# 1006
# 0
# 1006
# 78
# 1
# 79

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
  select(singleassignment) %>% unique() %>% View()
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
filter(asg_info == "Unassigned" & arr_year_month > "Jan 2022") %>% View()
%>% dim()
# [1] 4824   56
