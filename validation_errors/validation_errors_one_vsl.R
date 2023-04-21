## === test DB one vessel ====
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

## === From FHIER test one vessel ====
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

## === Both test one vessel ====
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
# F

setdiff(db_to_compare$arrival_date, fhier_to_compare$arrival_date) %>%
  length()
# 42

setdiff(fhier_to_compare$official_number,
        db_to_compare$official_number)
# 0

# write_csv(db_to_compare, "~/db_to_compare.csv")
# write_csv(fhier_to_compare, "~/fhier_to_compare.csv")

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
glimpse(val_param_id_vec)

sql_text_in <- "SELECT
    *
FROM
  srh.val_param@secapxdv_dblk
WHERE
  val_param_id in "

my_param_df <- val_param_id_vec$val_param_id


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

## === test db_n_fhier_data_ok ====
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
  glimpse()

