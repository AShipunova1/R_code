# how many interviews with no logbooks ----

## prep lgb info ----
db_logbooks_2022_vsl_t_end <-
  db_logbooks_2022 |>
  select(VESSEL_OFFICIAL_NBR, TRIP_END_DATE, TRIP_ID) |>
  mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR)) |>
  mutate(TRIP_END_DATE = lubridate::date(TRIP_END_DATE)) |>
  distinct()

dim(db_logbooks_2022_vsl_t_end)
# [1] 86134     2
# [1] 94870     3 w trip_id

## prep survey info ----
survey_data_l_2022_date_i1_vsl_int_t <-
  survey_data_l_2022_vsl_date |>
  filter(int_year == "2022") |>
  select(vsl_num, interview_date) |>
  mutate(vsl_num = stringr::str_replace_all(vsl_num, " ", "")) |>
  mutate(vsl_num = stringr::str_replace_all(vsl_num, "-", "")) |>
  mutate(vsl_num = tolower(vsl_num)) |>
  mutate(interview_date = lubridate::date(interview_date)) |>
  distinct()

dim(survey_data_l_2022_date_i1_vsl_int_t)
# 1812

## survey not in lgb by vessel and day ----

survey_not_in_lgb <-
  survey_data_l_2022_date_i1_vsl_int_t |>
  filter(
    !(
      vsl_num %in% tolower(db_logbooks_2022_vsl_t_end$VESSEL_OFFICIAL_NBR) &
        lubridate::date(interview_date) %in% lubridate::date(db_logbooks_2022_vsl_t_end$TRIP_END_DATE)
    )
  )

dim(survey_not_in_lgb)
# 1448    in
# 364 not in

# percent
# 364 * 100 / 1812
# 20%

## the same with full join 
lgb_join_i1_full <-
  dplyr::full_join(
    db_logbooks_2022_vsl_t_end,
    survey_data_l_2022_date_i1_vsl_int_t,
    join_by(
      VESSEL_OFFICIAL_NBR == vsl_num,
      TRIP_END_DATE == interview_date
    ),
    relationship = "many-to-many"
  )

# View(lgb_join_i1_full)

intv_w_no_lgb_join_by_day_vsl <- 
  lgb_join_i1_full |> 
  filter(is.na(TRIP_ID)) |> 
  auxfunctions::remove_empty_cols() |> 
  distinct()

glimpse(db_logbooks_2022_vsl_t_end)

glimpse(survey_data_l_2022_date_i1_vsl_int_t)
glimpse(intv_w_no_lgb_join_by_day_vsl)
# 827

# compare survey_not_in_lgb and intv_w_no_lgb_join_by_day_vsl

vessels_from_join_not_in_lgb <-
  intv_w_no_lgb_join_by_day_vsl$VESSEL_OFFICIAL_NBR |>
  unique()

length(vessels_from_join_not_in_lgb)
# 261

## why different from the previous method? ----
vsl_in_diff_methods__no_join <- 
setdiff(
  unique(survey_data_l_2022_date_i1_vsl_int_t$vsl_num),
  unique(intv_w_no_lgb_join_by_day_vsl$VESSEL_OFFICIAL_NBR)
)

length(vsl_in_diff_methods__no_join)
# 168

vsl_in_diff_methods__join <-
  setdiff(
    unique(intv_w_no_lgb_join_by_day_vsl$VESSEL_OFFICIAL_NBR),
    unique(survey_data_l_2022_date_i1_vsl_int_t$vsl_num)
  )

length(vsl_in_diff_methods__join)
# 0

n_distinct(survey_data_l_2022_date_i1_vsl_int_t$vsl_num)
# [1] 429

n_distinct(intv_w_no_lgb_join_by_day_vsl$VESSEL_OFFICIAL_NBR)
# [1] 261

# check
survey_data_l_2022_date_i1_vsl_int_t |> 
  dplyr::filter(vsl_num %in% vsl_in_diff_methods__no_join) |> 
  head()
# false positives
  
survey_data_l_2022_date_i1_vsl_int_t |> 
    filter(vsl_num == "1041849") |> 
    filter(lubridate::month(interview_date) == 4) |>
    glimpse()
# 1

db_logbooks_2022 |> 
  filter(VESSEL_OFFICIAL_NBR == "1041849") |> 
  filter(lubridate::month(TRIP_END_DATE) == 4) |> 
  glimpse()
# 1
# diff day

## check all vessel ids not in lgb ----
# survey_not_in_lgb$vsl_num |> 
#   cat(sep = ", ")

survey_vsl_num_not_in_lgb <- 
  survey_not_in_lgb$vsl_num |> 
  unique()

length(survey_vsl_num_not_in_lgb)
# 152 

lubridate::intersect(tolower(db_logbooks_2022$VESSEL_OFFICIAL_NBR),
                     tolower(survey_vsl_num_not_in_lgb))
# 0


lubridate::setdiff(tolower(survey_vsl_num_not_in_lgb),
                   tolower(db_logbooks_2022$VESSEL_OFFICIAL_NBR)) |> 
  unique() |> 
  length()
# 152

vsl_in_survey_not_in_lgb <-
  lubridate::setdiff(
    tolower(survey_vsl_num_not_in_lgb),
    tolower(processed_logbooks_2022_calendar$VESSEL_OFFICIAL_NUMBER)
  ) |>
  unique() 
length(vsl_in_survey_not_in_lgb)
# 152

vsl_in_survey_not_in_lgb__str <-
  vsl_in_survey_not_in_lgb |>
  toupper() |> 
  paste(collapse = "', '")

vsl_in_survey_not_in_lgb_query <- 
    stringr::str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_end_date >= '{my_date_beg}'
  AND trip_start_date <= '{my_date_end}'
  and vessel_official_nbr IN ('{vsl_in_survey_not_in_lgb__str}')
")

vsl_in_survey_not_in_lgb_query_res <-
  try(DBI::dbGetQuery(con, vsl_in_survey_not_in_lgb_query))

vsl_in_survey_not_in_lgb_query_res |> dim()
# 0

## check if these interviews are for DNFs ----
dplyr::glimpse(db_dnfs_2022)

in_survey_not_in_lgb_not_in_dnf <-
  lubridate::setdiff(tolower(survey_vsl_num_not_in_lgb),
                     tolower(db_dnfs_2022$VESSEL_OFFICIAL_NBR)) |>
  unique()

length(in_survey_not_in_lgb_not_in_dnf)
# 152

length(survey_vsl_num_not_in_lgb) == length(in_survey_not_in_lgb_not_in_dnf)
# T, vessels are not in lgb, not in dnf