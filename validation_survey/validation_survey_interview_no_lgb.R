# how many interviews with no logbooks?

# Prep data for interviews w no logbooks ----
## prep lgb info ----
db_logbooks_2022_vsl_t_end_all <-
  db_logbooks_2022 |>
  dplyr::mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR)) |>
  dplyr::mutate(TRIP_END_DATE = lubridate::date(TRIP_END_DATE))

db_logbooks_2022_vsl_t_end <-
  db_logbooks_2022_vsl_t_end_all |>
  dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_END_DATE, TRIP_ID) |>
  dplyr::distinct()

dim(db_logbooks_2022_vsl_t_end)
# [1] 86134     2
# [1] 94870     3 w trip_id

## prep survey info ----
# Explanations:
# - `dplyr::select(vsl_num, interview_date, st, cnty)` selects specific columns (`vsl_num`, `interview_date`, `st`, `cnty`) from the data frame. The `select` function from the `dplyr` package is used to choose these columns.
# - `dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, " ", ""))` modifies the `vsl_num` column by removing all spaces. The `str_replace_all` function from the `stringr` package is used to replace spaces with an empty string.
# - `dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, "-", ""))` further modifies the `vsl_num` column by removing all hyphens. The `str_replace_all` function from the `stringr` package is used to replace hyphens with an empty string.
# - `dplyr::mutate(vsl_num = tolower(vsl_num))` converts all characters in the `vsl_num` column to lowercase. The `tolower` function is used for this conversion.
# - `dplyr::mutate(interview_date = lubridate::date(interview_date))` converts the `interview_date` column to Date format. The `date` function from the `lubridate` package is used for this conversion.
# - `dplyr::distinct()` removes duplicate rows from the data frame. The `distinct` function from the `dplyr` package is used to ensure that only unique rows are kept.
# 
# This code processes the `survey_data_l_2022_i1_w_dates` data frame to clean and transform the `vsl_num` and `interview_date` columns, and selects only the relevant columns while removing any duplicate rows. The result is stored in `survey_data_l_2022_date_i1_vsl__int_t`.
survey_data_l_2022_date_i1_vsl__int_t_clean_vsl <-
  survey_data_l_2022_i1_w_dates |>
  dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, " ", "")) |>
  dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, "-", "")) |>
  dplyr::mutate(vsl_num = tolower(vsl_num)) |>
  dplyr::mutate(interview_date = lubridate::date(interview_date))

dim(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl)

survey_data_l_2022_date_i1_vsl__int_t <-
  survey_data_l_2022_date_i1_vsl__int_t_clean_vsl |>
  dplyr::select(id_code, vsl_num, interview_date, st, cnty) |>
  dplyr::distinct()

survey_data_l_2022_date_i1_vsl__int_t |>
	head() |>
	dplyr::glimpse()
# 1812
# 1835 w id code

### check empty state and county ----

# summary(survey_data_l_2022_i1_w_dates)

survey_data_l_2022_i1_w_dates |> 
  dplyr::filter(is.na(st)) |> 
  nrow()
# 312 NAs out of 1523+312 = 1835

survey_data_l_2022_i1_w_dates |> 
  dplyr::filter(!cnty > 0) |> 
  nrow()
# 0, OK, no NAs

# survey_data_l_2022_i1_w_dates |> 
#   dplyr::count(cnty) |> tail()

### restore possible states ----
survey_data_l_2022_i1_w_dates__states_by_cnty |>
	head() |>
	dplyr::glimpse()

survey_data_l_2022_date_i1_vsl__int_t |>
	head() |>
	dplyr::glimpse()

restore_states <- function(my_df) {
  my_df_join_states_by_cnty <-
    my_df |>
    dplyr::left_join(survey_data_l_2022_i1_w_dates__states_by_cnty,
                     dplyr::join_by(cnty, st))
  
  my_df_join_states_by_cnty__restored_st <-
    my_df_join_states_by_cnty |>
    dplyr::rowwise() |>
    dplyr::mutate(temp_res =
                    case_when(is.na(st) ~ paste(unlist(states_l_by_cnty), collapse = ""), 
                              .default = st)) |>
    dplyr::mutate(restored_st =
                    stringr::str_extract(temp_res, "\\d+")) |>
    dplyr::select(-temp_res) |>
    dplyr::ungroup()
  
  return(my_df_join_states_by_cnty__restored_st)
}

survey_data_l_2022_date_i1_vsl__int_t__restore_st <-
  restore_states(survey_data_l_2022_date_i1_vsl__int_t)

glimpse(survey_data_l_2022_date_i1_vsl__int_t__restore_st)

# check
survey_data_l_2022_date_i1_vsl__int_t__restore_st[1,] |> 
  str()
# "12"
survey_data_l_2022_date_i1_vsl__int_t__restore_st[135,] |> 
  str()
# "NA"

survey_data_l_2022_date_i1_vsl__int_t__restore_st |> 
  tail(3) |> 
  str()
# as st, ok

### format state and county codes ----
format_state_and_county_codes <-
  function(my_df, state_code_field) {
    my_df |>
      dplyr::mutate(st_2 =
               case_when(is.na(!!sym(state_code_field)) ~ "00", .default =
                           stringr::str_pad(!!sym(state_code_field), 2, pad = "0"))) |>
      dplyr::mutate(cnty_3 = stringr::str_pad(cnty, 3, pad = "0"),
             fips = paste0(st_2, cnty_3))
    
  }

survey_data_l_2022_date_i1_vsl__int_t__fips <-
  survey_data_l_2022_date_i1_vsl__int_t |>
  format_state_and_county_codes("st")

survey_data_l_2022_date_i1_vsl__int_t__restore_st__fips <- 
  survey_data_l_2022_date_i1_vsl__int_t__restore_st |> 
  format_state_and_county_codes("restored_st")

# Join for interviews w no logbooks ----
full_join_int_lgb <- function(survey_df, by_fields = NA) {
  if (is.na(by_fields)) {
    by_fields = dplyr::join_by(vsl_num == VESSEL_OFFICIAL_NBR,
                               interview_date == TRIP_END_DATE)
  }
  dplyr::full_join(
    survey_df,
    db_logbooks_2022_vsl_t_end,
    by = by_fields,
    relationship = "many-to-many"
  )
}

## 1 full join by date and vessel ----
intersect(
  db_logbooks_2022_vsl_t_end$VESSEL_OFFICIAL_NBR,
  survey_data_l_2022_date_i1_vsl__int_t__fips$vsl_num
) |> length()
# 277

setdiff(
  survey_data_l_2022_date_i1_vsl__int_t__fips$vsl_num,
    db_logbooks_2022_vsl_t_end$VESSEL_OFFICIAL_NBR
) |> length()
# 152

lgb_join_i1_full <- 
  full_join_int_lgb(survey_data_l_2022_date_i1_vsl__int_t__fips)

dim(lgb_join_i1_full)
# [1] 95697     3

summary(lgb_join_i1_full)

### 1a the same with restored states ----

lgb_join_i1_full_restored <- 
  full_join_int_lgb(survey_data_l_2022_date_i1_vsl__int_t__restore_st__fips)

dim(lgb_join_i1_full_restored)
# [1] 95697    10

# auxfunctions::data_overview(lgb_join_i1_full_restored)

### get interviews w no logbooks ----
intv_w_no_lgb_join_by_day_vsl <- 
  lgb_join_i1_full |>
  dplyr::filter(is.na(TRIP_ID)) |> 
  auxfunctions::remove_empty_cols() |> 
  dplyr::distinct()

dim(intv_w_no_lgb_join_by_day_vsl)
# [1] 827   7

# same for restored states
intv_w_no_lgb_join_by_day_vsl_restored <-
  lgb_join_i1_full_restored |>
  dplyr::filter(is.na(TRIP_ID)) |>
  auxfunctions::remove_empty_cols() |>
  dplyr::distinct()

dim(intv_w_no_lgb_join_by_day_vsl_restored)
# [1] 827   9

### check NAs ----
summary(intv_w_no_lgb_join_by_day_vsl)
# [1] 827   2 (no NAs)

intv_w_no_lgb_join_by_day_vsl |> 
  dplyr::filter(is.na(st)) |> 
  dim()
# 192

intv_w_no_lgb_join_by_day_vsl |> 
  dplyr::filter(is.na(cnty)) |> 
  dim()
# 0 ok

intv_w_no_lgb_join_by_day_vsl |>
	head() |>
	dplyr::glimpse()
intv_w_no_lgb_join_by_day_vsl_restored |>
	head() |>
	dplyr::glimpse()

### check all vessel ids not in lgb ----
# intv_w_no_lgb_join_by_day_vsl$VESSEL_OFFICIAL_NBR |>
#   unique() |> 
#   cat(sep = ", ")

survey_vsl_num_not_in_lgb <- 
  intv_w_no_lgb_join_by_day_vsl$vsl_num |>
  unique()

length(survey_vsl_num_not_in_lgb)
# 261

lubridate::intersect(tolower(db_logbooks_2022$VESSEL_OFFICIAL_NBR),
                     tolower(survey_vsl_num_not_in_lgb)) |> 
  length()
# 109

vsl_in_survey_not_in_db_lgb <-
  lubridate::setdiff(
    tolower(survey_vsl_num_not_in_lgb),
    tolower(db_logbooks_2022$VESSEL_OFFICIAL_NBR)
  ) |>
  unique()
length(vsl_in_survey_not_in_db_lgb)
# 152

vsl_in_survey_not_in_processed_lgb <-
  lubridate::setdiff(
    tolower(survey_vsl_num_not_in_lgb),
    tolower(processed_logbooks_2022_calendar$VESSEL_OFFICIAL_NUMBER)
  ) |>
  unique() 

length(vsl_in_survey_not_in_processed_lgb) == length(vsl_in_survey_not_in_db_lgb)
# T

vsl_in_survey_not_in_lgb__str <-
  vsl_in_survey_not_in_db_lgb |>
  toupper() |> 
  paste(collapse = "', '")

vsl_in_survey_not_in_lgb_query <- 
    stringr::str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_end_date >= TO_DATE('{my_date_beg}', 'yyyy-mm-dd')
  AND trip_start_date <= TO_DATE('{my_date_end}', 'yyyy-mm-dd')
  and vessel_official_nbr IN ('{vsl_in_survey_not_in_lgb__str}')
")

vsl_in_survey_not_in_lgb_query_res <-
  try(DBI::dbGetQuery(con, vsl_in_survey_not_in_lgb_query))

vsl_in_survey_not_in_lgb_query_res |> dim()
# 0 (confirmed not in lgb for 2022)

### check if these interviews are for DNFs ----
db_dnfs_2022 |>
	head() |>
	dplyr::glimpse()

in_survey_not_in_lgb_not_in_dnf <-
  lubridate::setdiff(tolower(survey_vsl_num_not_in_lgb),
                     tolower(db_dnfs_2022$VESSEL_OFFICIAL_NBR)) |>
  unique()

length(in_survey_not_in_lgb_not_in_dnf)
# 261

length(survey_vsl_num_not_in_lgb) == length(in_survey_not_in_lgb_not_in_dnf)
# T, vessels are not in lgb, not in dnf

# manual check
intv_w_no_lgb_join_by_day_vsl |> 
  dplyr::arrange(vsl_num,
          interview_date) |> 
  dplyr::filter(vsl_num == "1041849") |> 
  head(10)

one_check_query <- 
  stringr::str_glue("SELECT
  *
FROM
  srh.mv_safis_trip_download@secapxdv_dblk
WHERE
    trip_end_date >= TO_DATE('2022-08-01', 'yyyy-mm-dd')
    AND trip_end_date <= TO_DATE('2022-08-30', 'yyyy-mm-dd')
    AND vessel_official_nbr IN ('1041849')
")

one_check_res <-
  try(DBI::dbGetQuery(con, one_check_query))
one_check_res |> dim()
# 0

# one_check_res$TRIP_END_DATE |> 
#   unique() |> 
#   sort() |> 
#   print()

## spot check the interviews by harvest ----

test1_tsns <-
  survey_i1_i3_harvested_dates |>
  dplyr::filter(vsl_num == '1041849' &
           interview_date == lubridate::ymd('2022-06-06')) |>
  dplyr::distinct() |>
  dplyr::select(tsn) |>
  dplyr::distinct()
# dplyr::glimpse()

test1_logbooks <-
  db_logbooks_2022 |>
  dplyr::filter(
    lubridate::month(TRIP_END_DATE) == 6 &
      CATCH_SPECIES_ITIS %in% test1_tsns$tsn &
      VESSEL_OFFICIAL_NBR == "1041849"
  ) |> 
  dplyr::select(TRIP_ID, CATCH_SPECIES_ITIS, TRIP_END_DATE) |> 
  dplyr::distinct()

unique(test1_logbooks$CATCH_SPECIES_ITIS)
# only one out of 3

test1_logbooks_no_month <-
  db_logbooks_2022 |>
  dplyr::filter(
    # lubridate::month(TRIP_END_DATE) == 6 &
      CATCH_SPECIES_ITIS %in% test1_tsns$tsn &
      VESSEL_OFFICIAL_NBR == "1041849"
  ) |> 
  dplyr::select(TRIP_ID, CATCH_SPECIES_ITIS, TRIP_END_DATE) |> 
  dplyr::distinct()

test1_logbooks_no_month |> 
  dplyr::count(CATCH_SPECIES_ITIS)
#   CATCH_SPECIES_ITIS  n
# 1             167759  2
# 2             167763  1
# 3             168853 27

test1_logbooks_no_month_2spp <- 
  test1_logbooks_no_month |> 
  dplyr::filter(CATCH_SPECIES_ITIS %in% c("167759", "167763"))

db_logbooks_2022 |>
  dplyr::filter(TRIP_ID %in% c("62538162", "63569515", "62538257")) |>
  head() |> 
  dplyr::glimpse()

# one vessel only has those 2 spp
db_logbooks_2022 |>
  dplyr::filter(TRIP_ID %in% c("62538162", "63569515", "62538257")) |>
  dplyr::select(
    TRIP_ID,
    TRIP_END_DATE,
    CATCH_SPECIES_ITIS,
    COMMON_NAME,
    REPORTED_QUANTITY,
    DISPOSITION_NAME
  ) |> 
  dplyr::distinct() |>
  dplyr::group_by(TRIP_ID) |>
  dplyr::mutate(all_spp = list(sort(paste(
    unique(CATCH_SPECIES_ITIS)
  ))),
  spp_cnt = length(all_spp)) |>
  dplyr::ungroup() |>
  dplyr::arrange(TRIP_END_DATE) |>
  head() |> 
  dplyr::glimpse()

# there are no trips with both ("167759", "167763")

## spot check the interviews by time window ----
# TODO
# 2)
# And if you limit to a smaller window (e.g. end or start in logbook within 1 hour of the survey, or within 2, or within 3 hours) how does that % come out?

# Interviews w no logbooks by captain name ----
# MM 1) also suggest using captain's name - to try to match, if that is a field in both. like instead of just trying to match by vessel ID.

# print_df_names(db_logbooks_2022_vsl_t_end_all)
# CAPT_NAME_FIRST, CAPT_NAME_LAST

# print_df_names(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl)
# interviewee_f_name, interviewee_l_name

intersect(
  tolower(db_logbooks_2022_vsl_t_end_all$CAPT_NAME_FIRST),
  tolower(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl$interviewee_f_name)) |> length()
# 183

intersect(
  tolower(db_logbooks_2022_vsl_t_end_all$CAPT_NAME_LAST),
  tolower(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl$interviewee_l_name)) |> length()
# 279

setdiff(
  tolower(
    survey_data_l_2022_date_i1_vsl__int_t_clean_vsl$interviewee_l_name
  ),
  tolower(db_logbooks_2022_vsl_t_end_all$CAPT_NAME_LAST)
) |> length()
# 134

by_fields = dplyr::join_by(vsl_num == VESSEL_OFFICIAL_NBR,
                           interviewee_l_name == CAPT_NAME_LAST)

survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low <- 
  survey_data_l_2022_date_i1_vsl__int_t_clean_vsl |> 
  dplyr::mutate(vsl_num = tolower(vsl_num),
         interviewee_l_name = tolower(interviewee_l_name))

db_logbooks_2022_vsl_t_end_all_low <- 
  db_logbooks_2022_vsl_t_end_all |> 
  dplyr::mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR),
         CAPT_NAME_LAST = tolower(CAPT_NAME_LAST))

### join by captain name instead of a vessel ----
# 1) get all id_codes with no logbooks from the by vessel and day join;
# 2) get captain last name for these id_codes
# 3) join logbooks to surveys which marked as having no logbooks to logbooks by date and captain last name

# 1) get all id_codes with no logbooks, add interviewee_l_name
# 2) get captain last name for these id_codes

survey_fields_to_compare <-
  c(
    names(intv_w_no_lgb_join_by_day_vsl),
    "interviewee_f_name",
    "interviewee_l_name",
    "vessel_name"
  )

survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb <-
  survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low |>
  dplyr::select(tidyselect::any_of(survey_fields_to_compare)) |>
  dplyr::distinct() |>
  # 1835
  dplyr::filter(id_code %in% intv_w_no_lgb_join_by_day_vsl$id_code)

dim(intv_w_no_lgb_join_by_day_vsl)
# 833
dim(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb)
# 833

# 3) join logbooks to surveys which marked as having no logbooks to logbooks by date and captain last name

by_fields =
  dplyr::join_by(interviewee_l_name == CAPT_NAME_LAST,
                 interview_date == TRIP_END_DATE)

# num_int_no_lgb <- 
#   n_distinct(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb$vsl_num)
# 261
# dim(survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb)
# 833

join_by_date_captain <-
  survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb |>
  dplyr::full_join(db_logbooks_2022_vsl_t_end_all_low,
                   by = by_fields,
                   relationship = "many-to-many")

# ℹ Row 5 of `x` matches multiple rows in `y`.
# ℹ Row 32575 of `y` matches multiple rows in `x`.

survey_x_5 <-
  survey_data_l_2022_date_i1_vsl__int_t_clean_vsl_low__no_lgb[5, ]

lgb_fields_to_compare <-
  c(
    "TRIP_END_DATE",
    "CAPT_NAME_FIRST",
    "CAPT_NAME_LAST",
    "TRIP_ID",
    "VESSEL_OFFICIAL_NBR",
    "VESSEL_NAME",
    "STATE",
    "STATE_NAME",
    "END_PORT_COUNTY",
    "START_PORT_COUNTY"
  )

db_logbooks_2022_vsl_t_end_all_low |> 
  dplyr::filter(
    CAPT_NAME_LAST == survey_x_5$interviewee_l_name &
      TRIP_END_DATE == survey_x_5$interview_date
  ) |>
  dplyr::select(tidyselect::all_of(lgb_fields_to_compare)) |> 
  dplyr::distinct() |> 
  head() |> 
  dplyr::glimpse()

# diff everything else

# intersect(survey_x_3$id_code, intv_w_no_lgb_join_by_day_vsl$id_code)
# 0

db_logbooks_2022_vsl_t_end_all_low_32575 <-
  db_logbooks_2022_vsl_t_end_all_low[32575, ] |>
  dplyr::select(tidyselect::all_of(lgb_fields_to_compare)) |>
  dplyr::distinct()

db_logbooks_2022_vsl_t_end_all_low_32575 |>
	head() |>
	dplyr::glimpse()
# names == NA

### check if the join is correct ---
join_by_date_captain__has_lgb <-
  join_by_date_captain |>
  dplyr::filter(!is.na(TRIP_ID) &
           !is.na(id_code)) |>
  dplyr::select(tidyselect::any_of(c(
    survey_fields_to_compare, lgb_fields_to_compare
  ))) |>
  dplyr::distinct()

# dim(join_by_date_captain__has_lgb)
# 771

get_county_name <- function(state_both, cnty_3) {
  # browser()
  # state_both = "34"
  # cnty_3 = "315"
  res <-
    tidycensus::fips_codes |>
    dplyr::filter(state_code == state_both & county_code == cnty_3) |>
    dplyr::select(county) |>
    dplyr::mutate(county_short =
             stringr::str_replace_all(county, " County| Parish", "") |> 
             tolower())
  
  county_short <- res[["county_short"]]
  if (nrow(res) == 0) {
    county_short <- NA
  }
  
  return(county_short)
}

#### check vessel ids in join_by_date_captain__has_lgb ----

join_by_date_captain__has_lgb_short <-
  join_by_date_captain__has_lgb |>
  select(
    VESSEL_OFFICIAL_NBR,
    vsl_num,
    interviewee_l_name,
    interviewee_f_name,
    CAPT_NAME_FIRST,
    TRIP_ID,
    id_code
  ) |>
  filter(!is.na(interviewee_l_name))

nrow(join_by_date_captain__has_lgb_short)
# 184

# n_distinct(join_by_date_captain__has_lgb_short$VESSEL_OFFICIAL_NBR)
# 80
# 
# n_distinct(join_by_date_captain__has_lgb_short$vsl_num)
# 70

join_by_date_captain__has_lgb_short |>
  head() |>
  glimpse()

to_check_in_df <-
  join_by_date_captain__has_lgb |>
  select(
    vsl_num,
    VESSEL_OFFICIAL_NBR,
    vessel_name,
    VESSEL_NAME,
    interviewee_l_name,
    interviewee_f_name,
    CAPT_NAME_FIRST,
    st,
    STATE,
    STATE_NAME,
    cnty,
    END_PORT_COUNTY,
    id_code,
    TRIP_ID,
    interview_date
  ) |>
  rowwise() |>
  format_state_and_county_codes("STATE") |> 
  mutate(int_count_name =
           get_county_name(coalesce(st, STATE), cnty_3),
         .after = cnty,
         st_state = coalesce(st, STATE)) |>
  select(-c(st_2, cnty_3)) |> 
  ungroup()

# glimpse(to_check_in_df)

# tidycensus::fips_codes |>
    # dplyr::filter(state_code == "12" & county == "Okaloosa County") 
individual_pair_check <- function(field_name, field_value) {

  if (field_name == "VESSEL_OFFICIAL_NBR") {
    curr_filter <-
      rlang::quo(VESSEL_OFFICIAL_NBR == field_value)
  } else if (field_name == "interviewee_l_name") {
    curr_filter <-
      rlang::quo(interviewee_l_name == field_value)
  }
  
  res <- 
    to_check_in_df |>
    filter(!!curr_filter)
  
  return(res)
}

individual_pair_check("VESSEL_OFFICIAL_NBR", "1291008") |> 
  glimpse()

##### write out, check manually in PIMS, add notes, load back ----
# join_by_date_captain__has_lgb_short |>
#   readr::write_csv(file.path(curr_proj_output_path, "diff_vsl_ids_same_captn.csv"))

join_by_date_captain__has_lgb_short__checked <-
  openxlsx::read.xlsx(file.path(curr_proj_output_path, "diff_vsl_ids_same_captn.xlsx"))

diff_vsls <-
  join_by_date_captain__has_lgb_short__checked |>
  filter(is.na(notes) | grepl("diff", notes))

dim(diff_vsls)
# 62

same_vsls <-
  join_by_date_captain__has_lgb_short__checked |>
  filter(!is.na(notes)) |>
  filter(!grepl("diff", notes))

same_vsls |> head() |> glimpse()
nrow(same_vsls)
# 44

# n_distinct(same_vsls$VESSEL_OFFICIAL_NBR)
# 36
# n_distinct(same_vsls$vsl_num)
# 35

# We can use an vessel_official_number for vsl_num for same_vsls

#### get back fields ----
same_vsls_all_fields <-
  inner_join(join_by_date_captain__has_lgb, same_vsls)
# Joining with `by = join_by(vsl_num, interviewee_f_name, interviewee_l_name, CAPT_NAME_FIRST,
# VESSEL_OFFICIAL_NBR)`

same_vsls_all_fields |> head() |> glimpse()
# nrow(same_vsls_all_fields)
# 82

#### add county names ----
join_by_date_captain__has_lgb__fips <-
  join_by_date_captain__has_lgb |>
  format_state_and_county_codes("st")

#### add state if missing ----
join_by_date_captain__has_lgb__fips_st <- 
  join_by_date_captain__has_lgb__fips |>
  dplyr::mutate(state_both = coalesce(st, STATE))

# check
join_by_date_captain__has_lgb__fips_st |> 
  dplyr::filter(!st == STATE) |> 
  dplyr::count(st, STATE) |> 
  head() |> 
  dplyr::glimpse()

join_by_date_captain__has_lgb__fips_st |>
  dplyr::filter(st == STATE) |>
  dplyr::count(st, STATE) |>
  head() |> 
  dplyr::glimpse()

join_by_date_captain__has_lgb__fips_st |> 
  dplyr::count(state_both) |> 
  head() |> 
  dplyr::glimpse()

  # dplyr::select(where(~is.numeric(.x) && any(.x == 9)))
# join_by_date_captain__has_lgb__fips_st[40,] |> dplyr::glimpse()

join_by_date_captain__has_lgb__fips_st_county_names <-
  join_by_date_captain__has_lgb__fips_st |>
  dplyr::rowwise() |>
  dplyr::mutate(survey_county_name0 = get_county_name(state_both, cnty_3)) |>
  dplyr::ungroup()

dim(join_by_date_captain__has_lgb__fips_st_county_names)
# [1] 771  21

# check survey county names
join_by_date_captain__has_lgb__fips_st_county_names |>
  dplyr::rowwise() |>
  dplyr::mutate(survey_county_name_len = length(survey_county_name0)) |>
  dplyr::ungroup() |>
  dplyr::count(survey_county_name_len)
# 1     0     1
# 2     1   770
# fixed:
# 1     1   771  
           
join_by_date_captain__has_lgb__fips_st_county_names |>
  dplyr::count(survey_county_name0) |> 
  head() |> 
  dplyr::glimpse()

# check if county names are the same
join_by_date_captain__has_lgb__fips_st_county_names |> 
  dplyr::filter(survey_county_name0 == tolower(END_PORT_COUNTY)) |> 
  head() |> 
  dplyr::glimpse()

join_by_date_captain__has_lgb__fips_st_county_names[40,] |> 
  dplyr::select(survey_county_name0, END_PORT_COUNTY)

join_by_date_captain__has_lgb__fips_st_county_names |>
  dplyr::rowwise() |>
  dplyr::filter(
    !is.na(survey_county_name0) &
      agrepl(survey_county_name0, 
             tolower(END_PORT_COUNTY), 
             ignore.case = TRUE,
             max.distance = 2)
    ) |>
  dplyr::filter(!is.na(survey_county_name0) &
           !survey_county_name0 == tolower(END_PORT_COUNTY)) |>
  dplyr::ungroup() |>
  dplyr::select(survey_county_name0, END_PORT_COUNTY) |>
  dplyr::distinct() |>
  head() |> 
  dplyr::glimpse()
# $ survey_county_name0 <chr> "levy"
# $ END_PORT_COUNTY     <chr> "LEE"

# All counties are either completely different or the same

join_by_date_captain__has_lgb__fips_st_county_names_short <-
  join_by_date_captain__has_lgb__fips_st_county_names |>
  dplyr::select(
    id_code,
    TRIP_ID,
    interview_date,
    vsl_num,
    VESSEL_OFFICIAL_NBR,
    vessel_name,
    VESSEL_NAME,
    interviewee_l_name,
    interviewee_f_name,
    CAPT_NAME_FIRST,
    state_both,
    cnty_3,
    survey_county_name0,
    END_PORT_COUNTY,
    fips
  ) |> 
  dplyr::distinct()

dim(join_by_date_captain__has_lgb__fips_st_county_names_short)
# [1] 771  15

#### same county ----
join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty <-
  join_by_date_captain__has_lgb__fips_st_county_names_short |>
  dplyr::filter(survey_county_name0 == tolower(END_PORT_COUNTY))

dim(join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty)
# 122

#### the same first name ----
join_by_date_captain__has_lgb__fips_st_county_names_short_same_f_name <-
  join_by_date_captain__has_lgb__fips_st_county_names_short |>
  dplyr::filter(tolower(interviewee_f_name) == tolower(CAPT_NAME_FIRST))

dim(join_by_date_captain__has_lgb__fips_st_county_names_short_same_f_name)
# 58

#### compare first names ---- 
join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty |> 
  dplyr::filter(!tolower(interviewee_f_name) == tolower(CAPT_NAME_FIRST)) |> 
# 63
  dplyr::select(interviewee_f_name, CAPT_NAME_FIRST) |> 
  dplyr::mutate(dplyr::across(tidyselect::everything(), ~tolower(.))) |> 
  dplyr::distinct() |> 
  dplyr::arrange(interviewee_f_name) |> 
  str()
# 22
# 11 are derivatives or typos

#### compare vessel names in join_by_date_captain ---- 

clean_vessel_name <- function(vessel_name) {
  vessel_name |>
    tolower() |>
    stringr::str_replace_all(" ii+", " i") |> 
    stringr::str_replace_all("\\W+", "")
}

join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name <-
  join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty |>
  dplyr::mutate(dplyr::across(c("vessel_name", "VESSEL_NAME"), 
                              ~ clean_vessel_name(.)))

join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name |> 
  dplyr::filter(tolower(vessel_name) == tolower(VESSEL_NAME)) |>
  dim()
# 67 (out of 122)

join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name__diff_vsl_names <-
  join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name |>
  dplyr::filter(!tolower(vessel_name) == tolower(VESSEL_NAME)) |>
  # dim()
  # 55
  dplyr::filter(!tolower(VESSEL_NAME) == "unnamed") 

join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name__diff_vsl_names |>
  dim()
# 33

join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name__diff_vsl_names |> 
  dplyr::select(vessel_name, VESSEL_NAME, vsl_num, VESSEL_OFFICIAL_NBR) |>
  dplyr::distinct() |>
# 23
  dplyr::rowwise() |>
  dplyr::filter(agrepl(
    vsl_num,
    tolower(VESSEL_OFFICIAL_NBR),
    ignore.case = TRUE,
    max.distance = 2
  )) |>
  dplyr::ungroup() |>
  head() |> 
  dplyr::glimpse()

diff_vsl_ids <- 
  join_by_date_captain__has_lgb__fips_st_county_names_short_same_cnty__clena_vsl_name__diff_vsl_names |>
  dplyr::select(vessel_name, VESSEL_NAME, vsl_num, VESSEL_OFFICIAL_NBR) |>
  dplyr::distinct() |>
  # 23
  dplyr::rowwise() |>
  dplyr::filter(!agrepl(
    vsl_num,
    tolower(VESSEL_OFFICIAL_NBR),
    ignore.case = TRUE,
    max.distance = 2
  )) |> 
  dplyr::ungroup()

diff_vsl_ids_pairs <-
  diff_vsl_ids |>
  dplyr::rowwise() |>
  dplyr::mutate(check_vsl_num_l = list(unique(sort(
    paste(toupper(vsl_num), toupper(VESSEL_OFFICIAL_NBR), sep = "', '")
  )))) |>
  dplyr::ungroup()

#### print out ----
# readr::write_csv(diff_vsl_ids_pairs,
#                  file.path(curr_proj_output_path, "diff_vsl_ids.csv"))

# str(diff_vsl_ids_pairs)
print_vsl_ids_to_check <-
  diff_vsl_ids_pairs |>
  dplyr::select(check_vsl_num_l) |>
  dplyr::distinct() |> 
  unlist()

# print_vsl_ids_to_check |> 
#    cat(sep = "'\n'")

#### check in db ----
# vsl_id_pair <- "FL7092NJ', '1074576"

make_a_pair_vsl_ids_query <- function(vsl_id_pair) {
  check_vsl_ids_query <-
    stringr::str_glue(
      "SELECT
  vessel_id,
  hull_id_nbr,
  vessel_name,
  coast_guard_nbr,
  state_reg_nbr,
  sero_official_number, owner_id
FROM
  safis.vessels@secapxdv_dblk.sfsc.noaa.gov
WHERE
  coast_guard_nbr IN ( '{vsl_id_pair}' )
  OR state_reg_nbr IN ( '{vsl_id_pair}' )
  OR sero_official_number IN ( '{vsl_id_pair}' )
ORDER BY
  vessel_id"
    )
  
  return(check_vsl_ids_query)
}
  
# one_query_res <-
  # try(DBI::dbGetQuery(con, check_vsl_ids_query))

# str(one_query_res)
# str(diff_vsl_ids_pairs)

vsl_ids_to_check_db <-
  print_vsl_ids_to_check |>
  purrr::map(\(vsl_id_pair) {
    # browser()
    str(vsl_id_pair)
    curr_query <- make_a_pair_vsl_ids_query(vsl_id_pair)
    one_query_res <-
      try(DBI::dbGetQuery(con, curr_query))
    return(one_query_res)
  })

names(vsl_ids_to_check_db) <- print_vsl_ids_to_check

vsl_ids_to_check_db |>
	head() |>
	dplyr::glimpse()

# check_vsl_ids_query
  # coast_guard_nbr IN ( 'FL7092NJ', '1074576' )
  # OR state_reg_nbr IN ( 'FL7092NJ', '1074576' )
  # OR sero_official_number IN ( 'FL7092NJ', '1074576' )

# TODO:
# 1) VESSEL_NAME == "unnamed"
# 2) agrep or adist for vsl_num/VESSEL_OFFICIAL_NBR, excl. 99999
# 3) check vessels with double ids

# count interviews w no logbooks 1 ----
count_interview_no_lgb <-
  function(my_df, cnt_field = "st_2") {
    my_df |>
      dplyr::group_by(fips) |>
      dplyr::mutate(num_int_no_lgb_by_fips = n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(!!sym(cnt_field)) |>
      dplyr::add_count(!!sym(cnt_field), name = "total_int_no_lgb_by_state") |>
      dplyr::ungroup()
  }

# NA states
intv_w_no_lgb_join_by_day_vsl_cnt <-
  intv_w_no_lgb_join_by_day_vsl |>
  dplyr::select(st_2, vsl_num, interview_date, fips) |>
  dplyr::distinct() |>
  count_interview_no_lgb()

# check
intv_w_no_lgb_join_by_day_vsl_cnt |>
	head() |>
	dplyr::glimpse()

intv_w_no_lgb_join_by_day_vsl_cnt |>
  dplyr::filter(st_2 == "48") |>
  dplyr::arrange(fips) |> 
  head() |> 
  dplyr::glimpse()

# restored states
intv_w_no_lgb_join_by_day_vsl_restored_cnt <-
  intv_w_no_lgb_join_by_day_vsl_restored |> 
  dplyr::select(restored_st, vsl_num, interview_date, fips) |>
  dplyr::distinct() |>
  count_interview_no_lgb(cnt_field = "restored_st")

dim(intv_w_no_lgb_join_by_day_vsl_restored_cnt)
# 827

intv_w_no_lgb_join_by_day_vsl_restored_cnt |>
  dplyr::select(restored_st, total_int_no_lgb_by_state) |>
  dplyr::distinct() |> 
  dplyr::count(wt = total_int_no_lgb_by_state)
# 827
# correct

## percent interviews w no logbooks ----
num_of_interviews_w_no_lgb <-
  nrow(intv_w_no_lgb_join_by_day_vsl_cnt)
# 827

# check
# num_of_interviews_w_no_lgb_restored <-
#   nrow(intv_w_no_lgb_join_by_day_vsl_restored_cnt)

# num_of_interviews_w_no_lgb_restored == num_of_interviews_w_no_lgb
# T

num_of_interviews <-
  nrow(survey_data_l_2022_vsl_date)
# 1835

percent_num_of_interviews_w_no_lgb__num_of_interviews <-
  num_of_interviews_w_no_lgb * 100 / num_of_interviews
# 45%

# count interviews w no logbooks and checked captain names ----
## remove same_vsls from interview_no_lgb ----
intv_w_no_lgb_join_by_day_vsl__corrected1 <-
  intv_w_no_lgb_join_by_day_vsl |> 
  left_join(same_vsls_all_fields)
# Joining with `by = join_by(id_code, vsl_num, interview_date, st, cnty)`

dim(intv_w_no_lgb_join_by_day_vsl)
# [1] 833   8
n_distinct(intv_w_no_lgb_join_by_day_vsl$vsl_num)
# 261
n_distinct(intv_w_no_lgb_join_by_day_vsl$id_code)
# [1] 833

dim(intv_w_no_lgb_join_by_day_vsl__corrected1)
# [1] 857  23
n_distinct(intv_w_no_lgb_join_by_day_vsl__corrected1$vsl_num)
# 261
n_distinct(intv_w_no_lgb_join_by_day_vsl__corrected1$id_code)
# [1] 833
n_distinct(intv_w_no_lgb_join_by_day_vsl__corrected1$VESSEL_OFFICIAL_NBR)
# 37

# Count unique values in each column:
auxfunctions::count_uniq_by_column(intv_w_no_lgb_join_by_day_vsl__corrected1)
# id_code             833
# vsl_num             261
# interview_date      198
# st                    6
# cnty                 19
# st_2                  6
# cnty_3               19
# fips                 31
# interviewee_f_name   39
# interviewee_l_name   36
# vessel_name          43
# CAPT_NAME_FIRST      35
# TRIP_ID              80
# VESSEL_OFFICIAL_NBR  37
# VESSEL_NAME          35
# STATE                 6
# STATE_NAME            6
# END_PORT_COUNTY      13
# START_PORT_COUNTY    11
# notes                22

## keep only interview_no_lgb with no same captain or owner ----
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn <-
  intv_w_no_lgb_join_by_day_vsl__corrected1 |>
  filter(is.na(interviewee_l_name)) |> 
  auxfunctions::remove_empty_cols()

dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn)
# 775 8

auxfunctions::count_uniq_by_column(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn)

# intv_w_no_lgb_join_by_day_vsl__minus_same_cptn |> View()

## restore states for no cptn ----
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states <-
  restore_states(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn)

# dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn)
# dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states)
# 775
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states |> 
  filter(cnty == "57") |> glimpse()

## add fips for no cptn restored states ----
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short <-
  intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states |>
  select(-c(st_2, cnty_3, fips, states_l_by_cnty)) |>
  distinct()

dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short)

intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips <-
  intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short |>
  format_state_and_county_codes("restored_st")

# to plot intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips

# dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips)
# 775

## add counts ----
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt <-
  intv_w_no_lgb_join_by_day_vsl__minus_same_cptn |>
  dplyr::select(st_2, vsl_num, interview_date, fips) |>
  dplyr::distinct() |>
  count_interview_no_lgb()

dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt)
# 772

### counts for restored no cptn name ----
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips_cnt <-
  intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips |>
  dplyr::select(st_2, vsl_num, interview_date, fips) |>
  dplyr::distinct() |>
  count_interview_no_lgb()

# dim(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips_cnt)
# 772

## percent of interviews no lgb, checked by captain name ----
percent_num_of_interviews_w_no_lgb__checked_cptn__num_of_interviews <-
  nrow(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt) * 100 / num_of_interviews
# 42%

# Plot interviews w no logbooks ----

## prep intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt for plotting  ----

glimpse(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt)
intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt__restore_st <-
  restore_states(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt)


# inerview_no_lgb_geo <-
# intv_w_no_lgb_join_by_day_vsl |> dplyr::glimpse()

# library(ggplot2)
plot_states <- usmap::plot_usmap(include = c(gulf_states, "FL")) 

plot_cnties_only <-
  plot_usmap(regions = "counties",
             include = c(florida_gulf_counties))

# intv_w_no_lgb_join_by_day_vsl1 <-
#   intv_w_no_lgb_join_by_day_vsl |>
#   dplyr::mutate(fips = cnty) |> 
#   dplyr::group_by(st, cnty) |> 
#   dplyr::mutate(num_int_no_lgb = n()) |> 
#   dplyr::ungroup()

# intv_w_no_lgb_join_by_day_vsl1 |>
#   dplyr::arrange(num_int_no_lgb, st, cnty) |> 
#   View()

### prep state info for plotting ----
selected_states_df <- usmap::us_map(include = c(gulf_states, "FL"))

# Get centroids
centroid_labels <- usmapdata::centroid_labels("states")

# Join centroids to data
old_names <- names(centroid_labels)

names(centroid_labels) <- c("st_2", "abbr", "full", "geom")

make_state_labels <- function(my_df, state_field_name = "st_2") {
  # browser()
  # merge(my_df, centroid_labels, by = "st_2")
  temp_df <- 
  dplyr::inner_join(my_df, 
                   centroid_labels, 
                   dplyr::join_by(!!state_field_name == st_2))
  
  temp_df |> 
    dplyr::select(!!state_field_name,
                  abbr,
                  full,
                  total_int_no_lgb_by_state,
                  geom) |>
    dplyr::distinct() |>
    dplyr::mutate(label_st_cnt = 
                    paste(abbr, total_int_no_lgb_by_state)) |> 
    dplyr::arrange(full)
}

# print_df_names(intv_w_no_lgb_join_by_day_vsl_restored_cnt)

my_dfs_to_plot <- list(
  Hmisc::llist(intv_w_no_lgb_join_by_day_vsl_cnt, "st_2"),
  Hmisc::llist(intv_w_no_lgb_join_by_day_vsl_restored_cnt, "restored_st"),
  Hmisc::llist(intv_w_no_lgb_join_by_day_vsl__minus_same_cptn_cnt, "st_2"),
  Hmisc::llist(
    intv_w_no_lgb_join_by_day_vsl__minus_same_cptn__restored_states_short__fips_cnt,
    "st_2"
  )
)

my_dfs_to_plot_w_labels <-
  my_dfs_to_plot |>
  purrr::map(\(x)
    make_state_labels(x[[1]], x[[2]][[1]]))

#### add names back ----
names(my_dfs_to_plot_w_labels) <- 
  purrr::map(my_dfs_to_plot, names) |> purrr::map(1)

my_dfs_to_plot_w_labels[[4]] |> glimpse()

### interview w no lgb plot ----

plot_counties <- function(my_df) {
  usmap::plot_usmap(
    regions = "counties",
    include = c(gulf_states, "FL"),
    data = my_df,
    values = "num_int_no_lgb_by_fips",
    color = "lightgrey"
  ) +
    ggplot2::scale_fill_gradient(
      name = "Interviews w/o lgbks",
      high = "blue",
      low = "yellow",
      na.value = "transparent"
      # ,
      # guide = guide_legend()
    ) +
    ggplot2::theme(legend.position = "right",
                   # legend.position = c(.95, .95),
                   # legend.justification = c("right", "top"),
                   # legend.box.just = "right",
                   legend.margin = ggplot2::margin(0, 0, 0, 0)
    )
          # ,
          # legend.spacing.x = unit(0, 'cm'))
  # +
    # guides(fill = guide_legend(label.position = "bottom"))
}

# print_df_names(intv_w_no_lgb_join_by_day_vsl_cnt)

plot_cnties <- plot_counties(intv_w_no_lgb_join_by_day_vsl_cnt)

plot_cnties_restored <- 
  plot_counties(intv_w_no_lgb_join_by_day_vsl_restored_cnt)

# check
no_state_interview_no_lgb_num <- 
  intv_w_no_lgb_join_by_day_vsl_cnt |>
  dplyr::filter(st_2 == "00") |> 
  dplyr::select(total_int_no_lgb_by_state) |> 
  dplyr::distinct()
# 192

add_state_labels <-
  function(usmap_plot, labels_by_state = state_labels_short) {
    usmap_plot +
      ggplot2::geom_sf_text(data = labels_by_state, 
                            ggplot2::aes(geometry = geom, label = label_st_cnt)) +
      ggplot2::geom_sf(data = selected_states_df,
              color = "green",
              fill = NA)
  }

plot_restored_all <- 
  add_state_labels(plot_cnties_restored, state_labels_restored) +
  ggplot2::labs(title = "Number of interviews without logbooks by state/county")

plot_cnties_state_lbls <-
  add_state_labels(plot_cnties, state_labels_short) +
  ggplot2::labs(
    title = "Number of interviews without logbooks by state/county",
    caption = stringr::str_glue(
      "Number of interviews without logbooks with no state info is {no_state_interview_no_lgb_num$total_int_no_lgb_by_state}."
    )
  )

# library(gridExtra)
#### interview wo lgb plot show ----
# #| column: screen
#| out-width: 100%

gridExtra::grid.arrange(plot_cnties_state_lbls,
           plot_restored_all)

