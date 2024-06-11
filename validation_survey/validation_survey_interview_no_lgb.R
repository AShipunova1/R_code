# how many interviews with no logbooks?

# Prep data for interviews w no logbooks ----
## prep lgb info ----
db_logbooks_2022_vsl_t_end <-
  db_logbooks_2022 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_END_DATE, TRIP_ID) |>
  dplyr::mutate(VESSEL_OFFICIAL_NBR = tolower(VESSEL_OFFICIAL_NBR)) |>
  dplyr::mutate(TRIP_END_DATE = lubridate::date(TRIP_END_DATE)) |>
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
survey_data_l_2022_date_i1_vsl__int_t <-
  survey_data_l_2022_i1_w_dates |>
  dplyr::select(id_code, vsl_num, interview_date, st, cnty) |>
  dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, " ", "")) |>
  dplyr::mutate(vsl_num = stringr::str_replace_all(vsl_num, "-", "")) |>
  dplyr::mutate(vsl_num = tolower(vsl_num)) |>
  dplyr::mutate(interview_date = lubridate::date(interview_date)) |>
  dplyr::distinct()

glimpse(survey_data_l_2022_date_i1_vsl__int_t)
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
dplyr::glimpse(survey_data_l_2022_i1_w_dates__states_by_cnty)

dplyr::glimpse(survey_data_l_2022_date_i1_vsl__int_t)

survey_data_l_2022_date_i1_vsl__int_t__join_states_by_cnty <-
  survey_data_l_2022_date_i1_vsl__int_t |> 
  dplyr::left_join(survey_data_l_2022_i1_w_dates__states_by_cnty,
            dplyr::join_by(cnty, st))
  
str(survey_data_l_2022_date_i1_vsl__int_t__join_states_by_cnty)
# tibble [1,812 × 8] (S3: tbl_df/tbl/data.frame)
#  $ vsl_num         : chr [1:1812]
#  $ interview_date  : Date[1:1812], format: "2022-01-30" "2022-02-14" ...
#  $ st              : chr [1:1812] NA NA NA NA ...
#  $ cnty            : int [1:1812] 17 75 75 115 115 115 115 81 75 75 ...
#  $ st_2            : chr [1:1812] "00" "00" "00" "00" ...
#  $ cnty_3          : chr [1:1812] "017" "075" "075" "115" ...
#  $ fips            : chr [1:1812] "00017" "00075" "00075" "00115" ...
#  $ states_l_by_cnty:List of 1812
#   ..$ : chr [1:2] "12" "NA"

survey_data_l_2022_date_i1_vsl__int_t__restore_st <-
  survey_data_l_2022_date_i1_vsl__int_t__join_states_by_cnty |>
  dplyr::rowwise() |>
  dplyr::mutate(temp_res =
                  case_when(is.na(st) ~ paste(unlist(states_l_by_cnty),
                                              collapse = ""), 
                            .default = st)) |>
  dplyr::mutate(restored_st = 
                  stringr::str_extract(temp_res, "\\d+")) |>
  dplyr::select(-temp_res) |>
  dplyr::ungroup()

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
      mutate(st_2 =
               case_when(is.na(!!sym(state_code_field)) ~ "00", .default =
                           stringr::str_pad(!!sym(state_code_field), 2, pad = "0"))) |>
      mutate(cnty_3 = stringr::str_pad(cnty, 3, pad = "0"),
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

dplyr::glimpse(intv_w_no_lgb_join_by_day_vsl)
dplyr::glimpse(intv_w_no_lgb_join_by_day_vsl_restored)

## check all vessel ids not in lgb ----
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

## check if these interviews are for DNFs ----
dplyr::glimpse(db_dnfs_2022)

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
# glimpse()

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
  dplyr::glimpse()

# there are no trips with both ("167759", "167763")

## spot check the interviews by captain name ----
# TODO
# 1) also suggest using captain's name - to try to match, if that is a field in both. like instead of just trying to match by vessel ID.

## spot check the interviews by time window ----
# TODO
# 2)
# And if you limit to a smaller window (e.g. end or start in logbook within 1 hour of the survey, or within 2, or within 3 hours) how does that % come out?

# count interviews w no logbooks 1 ----
count_interview_no_lgb <-
  function(my_df, cnt_field = "st_2") {
    my_df |>
      group_by(fips) |>
      mutate(num_int_no_lgb_by_fips = n()) |>
      ungroup() |>
      group_by(!!sym(cnt_field)) |>
      add_count(!!sym(cnt_field), name = "total_int_no_lgb_by_state") |>
      ungroup()
  }

# NA states
intv_w_no_lgb_join_by_day_vsl_cnt <-
  intv_w_no_lgb_join_by_day_vsl |>
  select(st_2, vsl_num, interview_date, fips) |>
  distinct() |>
  count_interview_no_lgb()

# check
glimpse(intv_w_no_lgb_join_by_day_vsl_cnt)

intv_w_no_lgb_join_by_day_vsl_cnt |>
  filter(st_2 == "48") |>
  arrange(fips) |> 
  glimpse()

# restored states
intv_w_no_lgb_join_by_day_vsl_restored_cnt <-
  intv_w_no_lgb_join_by_day_vsl_restored |> 
  select(restored_st, vsl_num, interview_date, fips) |>
  distinct() |>
  count_interview_no_lgb(cnt_field = "restored_st")

dim(intv_w_no_lgb_join_by_day_vsl_restored_cnt)
# 827

intv_w_no_lgb_join_by_day_vsl_restored_cnt |>
  select(restored_st, total_int_no_lgb_by_state) |>
  distinct() |> 
  count(wt = total_int_no_lgb_by_state)
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

num_of_interviews_w_no_lgb * 100 / num_of_interviews
# 45%

# Plot interviews w no logbooks ----

# inerview_no_lgb_geo <-
# intv_w_no_lgb_join_by_day_vsl |> glimpse()

# library(ggplot2)
plot_states <- plot_usmap(include = c(gulf_states, "FL")) 

plot_cnties_only <-
  plot_usmap(regions = "counties",
             include = c(florida_gulf_counties))

# intv_w_no_lgb_join_by_day_vsl1 <-
#   intv_w_no_lgb_join_by_day_vsl |>
#   mutate(fips = cnty) |> 
#   group_by(st, cnty) |> 
#   mutate(num_int_no_lgb = n()) |> 
#   ungroup()

# intv_w_no_lgb_join_by_day_vsl1 |>
#   arrange(num_int_no_lgb, st, cnty) |> 
#   View()

### prep state info for plotting ----
selected_states_df <- usmap::us_map(include = c(gulf_states, "FL"))

# Get centroids
centroid_labels <- usmapdata::centroid_labels("states")

# Join centroids to data
old_names <- names(centroid_labels)

names(centroid_labels) <- c("st_2", "abbr", "full", "geom")

state_labels <-
  merge(intv_w_no_lgb_join_by_day_vsl_cnt, 
        centroid_labels, 
        by = "st_2")

state_labels_restored <-
  dplyr::left_join(
    intv_w_no_lgb_join_by_day_vsl_restored_cnt,
    centroid_labels,
    join_by(restored_st == st_2)
  ) |>
  mutate(label_st_cnt = paste(abbr, total_int_no_lgb_by_state))

glimpse(state_labels_restored)

state_labels_short <-
  state_labels |>
  select(st_2, abbr, full, total_int_no_lgb_by_state, geom) |> 
  distinct() |> 
  mutate(label_st_cnt = paste(abbr, total_int_no_lgb_by_state))

glimpse(state_labels_short)
# 5

### interview wo lgb plot ----

plot_counties <- function(my_df) {
  plot_usmap(
    regions = "counties",
    include = c(gulf_states, "FL"),
    data = my_df,
    values = "num_int_no_lgb_by_fips",
    color = "lightgrey"
  ) +
    ggplot2::scale_fill_gradient(
      name = "Interviews w/o logbooks",
      low = "blue",
      high = "yellow",
      na.value = "transparent"
    )
}

# print_df_names(intv_w_no_lgb_join_by_day_vsl_cnt)

plot_cnties <- plot_counties(intv_w_no_lgb_join_by_day_vsl_cnt)

plot_cnties_restored <- 
  plot_counties(intv_w_no_lgb_join_by_day_vsl_restored_cnt)

# check
no_state_interview_no_lgb_num <- 
  intv_w_no_lgb_join_by_day_vsl_cnt |>
  filter(st_2 == "00") |> 
  select(total_int_no_lgb_by_state) |> 
  distinct()
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

gridExtra::grid.arrange(plot_cnties_state_lbls,
           plot_restored_all)

