# Set up ----

library('devtools')

if (!require('auxfunctions')) {
  devtools::install_github("AShipunova1/R_code/auxfunctions@development")
  
  library('auxfunctions')
}

library(lubridate)

Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

my_paths <- auxfunctions::set_work_dir()

# get this project name
current_project_dir_name <- this.path::this.dir()

# find its base name
current_project_name <-
  basename(current_project_dir_name)

# use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
get_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_path)

source(get_data_path)

# Data are in:
# survey_data_l_2022
# processed_logbooks_2022
# processed_logbooks_2022_calendar
# db_logbooks_2022
# db_dnfs_2022
# ---

# prepare data ----

prepare_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "prepare_data.R"))

file.exists(prepare_data_path)

source(prepare_data_path)
# Processed Data are in:
# lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short
# db_logbooks_2022_short
# catch_info_lgb_i1_i2_i3
# ---

glimpse(lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short)
glimpse(db_logbooks_2022_short)
glimpse(catch_info_lgb_i1_i2_i3)

# db_logbooks_2022_short$ANYTHING_CAUGHT_FLAG |> unique()
# [1] NA  "Y" "N"

# plots ----
# source(file.path(current_project_dir_name, "validation_survey_plots.R"))

# compare field names ----
# source(file.path(current_project_dir_name, "validation_survey_fields.R"))

# compare vessel names ----
unify_names <- function(column_name) {
  tolower(column_name) |> 
    stringr::str_replace("\\s", "")
}

catch_info_lgb_i1_i2_i3 |>
  filter(!unify_names(vessel_name) == unify_names(VESSEL_NAME)) |>
  select(VESSEL_OFFICIAL_NBR, vsl_num, VESSEL_NAME, vessel_name) |>
  distinct() |>
  glimpse()
# Rows: 101 w/o spaces
# Rows: 112
# select(VESSEL_OFFICIAL_NBR) |> 
# 79 unique vessels

# filter(!VESSEL_OFFICIAL_NBR == vsl_num)
# 0

# compare TRIP_TYPE_NAME, operating_type ----

# 6=’HB’, 7=’CB’, 0=’Neither’
# catch_info_lgb_i1_i2_i3$TRIP_TYPE_NAME |> unique()
# [1] "CHARTER" "UNKNOWN"

catch_info_lgb_i1_i2_i3 |> 
  select(VESSEL_OFFICIAL_NBR, TRIP_TYPE_NAME, operating_type) |>
  distinct() |>
  mutate(surv_trip_type = case_when(operating_type == 6 ~ "headboat",
                                    operating_type == 7 ~ "CHARTER",
                                    operating_type == 0 ~ "Neither")) |> 
  filter(!unify_names(TRIP_TYPE_NAME) == unify_names(surv_trip_type)) |>
  glimpse()
# 16
# same type "CHARTER" 225

# compare NUM_ANGLERS, people_fishing ----
# catch_info_lgb_i1_i2_i3$people_fishing |> unique()

compare_fields <- 
  c("NUM_ANGLERS", "people_fishing")

catch_info_lgb_i1_i2_i3 |> 
  select(VESSEL_OFFICIAL_NBR, all_of(compare_fields)) |>
  distinct() |>
  rowwise() |> 
  # filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  # 135
  # filter(as.integer(!!sym(compare_fields[[1]])) > as.integer(!!sym(compare_fields[[2]]))) |>
  # 90
  # filter(as.integer(!!sym(compare_fields[[1]])) < as.integer(!!sym(compare_fields[[2]]))) |>
  # 45
  filter(abs(as.integer(!!sym(compare_fields[[1]])) - as.integer(!!sym(compare_fields[[2]]))) > 1) |>
  ungroup() |> 
# 68
  dim()

# when was the survey?, all < June 2022
# grep("month", tolower(names(catch_info_lgb_i1_i2_i3)), value = T)

fish_hours_diff <-
  catch_info_lgb_i1_i2_i3 |>
  select(VESSEL_OFFICIAL_NBR,
         all_of(compare_fields),
         interview_date_time) |>
  distinct() |>
  rowwise() |>
  filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  ungroup() |>
  mutate(
    interview_year = lubridate::year(interview_date_time),
    interview_month = lubridate::month(interview_date_time)
  ) |>
  count(interview_month, name = "diff_fishing_hours")

str(fish_hours_diff)

fish_hours_diff_plot <-
  ggplot2::ggplot(data = fish_hours_diff, ggplot2::aes(x = interview_month, y = diff_fishing_hours)) +
  ggplot2::geom_point(color = "blue") +
  ggplot2::geom_line(color = "blue") +
  # Change the x axis name
  ggplot2::scale_x_discrete(name = "Interview Month", limits = factor(seq_len(12))) +
  ggplot2::labs(title = "Trips with fishing hours different between logbooks and survey ny month", y = "Number of Trips with difference") +
  ggplot2::geom_text(
    label = fish_hours_diff$diff_fishing_hours,
    nudge_y = 0.5,
    nudge_x = 0.4
  )

# compare ACTIVITY_TYPE_NAME	no_harvested_selected

# ACTIVITY_TYPE_NAME
# [1] "TRIP WITH EFFORT"
# catch_info_lgb_i1_i2_i3$no_harvested_selected |> unique()
# 1, 2 (1=YES, 2=NO)

compare_fields <- 
  c("ACTIVITY_TYPE_NAME", "no_harvested_selected")

catch_info_lgb_i1_i2_i3 |> 
    filter(is.na(CATCH_SPECIES_ITIS)) |> glimpse()
# [1] 145  50

catch_info_lgb_i1_i2_i3 |>
  select(all_of(compare_fields)) |>
  distinct() |>
  glimpse()
# $ ACTIVITY_TYPE_NAME    <chr> "TRIP WITH EFFORT", "TRIP WITH EFFORT"
# $ no_harvested_selected <int> 2, 1

# compare DISTANCE_CODE_NAME	fishing_distance ----

compare_fields <-
  c("DISTANCE_CODE_NAME", "fishing_distance")

catch_info_lgb_i1_i2_i3 |>
  select(all_of(compare_fields)) |>
  distinct() |>
  glimpse()

# compare FISHING_HOURS	hrsf ----

compare_fields <-
  c("FISHING_HOURS", "hrsf")

catch_info_lgb_i1_i2_i3 |>
  select(VESSEL_OFFICIAL_NBR, all_of(compare_fields)) |>
  distinct() |>
  # dim()
  # 674
  rowwise() |> 
  filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  ungroup() |> 
  glimpse()
# 367

# TODO: check if the same trip

# compare CATCH_SPECIES_ITIS	tsn ----
# https://en.wikipedia.org/wiki/Integrated_Taxonomic_Information_System
# grep("tsn", names(catch_info_lgb_i1_i2_i3), value = T)
# [1] "tsn.releas" "tsn.harv" 

compare_fields <-
  c("CATCH_SPECIES_ITIS", "tsn.releas", "tsn.harv")

catch_info_lgb_i1_i2_i3 |> 
  select(starts_with("tsn")) |> 
  distinct() |> 
  # dim()
# 1026
  filter(!tsn.releas == tsn.harv) |> 
  glimpse()
# 937

# n_distinct(catch_info_lgb_i1_i2_i3$TRIP_ID)
# 887

catch_info_lgb_i1_i2_i3 |> 
  # select(all_of(compare_fields)) |>
  select(VESSEL_OFFICIAL_NBR, TRIP_ID, all_of(compare_fields)) |>
  distinct() |>
  # dim()
  # [1] 27634     5
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  # filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
# Rows: 23,108
  # filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]]))) |>
# Rows: 23,112
  # filter(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]])) &
  #          !(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]])))) |>
  filter(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]])) &
           !(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]])))) |>
  ungroup() |> 
  glimpse()

# get scientific and common names by tsn ----

tsn_only <-
  catch_info_lgb_i1_i2_i3 |>
  distinct(CATCH_SPECIES_ITIS, tsn.releas, tsn.harv) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

tsn_only <- data.frame(tsn = unique(unlist(tsn_only)))
# 'data.frame':	155 obs. of  1 variable

# good_tsn <- 172435 
# no_com_n_tsn <- 168790
# tsn <- good_tsn

get_itis_info <- function(tsn_s) {
  tsn_s |>
    purrr::map(\(tsn) {
      # browser()
      
      res <-
        tryCatch(
          ritis::full_record(tsn),
          error = function(e) {
            print(tsn)
            print(e)
          }
        )
      
      if (!is.list(res))
        return()
      
      commonNames <- res$commonNameList$commonNames
      
      if (is.data.frame(commonNames)) {
        com_name <-
          commonNames |>
          filter(language == "English") |>
          select(commonName) |>
          unlist() |>
          unname()
      }
      else {
        com_name <- ""
      }
      
      sci_name <-
        res$scientificName$combinedName
      
      res_list <-
        list("sci_name" = sci_name, "com_name" = com_name)
      
      return(res_list)
    })
}

tsn_info_itis_file_path <- 
  file.path(curr_proj_output_path,
            paste0("tsn_info_itis.rds"))

tictoc::tic()
if (file.exists(tsn_info_itis_file_path)) {
  tsn_info_itis <- readr::read_rds(tsn_info_itis_file_path)
} else {
  tsn_info_itis <-
    tsn_only |>
    dplyr::mutate(tsn_com = get_itis_info(tsn))
  readr::write_rds(tsn_info_itis, tsn_info_itis_file_path)
}
tictoc::toc()
# 26.21 sec elapsed
# errors: [1] NA
# [1] 0

glimpse(tsn_info_itis[[2]])

## check released and harvested separately ----
# "CATCH_SPECIES_ITIS" vs "tsn.harv"

CATCH_SPECIES_ITIS_vs_tsn.harv <-
  catch_info_lgb_i1_i2_i3 |>
  select(VESSEL_OFFICIAL_NBR, TRIP_ID, all_of(compare_fields)) |>
  distinct() |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  mutate(all_spp_1_trip_fhier_l =
           list(sort(unique(CATCH_SPECIES_ITIS)))) |>
  mutate(all_spp_1_trip_survey_l =
           list(sort(unique(tsn.harv)))) |> 
  ungroup()

cathc_spp_diff <-
  CATCH_SPECIES_ITIS_vs_tsn.harv |>
  select(-all_of(compare_fields)) |>
  distinct() |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::mutate(
    harv_diff_in_fhier_only =
      purrr::map2(
        all_spp_1_trip_fhier_l,
        all_spp_1_trip_survey_l,
        ~ setdiff(.x, .y)
      )
  ) |>
  dplyr::mutate(
    harv_diff_in_survey_only =
      purrr::map2(
        all_spp_1_trip_fhier_l,
        all_spp_1_trip_survey_l,
        ~ setdiff(.y, .x)
      )
  ) |> 
  ungroup()
# 887  

cathc_spp_diff__no_diff <-
  cathc_spp_diff |>
  rowwise() |>
  mutate(
    ll_f =
      length(harv_diff_in_fhier_only),
    ll_s =
      length(harv_diff_in_survey_only)
  ) |>
  mutate(no_diff_spp =
           case_when((ll_f == ll_s &
                        ll_f == 0) ~ "no_diff", 
                     .default = "is_diff")) |>
  ungroup()

glimpse(cathc_spp_diff__no_diff)

# TODO: check released and harvested separately

## check numbers for the same spp lgb/harvested ----
catch_info_lgb_i1_i2_i3 |> 
  # select(VESSEL_OFFICIAL_NBR, TRIP_ID, all_of(compare_fields)) |>
  # distinct() |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  ungroup() |> 
  glimpse()

## check numbers for the same spp lgb/released ----
catch_info_lgb_i1_i2_i3 |> 
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[2]]))) |>
  ungroup() |> 
  glimpse()

# the same sp. is both released and harvested in the same trip (OK) ----

catch_info_lgb_i1_i2_i3 |> 
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[2]]))) |>
  filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  ungroup() |> 
  glimpse()

# TODO: compare and remove if duplicate all fields with "."

## separate vessel_trip info from catch ----
vessel_trip_fields <- c(
  "TRIP_ID",
  "VESSEL_OFFICIAL_NBR",
  "trip_end_date_time",
  "interview_date_time",
  "TRIP_TYPE_NAME",
  "VESSEL_NAME",
  "CAPT_NAME_FIRST",
  "CAPT_NAME_LAST",
  "STATE",
  "STATE_NAME",
  "END_PORT_NAME",
  "END_PORT_COUNTY",
  "END_PORT_STATE",
  "NUM_ANGLERS",
  "ACTIVITY_TYPE_NAME",
  "DISTANCE_CODE_NAME",
  "FISHING_HOURS",
  "operating_type",
  "vsl_num",
  "vessel_name",
  "interviewee_f_name",
  "interviewee_l_name",
  "st.i1",
  "cnty",
  "people_fishing",
  "no_harvested_selected",
  "fishing_distance",
  "hrsf",
  "st.releas",
  "i2",
  "st"
)

# "UNIT_MEASURE", (all "CN")

catch_fields <-
  c(
    "CATCH_SEQ",
    "CATCH_SPECIES_ITIS",
    "REPORTED_QUANTITY",
    "DISPOSITION_CODE",
    "DISPOSITION_NAME",
    "num_typ2.i1",
    "num_typ3.releas",
    "tsn.releas",
    "num_fish",
    "num_typ2.releas",
    "num_typ2.i1",
    "num_typ3.releas",
    "tsn.releas",
    "num_fish",
    "num_typ2.releas",
    "tsn.harv",
    "fshinsp"
  )

# dim(catch_info_lgb_i1_i2_i3)

catch_info_lgb_i1_i2_i3_short <- 
  catch_info_lgb_i1_i2_i3 |> 
  select("TRIP_ID",
  "VESSEL_OFFICIAL_NBR",
  "id_code",
  all_of(catch_fields)
) |> 
  distinct()

dim(catch_info_lgb_i1_i2_i3)
# [1] 89466    50

dim(catch_info_lgb_i1_i2_i3_short)
# [1] 37506    15

# look at catch_info_lgb_i1_i2_i3_short_harvested ----
catch_info_lgb_i1_i2_i3_short_harvested <-
  catch_info_lgb_i1_i2_i3_short |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  ungroup() |> 
  select(-ends_with(".releas")) |>
  distinct()

dim(catch_info_lgb_i1_i2_i3_short_harvested)
# [1] 6469   15
# [1] 5829   11 (no ".releas")

# data_overview(catch_info_lgb_i1_i2_i3_short_harvested)
# TRIP_ID              841
# VESSEL_OFFICIAL_NBR  219

# View(catch_info_lgb_i1_i2_i3_short_harvested)

# catch_info_lgb_i1_i2_i3_short_harvested |> 
    # filter(TRIP_ID == "1000020436") |> distinct() |>  View()

# count amount of species per trip ----
# db_logbooks_2022_short |> print_df_names()
# survey_data_l_2022_short |> print_df_names()

db_logbooks_2022_short_cnt_spp <-
  db_logbooks_2022_short |>
  select(
    VESSEL_OFFICIAL_NBR,
    TRIP_ID,
    DISPOSITION_CODE,
    DISPOSITION_NAME,
    CATCH_SPECIES_ITIS
  ) |>
  distinct() |>
  add_count(VESSEL_OFFICIAL_NBR, 
            TRIP_ID, 
            DISPOSITION_CODE, 
            name = "n_CATCH_SPECIES_ITIS__by_disp") |>
  group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::mutate(n_CATCH_SPECIES_ITIS = n_distinct(CATCH_SPECIES_ITIS)) |>
  ungroup()

db_logbooks_2022_short_cnt_spp |> 
  arrange(VESSEL_OFFICIAL_NBR, TRIP_ID, DISPOSITION_CODE) |> 
  glimpse()

# db_logbooks_2022_short_cnt_spp |> 
#   filter(TRIP_ID == "61422515") |> View()

# count amount of species per interview ----
# survey_data_l_2022_short

survey_data_l_2022_short_cnt_spp <- 
  survey_data_l_2022_short$i3 |>
  select(id_code, tsn, fshinsp, disp3, lngth, wgt, num_typ3) |> 
  distinct() |>
  group_by(id_code, disp3) |> 
  dplyr::mutate(n_tsn_by_disp3 = n_distinct(tsn)) |> 
  ungroup() |> 
  group_by(id_code) |> 
  dplyr::mutate(n_tsn = n_distinct(tsn)) |> 
  ungroup()

glimpse(survey_data_l_2022_short_cnt_spp)

survey_data_l_2022_short_cnt_spp |> 
    filter(id_code == "1590520220121004") |> 
    select(-c(lngth, wgt)) |> 
    distinct() |> 
    arrange(tsn) |> 
    glimpse()

# TODO: compare ns

# compare NUM_TYP3 with number of cought and retained fish in FHIER
compare_fields <-
  c("num_typ3.harv", "REPORTED_QUANTITY")

catch_info_lgb_i1_i2_i3 |>
  select(VESSEL_OFFICIAL_NBR, 
         TRIP_ID,
         all_of(compare_fields)) |>
  # arrange(VESSEL_OFFICIAL_NBR,
  #         TRIP_ID,
  #         num_typ3.harv) |> 
  distinct() |>
  # dim()
  # [1] 19124     3
  rowwise() |> 
  filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  ungroup() |> 
  glimpse()

# compare fshinsp, REPORTED_QUANTITY ----
compare_fields <-
  c("fshinsp", "REPORTED_QUANTITY")

catch_info_lgb_i1_i2_i3 |>
  select(VESSEL_OFFICIAL_NBR, 
         TRIP_ID,
         all_of(compare_fields)) |>
  arrange(VESSEL_OFFICIAL_NBR,
          TRIP_ID,
          fshinsp) |>
  distinct() |>
  rowwise() |> 
  filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  ungroup() |> 
  glimpse()

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

## full join ----
lgb_join_i1_full <-
  dplyr::full_join(
    survey_data_l_2022_date_i1_vsl_int_t,
    db_logbooks_2022_vsl_t_end,
    join_by(
      vsl_num == VESSEL_OFFICIAL_NBR,
      interview_date == TRIP_END_DATE
    ),
    relationship = "many-to-many"
  )

dim(lgb_join_i1_full)
# [1] 95697     3

intv_w_no_lgb_join_by_day_vsl <- 
  lgb_join_i1_full |> 
  filter(is.na(TRIP_ID)) |> 
  auxfunctions::remove_empty_cols() |> 
  distinct()

dim(intv_w_no_lgb_join_by_day_vsl)
# [1] 827   2

glimpse(intv_w_no_lgb_join_by_day_vsl)

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
  arrange(vsl_num,
          interview_date) |> 
  filter(vsl_num == "1041849") |> 
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

one_check_res$TRIP_END_DATE |> 
  unique() |> 
  sort() |> 
  print()

### check the interviews ----

test1_tsns <-
  survey_i1_i3_harvested_dates |>
  filter(vsl_num == '1041849' &
           interview_date == lubridate::ymd('2022-06-06')) |>
  distinct() |>
  select(tsn) |>
  distinct()
# glimpse()

test1_logbooks <-
  db_logbooks_2022 |>
  filter(
    lubridate::month(TRIP_END_DATE) == 6 &
      CATCH_SPECIES_ITIS %in% tsns1$tsn &
      VESSEL_OFFICIAL_NBR == "1041849"
  ) |> select(TRIP_ID, CATCH_SPECIES_ITIS, TRIP_END_DATE) |> distinct()

unique(test1_logbooks$CATCH_SPECIES_ITIS)
# only one out of 3

test1_logbooks_no_month <-
  db_logbooks_2022 |>
  filter(
    # lubridate::month(TRIP_END_DATE) == 6 &
      CATCH_SPECIES_ITIS %in% tsns1$tsn &
      VESSEL_OFFICIAL_NBR == "1041849"
  ) |> select(TRIP_ID, CATCH_SPECIES_ITIS, TRIP_END_DATE) |> distinct()

test1_logbooks_no_month |> 
  count(CATCH_SPECIES_ITIS)
#   CATCH_SPECIES_ITIS  n
# 1             167759  2
# 2             167763  1
# 3             168853 27

test1_logbooks_no_month_2spp <- 
  test1_logbooks_no_month |> 
  filter(CATCH_SPECIES_ITIS %in% c("167759", "167763"))

db_logbooks_2022 |> 
    filter(TRIP_ID %in% c("62538162", "63569515", "62538257")) |> 
  glimpse()
    
# one vessel only has those 2 spp
db_logbooks_2022 |>
  filter(TRIP_ID %in% c("62538162", "63569515", "62538257")) |>
  select(
    TRIP_ID,
    TRIP_END_DATE,
    CATCH_SPECIES_ITIS,
    COMMON_NAME,
    REPORTED_QUANTITY,
    DISPOSITION_NAME
  ) |> 
  distinct() |>
  group_by(TRIP_ID) |>
  mutate(all_spp = list(sort(paste(
    unique(CATCH_SPECIES_ITIS)
  ))),
  spp_cnt = length(all_spp)) |>
  ungroup() |>
  arrange(TRIP_END_DATE) |>
  glimpse()

# there is no a trip with both ("167759", "167763")

## percent interviews w no logbooks ----
num_of_interviews_w_no_lgb <-
  nrow(intv_w_no_lgb_join_by_day_vsl)
# 827

num_of_interviews <-
  nrow(survey_data_l_2022_vsl_date)
# 1835

num_of_interviews_w_no_lgb * 100 / num_of_interviews
# 45%

# survey time difference  vs trip start/trip end ----
