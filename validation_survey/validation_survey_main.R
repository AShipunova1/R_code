#' title: Validation Survey and Logbooks Comparison
#' 
# Set up ----

library('devtools')

if (!require('auxfunctions')) {
  devtools::install_github("AShipunova1/R_code/auxfunctions@development")
  
  library('auxfunctions')
}

library(lubridate)
library(ROracle)
library(tidycensus)
library(usmap)

Sys.setenv(TZ = Sys.timezone())
Sys.setenv(ORA_SDTZ = Sys.timezone())

my_paths <- auxfunctions::set_work_dir()

#' get this project name
current_project_dir_name <- this.path::this.dir()

#' find its base name
current_project_name <-
  basename(current_project_dir_name)

#' use current_project_name to create input and output path
curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_input_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_name)

auxfunctions::create_dir_if_not(curr_proj_output_path)

# get data ----
#' %%%%% Get data

get_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "get_data.R"))

file.exists(get_data_path)

source(get_data_path)

#' Data are in:
#' 
#' survey_data_l_2022
#' 
#' processed_logbooks_2022
#' 
#' processed_logbooks_2022_calendar
#' 
#' db_logbooks_2022
#' 
#' db_dnfs_2022
#' 
#' permit_info_from_db
#' 
#' permits_from_pims__split1_short__split2
#' 
#' vessels_from_pims_double_bind
#' 
#' vessel_permit_owner_from_db
#' 
# ---

# source prepare data ----
#' %%%%% Prepare data
prepare_data_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "prepare_data.R"))

file.exists(prepare_data_path)

source(prepare_data_path)
#' Processed Data are in:
#' 
#' lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short
#' 
#' db_logbooks_2022_short
#' 
#' catch_info_lgb_i1_i2_i3
#' 
# ---

lgb_join_i1__t_diff_short__w_int_all_dup_rm__int_dup_rm_short |>
 head() |>
 dplyr::glimpse()

db_logbooks_2022_short |>
 head() |>
 dplyr::glimpse()

catch_info_lgb_i1_i2_i3 |>
 head() |>
 dplyr::glimpse()

# db_logbooks_2022_short$ANYTHING_CAUGHT_FLAG |> unique()
# [1] NA  "Y" "N"

# plots ----
# source(file.path(current_project_dir_name, "validation_survey_plots.R"))

# compare field names ----
# source(file.path(current_project_dir_name, "validation_survey_fields.R"))

# how many interviews with no logbooks ----
#' %%%%% Interviews With No Logbooks
interview_no_lgb_path <-
  file.path(current_project_dir_name,
            paste0(current_project_name, "_", "interview_no_lgb.R"))

file.exists(interview_no_lgb_path)

source(interview_no_lgb_path)

#' %%%%% Other Fields Comparison

# compare vessel names main ----
unify_names <- function(column_name) {
  tolower(column_name) |> 
    stringr::str_replace("\\s", "")
}

catch_info_lgb_i1_i2_i3 |>
  dplyr::filter(!unify_names(vessel_name) == unify_names(VESSEL_NAME)) |>
  dplyr::select(VESSEL_OFFICIAL_NBR, vsl_num, VESSEL_NAME, vessel_name) |>
  dplyr::distinct() |>
  head() |> 
  dplyr::glimpse()
# Rows: 101 w/o spaces
# Rows: 112
# dplyr::select(VESSEL_OFFICIAL_NBR) |> 
# 79 unique vessels

# dplyr::filter(!VESSEL_OFFICIAL_NBR == vsl_num)
# 0

# compare TRIP_TYPE_NAME, operating_type ----

#' 6=’HB’, 7=’CB’, 0=’Neither’
#' 
# catch_info_lgb_i1_i2_i3$TRIP_TYPE_NAME |> unique()
# [1] "CHARTER" "UNKNOWN"

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_TYPE_NAME, operating_type) |>
  dplyr::distinct() |>
  dplyr::mutate(
    surv_trip_type =
      dplyr::case_when(
        operating_type == 6 ~ "headboat",
        operating_type == 7 ~ "CHARTER",
        operating_type == 0 ~ "Neither"
      )
  ) |>
  dplyr::filter(!unify_names(TRIP_TYPE_NAME) == unify_names(surv_trip_type)) |>
  head() |> 
  dplyr::glimpse()
# 16
#' same type "CHARTER" 225
#' 

# compare NUM_ANGLERS, people_fishing ----
# catch_info_lgb_i1_i2_i3$people_fishing |> unique()

compare_fields <- 
  c("NUM_ANGLERS", "people_fishing")

catch_info_lgb_i1_i2_i3 |> 
  dplyr::select(VESSEL_OFFICIAL_NBR, all_of(compare_fields)) |>
  dplyr::distinct() |>
  dplyr::rowwise() |> 
  # dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  # 135
  # dplyr::filter(as.integer(!!sym(compare_fields[[1]])) > as.integer(!!sym(compare_fields[[2]]))) |>
  # 90
  # dplyr::filter(as.integer(!!sym(compare_fields[[1]])) < as.integer(!!sym(compare_fields[[2]]))) |>
  # 45
  dplyr::filter(abs(as.integer(!!sym(compare_fields[[1]])) - as.integer(!!sym(compare_fields[[2]]))) > 1) |>
  dplyr::ungroup() |> 
# 68
  dim()

#' when was the survey?, all < June 2022
#' 

# grep("month", tolower(names(catch_info_lgb_i1_i2_i3)), value = T)

fish_hours_diff <-
  catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR,
         all_of(compare_fields),
         interview_date_time) |>
  dplyr::distinct() |>
  dplyr::rowwise() |>
  dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    interview_year = lubridate::year(interview_date_time),
    interview_month = lubridate::month(interview_date_time)
  ) |>
  dplyr::count(interview_month, name = "diff_fishing_hours")

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

#' compare ACTIVITY_TYPE_NAME no_harvested_selected
#' 

#' ACTIVITY_TYPE_NAME
#' 
#' [1] "TRIP WITH EFFORT"
#' 
#' catch_info_lgb_i1_i2_i3$no_harvested_selected |> unique()
#' 
#' 1, 2 (1=YES, 2=NO)
#' 

compare_fields <- 
  c("ACTIVITY_TYPE_NAME", "no_harvested_selected")

catch_info_lgb_i1_i2_i3 |>
  dplyr::filter(is.na(CATCH_SPECIES_ITIS)) |>
  head() |>
  dplyr::glimpse()
# [1] 145  50

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(tidyselect::all_of(compare_fields)) |>
  dplyr::distinct() |>
  head() |> 
  dplyr::glimpse()
# $ ACTIVITY_TYPE_NAME    <chr> "TRIP WITH EFFORT", "TRIP WITH EFFORT"
# $ no_harvested_selected <int> 2, 1

# compare DISTANCE_CODE_NAME fishing_distance ----

compare_fields <-
  c("DISTANCE_CODE_NAME", "fishing_distance")

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(tidyselect::all_of(compare_fields)) |>
  dplyr::distinct() |>
  head() |> 
  dplyr::glimpse()

# compare FISHING_HOURS hrsf ----

compare_fields <-
  c("FISHING_HOURS", "hrsf")

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, all_of(compare_fields)) |>
  dplyr::distinct() |>
  # dim()
  # 674
  dplyr::rowwise() |> 
  dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()
# 367

# TODO: check if the same trip

# compare CATCH_SPECIES_ITIS tsn ----
#' https://en.wikipedia.org/wiki/Integrated_Taxonomic_Information_System
#' 

# grep("tsn", names(catch_info_lgb_i1_i2_i3), value = T)
# [1] "tsn.releas" "tsn.harv" 

compare_fields <-
  c("CATCH_SPECIES_ITIS", "tsn.releas", "tsn.harv")

catch_info_lgb_i1_i2_i3 |> 
  dplyr::select(tidyselect::starts_with("tsn")) |> 
  dplyr::distinct() |> 
  # dim()
# 1026
  dplyr::filter(!tsn.releas == tsn.harv) |> 
  head() |> 
  dplyr::glimpse()
# 937

# dplyr::n_distinct(catch_info_lgb_i1_i2_i3$TRIP_ID)
# 887

catch_info_lgb_i1_i2_i3 |>
  # dplyr::tidyselect::all_of(all_of(compare_fields)) |>
  dplyr::select(VESSEL_OFFICIAL_NBR,
                TRIP_ID,
                tidyselect::all_of(compare_fields)) |>
  dplyr::distinct() |>
  # dim()
  # [1] 27634     5
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  # dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
# Rows: 23,108
  # dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]]))) |>
# Rows: 23,112
  # dplyr::filter(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]])) &
  #          !(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]])))) |>
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[3]])) &
           !(as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]])))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

## get scientific and common names by tsn ----

tsn_only <-
  catch_info_lgb_i1_i2_i3 |>
  dplyr::distinct(CATCH_SPECIES_ITIS, tsn.releas, tsn.harv) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

tsn_only <- data.frame(tsn = unique(unlist(tsn_only)))
# 'data.frame': 155 obs. of  1 variable

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
          dplyr::filter(language == "English") |>
          dplyr::select(commonName) |>
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

tsn_info_itis[[2]] |>
 head() |>
 dplyr::glimpse()

## check released and harvested separately ----
#' "CATCH_SPECIES_ITIS" vs "tsn.harv"
#' 

CATCH_SPECIES_ITIS_vs_tsn.harv <-
  catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_ID, all_of(compare_fields)) |>
  dplyr::distinct() |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::mutate(all_spp_1_trip_fhier_l =
           list(sort(unique(CATCH_SPECIES_ITIS)))) |>
  dplyr::mutate(all_spp_1_trip_survey_l =
           list(sort(unique(tsn.harv)))) |> 
  dplyr::ungroup()

cathc_spp_diff <-
  CATCH_SPECIES_ITIS_vs_tsn.harv |>
  dplyr::select(-all_of(compare_fields)) |>
  dplyr::distinct() |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
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
  dplyr::ungroup()
# 887  

cathc_spp_diff__no_diff <-
  cathc_spp_diff |>
  dplyr::rowwise() |>
  dplyr::mutate(
    ll_f =
      length(harv_diff_in_fhier_only),
    ll_s =
      length(harv_diff_in_survey_only)
  ) |>
  dplyr::mutate(no_diff_spp =
           dplyr::case_when((ll_f == ll_s &
                        ll_f == 0) ~ "no_diff", 
                     .default = "is_diff")) |>
  dplyr::ungroup()

cathc_spp_diff__no_diff |>
 head() |>
 dplyr::glimpse()

# TODO: check released and harvested separately

## check numbers for the same spp lgb/harvested ----
catch_info_lgb_i1_i2_i3 |> 
  # dplyr::select(VESSEL_OFFICIAL_NBR, TRIP_ID, all_of(compare_fields)) |>
  # dplyr::distinct() |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

## check numbers for the same spp lgb/released ----
catch_info_lgb_i1_i2_i3 |> 
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

## the same sp. is both released and harvested in the same trip (OK) ----

catch_info_lgb_i1_i2_i3 |> 
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |> 
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

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

#' "UNIT_MEASURE", (all "CN")
#' 

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
  dplyr::select("TRIP_ID",
  "VESSEL_OFFICIAL_NBR",
  "id_code",
  all_of(catch_fields)
) |> 
  dplyr::distinct()

dim(catch_info_lgb_i1_i2_i3)
# [1] 89466    50

dim(catch_info_lgb_i1_i2_i3_short)
# [1] 37506    15

## look at catch_info_lgb_i1_i2_i3_short_harvested ----
catch_info_lgb_i1_i2_i3_short_harvested <-
  catch_info_lgb_i1_i2_i3_short |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::filter(as.integer(!!sym(compare_fields[[1]])) ==
           as.integer(!!sym(compare_fields[[3]]))) |>
  dplyr::ungroup() |> 
  dplyr::select(-tidyselect::ends_with(".releas")) |>
  dplyr::distinct()

dim(catch_info_lgb_i1_i2_i3_short_harvested)
# [1] 6469   15
# [1] 5829   11 (no ".releas")

# data_overview(catch_info_lgb_i1_i2_i3_short_harvested)
# TRIP_ID              841
# VESSEL_OFFICIAL_NBR  219

# View(catch_info_lgb_i1_i2_i3_short_harvested)

# catch_info_lgb_i1_i2_i3_short_harvested |> 
    # dplyr::filter(TRIP_ID == "1000020436") |> dplyr::distinct() |>  View()

## count amount of species per trip ----
# db_logbooks_2022_short |> print_df_names()
# survey_data_l_2022_short |> print_df_names()

db_logbooks_2022_short_cnt_spp <-
  db_logbooks_2022_short |>
  dplyr::select(
    VESSEL_OFFICIAL_NBR,
    TRIP_ID,
    DISPOSITION_CODE,
    DISPOSITION_NAME,
    CATCH_SPECIES_ITIS
  ) |>
  dplyr::distinct() |>
  dplyr::add_count(VESSEL_OFFICIAL_NBR, 
            TRIP_ID, 
            DISPOSITION_CODE, 
            name = "n_CATCH_SPECIES_ITIS__by_disp") |>
  dplyr::group_by(VESSEL_OFFICIAL_NBR, TRIP_ID) |>
  dplyr::mutate(n_CATCH_SPECIES_ITIS = dplyr::n_distinct(CATCH_SPECIES_ITIS)) |>
  dplyr::ungroup()

db_logbooks_2022_short_cnt_spp |> 
  dplyr::arrange(VESSEL_OFFICIAL_NBR, TRIP_ID, DISPOSITION_CODE) |> 
  head() |> 
  dplyr::glimpse()

# db_logbooks_2022_short_cnt_spp |> 
#   dplyr::filter(TRIP_ID == "61422515") |> View()

## count amount of species per interview ----
# survey_data_l_2022_short

survey_data_l_2022_short_cnt_spp <- 
  survey_data_l_2022_short$i3 |>
  dplyr::select(id_code, tsn, fshinsp, disp3, lngth, wgt, num_typ3) |> 
  dplyr::distinct() |>
  dplyr::group_by(id_code, disp3) |> 
  dplyr::mutate(n_tsn_by_disp3 = dplyr::n_distinct(tsn)) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(id_code) |> 
  dplyr::mutate(n_tsn = dplyr::n_distinct(tsn)) |> 
  dplyr::ungroup()

survey_data_l_2022_short_cnt_spp |>
 head() |>
 dplyr::glimpse()

survey_data_l_2022_short_cnt_spp |>
  dplyr::filter(id_code == "1590520220121004") |>
  dplyr::select(-c(lngth, wgt)) |>
  dplyr::distinct() |>
  dplyr::arrange(tsn) |>
  head() |>
  dplyr::glimpse()

# TODO: compare ns

#' compare NUM_TYP3 with number of cought and retained fish in FHIER
#' 
compare_fields <-
  c("num_typ3.harv", "REPORTED_QUANTITY")

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, 
         TRIP_ID,
         all_of(compare_fields)) |>
  # dplyr::arrange(VESSEL_OFFICIAL_NBR,
  #         TRIP_ID,
  #         num_typ3.harv) |> 
  dplyr::distinct() |>
  # dim()
  # [1] 19124     3
  dplyr::rowwise() |> 
  dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

# compare fshinsp, REPORTED_QUANTITY ----
compare_fields <-
  c("fshinsp", "REPORTED_QUANTITY")

catch_info_lgb_i1_i2_i3 |>
  dplyr::select(VESSEL_OFFICIAL_NBR, 
         TRIP_ID,
         all_of(compare_fields)) |>
  dplyr::arrange(VESSEL_OFFICIAL_NBR,
          TRIP_ID,
          fshinsp) |>
  dplyr::distinct() |>
  dplyr::rowwise() |> 
  dplyr::filter(!as.integer(!!sym(compare_fields[[1]])) == as.integer(!!sym(compare_fields[[2]]))) |>
  dplyr::ungroup() |> 
  head() |> 
  dplyr::glimpse()

# survey time difference vs trip start/trip end ----

db_logbooks_2022_short__fish_hours <-
  db_logbooks_2022 |>
  dplyr::select(
    TRIP_ID,
    VESSEL_OFFICIAL_NBR,
    FISHING_HOURS,
    TRIP_START_DATE,
    TRIP_START_TIME,
    TRIP_END_DATE,
    TRIP_END_TIME
  ) |> 
  dplyr::distinct()

dim(db_logbooks_2022_short__fish_hours)
# [1] 94899     7

survey_lgb_by_date_vessl_all <-
  inner_join(
    survey_data_l_2022_vsl_date,
    db_logbooks_2022_short__fish_hours,
    join_by(vsl_num == VESSEL_OFFICIAL_NBR, 
            interview_date == TRIP_END_DATE),
    relationship = "many-to-many"
  ) |> 
  dplyr::arrange(interview_date) |> 
  dplyr::select(-all_of(tidyselect::starts_with("int_")))

dim(survey_lgb_by_date_vessl_all)
# 1115

survey_lgb_by_date_vessl_all |> 
    dplyr::filter(!TRIP_START_DATE == interview_date) |> 
  dim()
# 8

# survey_lgb_by_date_vessl_all$TRIP_START_TIME1 <-
strftime(strptime(sapply(paste0("0000", survey_lgb_by_date_vessl_all$TRIP_START_TIME), function(i)
  substring(i, nchar(i) - 3, nchar(i))), "%H%M"), format = "%H:%M") |> head()

survey_lgb_by_date_vessl_all__trip_dur <-
  survey_lgb_by_date_vessl_all |>
  dplyr::mutate(TRIP_START_TIME_1 =
           stringr::str_replace(TRIP_START_TIME, "(\\d+)(\\d\\d)", "\\1:\\2")) |>
  dplyr::mutate(TRIP_END_TIME_1 =
           stringr::str_replace(TRIP_END_TIME, "(\\d+)(\\d\\d)", "\\1:\\2")) |>
  dplyr::mutate(TRIP_START_TIME_2 =
           lubridate::hm(TRIP_START_TIME_1)) |>
  dplyr::mutate(TRIP_END_TIME_2 =
           lubridate::hm(TRIP_END_TIME_1)) |>
  dplyr::mutate(TRIP_START_TO_END =
           TRIP_END_TIME_2 - TRIP_START_TIME_2) |> 
  dplyr::mutate(TRIP_START_TO_END_d =
           lubridate::as.duration(TRIP_START_TO_END))

## convert fishing hours to duration ----
convert_fish_hours_to_duration <- function(FISHING_HOURS) {
  as.difftime(FISHING_HOURS, units = "hours") |> 
    lubridate::as.period() |> 
    lubridate::as.duration()
}

### for logbooks ----
survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur <-
  survey_lgb_by_date_vessl_all__trip_dur |>
  dplyr::rowwise() |>
  dplyr::mutate(FISHING_HOURS_d = convert_fish_hours_to_duration(FISHING_HOURS)) |>
  dplyr::ungroup()

#' check
#' 
survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur[93,][["FISHING_HOURS_d"]]
#' [1] "12600s (~3.5 hours)"
#' 

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur[93,][["TRIP_START_TO_END_d"]] - survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur[93,][["FISHING_HOURS_d"]]
# [1] "1800s (~30 minutes)"

### convert survey fishing hours hrsf to duration ----

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur <-
  survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur |>
  dplyr::rowwise() |>
  dplyr::mutate(hrsf_d = convert_fish_hours_to_duration(hrsf)) |>
  dplyr::ungroup()

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur |>
  head() |>
  dplyr::glimpse()

#' check row 93 with 3.5
#' 
survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur |> 
  head(96) |> 
  tail() |> 
  dplyr::mutate(FISHING_HOURS_mean = mean.difftime(FISHING_HOURS_d)) |> 
  dplyr::select(tidyselect::starts_with("FISHING_HOURS")) |> 
  dplyr::distinct() |> 
  str()

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur[93,][["TRIP_START_TO_END_d"]] - survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur[93,][["hrsf_d"]]
# [1] "3600s (~1 hours)"

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur[93,][["FISHING_HOURS_d"]] - survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur[93,][["hrsf_d"]]
# [1] "1800s (~30 minutes)"

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur$TRIP_START_TO_END_d |> 
  mean()

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur$hrsf_d |> 
  mean()

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur |> 
  # dplyr::filter(!is.na(FISHING_HOURS_d)) |> 
  dplyr::select(tidyselect::ends_with("_d")) |> 
  summary()

# how many fishing hours are matching vs. not (interval) ----

survey_lgb_by_date_vessl_all__trip_dur__fish_h_dur__hrsf_dur |>
  dplyr::select(tidyselect::ends_with("_d")) |>
  head() |> 
  dplyr::glimpse()

# how many are srhs vsls? ----

#' all interviews:
#' 
survey_data_l_2022_i1_w_dates |>
  dplyr::select(vsl_num, srhs_vessel) |>
  dplyr::distinct() |>
  dplyr::count(srhs_vessel)
  # srhs_vessel     n
# 1           1     4
# 2           2   472
