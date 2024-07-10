## compliance from FHIER ----

### FHIER: complaince after ovrridden ----

tictoc::tic("fhier_compliance compl_overr")
fhier_compliance_2022__comp_after_overr <-
  fhier_compliance_2022 |>
  auxfunctions::add_compliant_after_override(overridden_col_name = "overridden_",
                                             compliance_col_name = "compliant_")
tictoc::toc()
# fhier_compliance compl_overr: 30.16 sec elapsed

# dim(fhier_compliance_2022__comp_after_overr)

### FHIER: shorten _compliance_2022__comp_after_overr ----

fhier_compliance_2022__comp_after_overr__short <-
  fhier_compliance_2022__comp_after_overr |>
  select(
    c(
      vessel_official_number,
      week,
      gom_permitteddeclarations__,
      captainreports__,
      negativereports__,
      compliant_after_override
    )
  ) |>
  distinct()

dim(fhier_compliance_2022__comp_after_overr__short)
# [1] 125823      6

### split week column ----
#' split week column ("52: 12/26/2022 - 01/01/2023") into 3 columns with proper classes, week_num (week order number), week_start and week_end
clean_weeks <- function(my_df) {
  my_df %>%
    tidyr::separate_wider_delim(week, ":", names = c("week_num", "week_rest")) %>%
    tidyr::separate_wider_delim(week_rest, " - ", names = c("week_start", "week_end")) ->
    temp_df

  my_df$week_num <- as.integer(trimws(temp_df$week_num))
  my_df$week_start <- as.Date(trimws(temp_df$week_start), "%m/%d/%Y")
  my_df$week_end <- as.Date(trimws(temp_df$week_end), "%m/%d/%Y")

  return(my_df)
}

fhier_compliance_2022__comp_after_overr__short__clean_weeeks <-
  fhier_compliance_2022__comp_after_overr__short |> 
  clean_weeks() |> 
  select(-week) |> 
  distinct()
  
dim(fhier_compliance_2022__comp_after_overr__short__clean_weeeks)
# [1] 125823      8

### trim vesselofficialnumber, there are 273 white spaces in Feb 2023 ----
trim_all_vessel_ids_simple <-
  function(my_df, col_name_to_trim = NA) {
    # browser()
    #' get col name to trim
    if (is.na(col_name_to_trim)) {
      col_name_to_trim <- grep("vessel.*official.*number", tolower(names(my_df)), value = T)
    }
    col_name_to_trim_s <- sym(col_name_to_trim)
    #' Hard code vessel_official_number as vessel id col name
    res_df <-
      my_df |>
      dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s) |>
                      tolower())
    
    return(res_df)
    
  }

fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id <-
  trim_all_vessel_ids_simple(fhier_compliance_2022__comp_after_overr__short__clean_weeeks)

glimpse(fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id)

### FHIER: add a column for month  ----

fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m <-
  fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id |>
  dplyr::mutate(year_month = zoo::as.yearmon(week_start))

dim(fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m)
# [1] 125823      9

# Survey comparison with FHIER compliance ----
# fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m

# get fhier compliance information for vessels from survey no lgb ----
fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m__interv <-
  fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m |>
  filter(tolower(vessel_official_number) %in% tolower(lgb_join_i1__no_lgb__short$VESSEL_OFFICIAL_NBR))

dim(fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m__interv)
# [1] 7600    9

vessels_in_survey_no_lgb__n__compl_fhier <- 
  unique(fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m__interv$vessel_official_number)

setdiff(tolower(vessels_in_survey_no_lgb__n__compl), vessels_in_survey_no_lgb__n__compl_fhier)
# 0

n_distinct(fhier_compliance_2022__comp_after_overr__short__clean_weeeks__clean_vsl_id__m__interv$vessel_official_number)
# 192

#' Why there are vessels in survey_no_lgb with no compliance info?

vessels_in_survey_not_in_compl <-
  setdiff(vessels_in_survey_no_lgb,
          vessels_in_survey_no_lgb__n__compl_fhier)

length(vessels_in_survey_not_in_compl)
# 38

#' To use as a filter in FHIER/compliance
vessels_in_survey_not_in_compl |> 
  paste(collapse = ", ") |> 
  print()
#' nothing found for 2022

#' check these vessels in original survey, found 8, the rest is from the fuzzy join with PIMS
vessels_in_survey_and_in_compl <-
  survey_data_l_2022_i1_w_dates_clean_vsl_no_na_vsl_num__short |>
  filter(survey_vessel_id %in% vessels_in_survey_not_in_compl) |>
  select(survey_vessel_id) |>
  distinct()
#' Manually checked in FHIER, no compliance  found for 2022

#' stop repeating here, the vessel ids are the same as from db.