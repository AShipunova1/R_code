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