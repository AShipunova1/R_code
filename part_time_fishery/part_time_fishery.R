library(zoo)

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int |>
  filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# compl_clean_sa_vs_gom_m_int_1 |> 
#   select(permit_sa_gom) |> 
#   distinct()

# remove 2023 gom_only ----
compl_clean_sa_vs_gom_m_int_filtered_2022_gom <-
  # from get_data
  compl_clean_sa_vs_gom_m_int_1 %>%
  filter(year == '2022' & permit_sa_gom %in% c("gom_only", "dual"))

compl_clean_sa_vs_gom_m_int_filtered_2022_gom_vsls <-
  compl_clean_sa_vs_gom_m_int_filtered_2022_gom |> 
  select(vessel_official_number) |> 
  distinct() |> 
  dim()
# [1] 1304    1
# 1304 total permitted in the compliance from FHIER ok

total_gom_dual_22 <- compl_clean_sa_vs_gom_m_int_filtered_2022_gom_vsls[1]

# View(compl_clean_sa_vs_gom_m_int_filtered_2022_gom)

compl_clean_sa_vs_gom_m_int_filtered_2022_gom |> 
  # has a declaration
  filter(gom_permitteddeclarations__ > 0) |> 
  select(vessel_official_number) |> 
  distinct() |> 
  dim()
# 808

compl_clean_sa_vs_gom_m_int_filtered_2022_gom |> 
  # has a declaration
  filter(gom_permitteddeclarations__ == 0) |> 
  select(vessel_official_number) |> 
  distinct() |> 
  dim()
