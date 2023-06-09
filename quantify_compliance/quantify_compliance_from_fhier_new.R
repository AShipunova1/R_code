# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# save vsl count ----
compl_clean_sa_vs_gom_m_int %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 4017 vessels

compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_, year, permit_sa_gom) %>% 
  View()
# weeks

compl_clean_sa_vs_gom_m_int %>% 
  select(vessel_official_number, compliant_, year, permit_sa_gom) %>% 
  unique() %>% 
  count(compliant_, year, permit_sa_gom) %>% 
  View()
# vessels

# add year_region column ----
compl_clean_sa_vs_gom_m_int_c <-
  compl_clean_sa_vs_gom_m_int %>%
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  mutate(year_region = 
           paste(year, permit_sa_gom)) %>% 
  # return to the default colwise operations
  dplyr::ungroup()
  
View(compl_clean_sa_vs_gom_m_int_c2)

compl_clean_sa_vs_gom_m_int_c2 <-
  compl_clean_sa_vs_gom_m_int %>%
  # # compute on a data frame a row-at-a-time
  # dplyr::rowwise() %>%
  mutate(year_region = 
           paste(year, permit_sa_gom))
# %>% 
  # return to the default colwise operations
  # dplyr::ungroup()

identical(compl_clean_sa_vs_gom_m_int_c, compl_clean_sa_vs_gom_m_int_c2)
# compl vs. non-compl vessels per year, region ----

compl_vs_non_compl_per_year_cnt <-
  compl_clean_sa_vs_gom_m_int %>% 
  count(permit_sa_gom, compliant_, year, name = "cnt_compl_per_perm_year")

glimpse(compl_vs_non_compl_per_year_cnt)

total_per_permit_year <-
  compl_vs_non_compl_per_year_cnt %>%
  count(permit_sa_gom,
        year,
        wt = cnt_compl_per_perm_year,
        name = "total_compl_per_perm_year")
