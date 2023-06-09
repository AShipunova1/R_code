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
  mutate(year_region = 
           paste(year, permit_sa_gom))

# count weeks per vessel, year, region, compl ----
compl_clean_sa_vs_gom_m_int_c_cnts <-
  compl_clean_sa_vs_gom_m_int_c %>%
  add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  add_count(year, permit_sa_gom, vessel_official_number, name = "total_weeks_per_vessel")

## test ----
nc_2023_gom_only_test <-
  compl_clean_sa_vs_gom_m_int_c_cnts %>%
  filter(year_region == "2023 gom_only",
         compliant_ == "NO") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()

head(nc_2023_gom_only_test)
# 1247024   11  22
# FL4749LH  1   22
# 1298355   1   22

compl_clean_sa_vs_gom_m_int_c_cnts %>%
  filter(year_region == "2023 gom_only",
         compliant_ == "YES",
         vessel_official_number == "FL4749LH") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()
# 21  22

# compl vs. non-compl vessels per year, region ----

compl_vs_non_compl_per_year_cnt <-
  compl_clean_sa_vs_gom_m_int_c_cnts %>% 
  select(vessel_official_number, year_region, compliant_) %>% 
  unique() %>% 
  add_count(compliant_, year_region, name = "cnt_compl_per_perm_year")

glimpse(compl_vs_non_compl_per_year_cnt)

### test ----
compl_vs_non_compl_per_year_cnt %>%
  filter(year_region == "2023 gom_only",
         vessel_official_number == "FL4749LH") %>%
  glimpse()
# $ compliant_              <chr> "YES", "NO"
# $ cnt_compl_per_perm_year <int> 998, 3

# separate vessels non-compliant at least once per year ----
non_compl_vessel_ids_per_y_r <-
  compl_vs_non_compl_per_year_cnt %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, year_region) %>%
  unique()

View(non_compl_vessel_ids_per_y_r)

## split by year_region ----
non_compl_vessel_ids_per_y_r_list <-
  split(non_compl_vessel_ids_per_y_r,
        as.factor(non_compl_vessel_ids_per_y_r$year_region))

# View(non_compl_vessel_ids_per_y_r_list)

all_compl_vs_non_compl_per_year_cnt_list <-
  split(compl_vs_non_compl_per_year_cnt,
        as.factor(compl_vs_non_compl_per_year_cnt$year_region))

View(all_compl_vs_non_compl_per_year_cnt_list)
# all_compl_vs_non_compl_per_year_cnt_list[["2023 gom_only"]][["cnt_compl_per_perm_year"]] %>% 
# unique()
# [1] 998   3


# If a vessel was non-compliant even once during a year, it is non_compliant for that year.
# remove non-compl vessels from compliant = 
# total unique vessels number vs. non-compl vessels

compl_only <-
  names(all_compl_vs_non_compl_per_year_cnt_list) %>% 
  map_df(
    function(current_year_region) {
      browser()
      curr_df <-
        all_compl_vs_non_compl_per_year_cnt_list[[current_year_region]]
      
      curr_non_compl_vsl_ids <- non_compl_vessel_ids_per_y_r %>%
        filter(year_region == current_year_region) %>%
        select(vessel_official_number)
      
      curr_df_compl_only <-
        curr_df %>%
        filter(!(vessel_official_number %in% curr_non_compl_vsl_ids$vessel_official_number))
      
      return(curr_df_compl_only)
    }
)
