# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# save vsl count ----

count_vessels <-
  compl_clean_sa_vs_gom_m_int %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 4017 vessels

count_weeks <-
  compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_, year, permit_sa_gom)
# ...
# 5 NO         2023  gom_only         13
# 6 NO         2023  sa_only       11793
# 7 YES        2022  both          15439
# 8 YES        2022  gom_only      43326
# ...

vessels_compl_or_not_per_y_r <-
  compl_clean_sa_vs_gom_m_int %>%
  select(vessel_official_number, compliant_, year, permit_sa_gom) %>%
  unique() %>%
  count(compliant_, year, permit_sa_gom)
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

## fewer cols ---- 
compl_clean_sa_vs_gom_m_int_c_short <-
  compl_clean_sa_vs_gom_m_int_c %>% 
  select(vessel_official_number, year_region, compliant_) %>% 
  unique()

glimpse(compl_clean_sa_vs_gom_m_int_c_short)

### test ----
compl_clean_sa_vs_gom_m_int_c_short %>%
  filter(year_region == "2023 gom_only",
         vessel_official_number == "FL4749LH") %>%
  glimpse()
# $ compliant_              <chr> "YES", "NO"

# separate vessels non-compliant at least once per year ----
non_compl_vessel_ids_per_y_r <-
  compl_clean_sa_vs_gom_m_int_c_short %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, year_region) %>%
  unique()

View(non_compl_vessel_ids_per_y_r)

## split by year_region ----
all_compl_vs_non_compl_per_year_cnt_list <-
  split(compl_clean_sa_vs_gom_m_int_c_short,
        as.factor(compl_clean_sa_vs_gom_m_int_c_short$year_region))

View(all_compl_vs_non_compl_per_year_cnt_list)

### test ----
all_compl_vs_non_compl_per_year_cnt_list[["2023 gom_only"]] %>%
  unique() %>% dim()
# [1] 1001   3

# If a vessel was non-compliant even once during a year, it is non_compliant for that year.
# remove non-compl vessels from compliant, to count each vessel once per year
# total unique vessels number vs. non-compl vessels

compl_only <-
  names(all_compl_vs_non_compl_per_year_cnt_list) %>% 
  map_df(
    function(current_year_region) {
      # browser()
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

### test ----
compl_only %>% 
  filter(year_region == "2023 gom_only") %>% 
  dim()
# 995  3

# get total unique vessel_ids per year_region ----

compl_only %>% 
  filter(year_region == "2023 gom_only") %>% 
  select(compliant_) %>% 
  table()
# compliant_
# YES 
# 995 

compl_only %>% 
  filter(year_region == "2023 gom_only") %>% 
  count(compliant_)
# 1 YES          995

compl_only_cnts <-
  compl_only %>% 
  add_count(year_region, compliant_, name = "compl_vsls") %>% 
  select(-c(vessel_official_number, compliant_)) %>% 
  unique()

tail(compl_only_cnts)
# 4 2023 both     YES           87
# 5 2023 gom_only YES          995
# 6 2023 sa_only  YES          521

non_compl_only_cnts <-
  non_compl_vessel_ids_per_y_r %>%
  add_count(year_region, name = "non_compl_vsls") %>%
  select(-vessel_official_number) %>%
  unique()

tail(non_compl_only_cnts)
# 1 2022 sa_only   1289
# 2 2022 both       117
# 3 2022 gom_only   187
# 4 2023 sa_only   1384
# 5 2023 both       244
# 6 2023 gom_only     3

vessels_cnt_per_year_reg_compl <-
  full_join(compl_only_cnts, non_compl_only_cnts)
# Joining with `by = join_by(year_region)`

head(vessels_cnt_per_year_reg_compl)
#   year_region   compl_vsls non_compl_vsls
# 1 2022 both            257            117
# 2 2022 gom_only        934            187
# 3 2022 sa_only         889           1289
# 4 2023 both             87            244
# 5 2023 gom_only        995              3
# 6 2023 sa_only         521           1384

# add total vessels_cnt_per_year_reg ----
vessels_cnt_per_year_reg_compl_tot <-
  vessels_cnt_per_year_reg_compl %>% 
  # compute on a data frame a row-at-a-time
  dplyr::rowwise() %>%
  mutate(total_vsl_ids_per_y_r = sum(compl_vsls, non_compl_vsls)) %>% 
  # return to the default colwise operations
  dplyr::ungroup()

head(vessels_cnt_per_year_reg_compl_tot)
                # YES NO    total
# 4 2023 both     87  244   331
# 5 2023 gom_only 995 3     998
# 6 2023 sa_only  521 1384  1905

# get perc non_compl vs. total for each year_region ----
names(vessels_cnt_per_year_reg_compl_tot)
vessels_cnt_per_year_reg_compl_tot_perc <-
  vessels_cnt_per_year_reg_compl_tot %>% 
  mutate(percent_compl = compl_vsls * 100 / total_vsl_ids_per_y_r) %>% 
  mutate(perc_labels = paste0(round(percent_compl, 1), "%"))

View(vessels_cnt_per_year_reg_compl_tot_perc)

vessels_cnt_per_year_reg_compl_tot_perc$year_region %>%
  map(function(curr_year_region) {
    browser()
    vessels_cnt_per_year_reg_compl_tot_perc %>%
      filter(year_region == curr_year_region) %>%
      select(compl_vsls, non_compl_vsls) %>% pie()
  })

# install.packages("lessR")
library(lessR)

PieChart(gend, hole = 0, values = "%", data = gender,
         fill = c("lightblue", "pink"), main = "")
