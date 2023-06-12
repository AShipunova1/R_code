# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# add year_region column ----
compl_clean_sa_vs_gom_m_int_c <-
  compl_clean_sa_vs_gom_m_int %>%
  mutate(year_region = 
           paste(year, permit_sa_gom))

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
# 5 NO         2023  gom_only          3
# 6 NO         2023  sa_only        1384
# 7 YES        2022  both            366


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
  mutate(percent_non_compl = non_compl_vsls * 100 / total_vsl_ids_per_y_r)
# %>% 
#   mutate(perc_labels = paste0(round(percent_compl, 1), "%"))

View(vessels_cnt_per_year_reg_compl_tot_perc)

# vessels_cnt_per_year_reg_compl_tot_perc$year_region %>%
#   map(function(curr_year_region) {
#     browser()
#     vessels_cnt_per_year_reg_compl_tot_perc %>%
#       filter(year_region == curr_year_region) %>%
#       select(compl_vsls, non_compl_vsls) %>% pie()
#   })

r1 <-
  vessels_cnt_per_year_reg_compl_tot_perc %>%
  filter(year_region == "2022 both") %>%
  # select(compl_vsls, non_compl_vsls) %>%
  pivot_longer(cols = c(percent_compl,
                        percent_non_compl),
               names_to = "is_compliant",
               values_to = "percent")

glimpse(r1)  

main_title = "Percent unique compliant vs. non compliant vessels per year, permit region. 05/31/2023"

curr_year_region = "2022 both"
y_r_title = curr_year_region

# my_df <- r1

plots_for_c_vs_nc_vsls <- function(my_df, y_r_title) {
  total_vsls <- unique(my_df$total_vsl_ids_per_y_r)
  current_title <- paste0(y_r_title, ". Total vsls: ", total_vsls)
  one_plot <-
    my_df %>%
    ggplot(aes(x = is_compliant,
               y = percent,
               fill = is_compliant)) +
    geom_col() +
    ylim(0, 100) +
    geom_text(aes(label = paste0(round(percent, 1), "%")),
              position = position_stack(vjust = 0.5)) +
    labs(title = current_title,
         x = "",
         y = "") +
    scale_x_discrete(labels = c("Yes", "No")) +
    
    scale_fill_manual(
      values =
        c(
          "percent_compl" = "lightgreen",
          "percent_non_compl" = "red"
        ),
      name = "Is compliant?",
      labels = c("Yes", "No")
    )
  return(one_plot)
}

gg_all_c_vs_nc_plots <-
vessels_cnt_per_year_reg_compl_tot_perc$year_region %>%
  map(function(curr_year_region) {
    # browser()
    curr_df <-    
    vessels_cnt_per_year_reg_compl_tot_perc %>%
      filter(year_region == curr_year_region) %>%
  pivot_longer(cols = c(percent_compl,
                        percent_non_compl),
               names_to = "is_compliant",
               values_to = "percent")

    y_r_title = curr_year_region
    plots_for_c_vs_nc_vsls(curr_df, y_r_title)

  })

# gg_all_c_vs_nc_plots[[5]]

grid.arrange(grobs = gg_all_c_vs_nc_plots,
             top = main_title,
             # left = my_legend,
             ncol = 3)

# View(vessels_cnt_per_year_reg_compl_tot_perc)
# TODO: 
# done) add actual numbers to plots,
# keep only one legend
# plots for nc vessels with buckets of weeks

# Non compliant only ----

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
count_weeks_per_vsl_permit_year_compl <-
  compl_clean_sa_vs_gom_m_int_c %>%
  add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  add_count(year, permit_sa_gom, vessel_official_number, name = "total_weeks_per_vessel")

## test ----
nc_2023_gom_only_test <-
  count_weeks_per_vsl_permit_year_compl %>%
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

count_weeks_per_vsl_permit_year_compl %>%
  filter(year_region == "2023 gom_only",
         compliant_ == "YES",
         vessel_official_number == "FL4749LH") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()
# 21  22

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  count_weeks_per_vsl_permit_year_compl %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

# View(count_weeks_per_vsl_permit_year_compl_p)

# test
# View(count_weeks_per_vsl_permit_year_compl_p)
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", year == "2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178 

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", 
         year == "2022",
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
# 1289    Non compliant vsl
  dim()
# [1] 26466 non compliant weeks

### test ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(
    year,
    permit_sa_gom,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc_percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----
count_weeks_per_vsl_permit_year_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>% 
  filter(compliant_ == "NO") %>%
  select(
    year_region,
    vessel_official_number,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

str(count_weeks_per_vsl_permit_year_compl_p_short)
# tibble [3,224 × 5] (S3: tbl_df/tbl/data.frame)

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_compl_p_short_y_p)

count_weeks_per_vsl_permit_year_compl_p_short_cuts <-
  count_weeks_per_vsl_permit_year_compl_p_short %>%
  mutate(
    percentage_rank =
      case_when(
        percent_compl < 25 ~ '0-24%',
        25 <= percent_compl &
          percent_compl < 50 ~ '25-49%',
        50 <= percent_compl &
          percent_compl < 75 ~ '50-74%',
        75 <= percent_compl ~ '75-100%'
      )
  )

View(count_weeks_per_vsl_permit_year_compl_p_short_cuts)

### test 2 ----
count_weeks_per_vsl_permit_year_compl_p_short_cuts %>% 
  filter(percentage_rank == '75-100%') %>%
  filter(year_region == "2023 sa_only") %>%
  count(percent_compl, year_region,
        name = "amount_of_occurences") %>%
  arrange(desc(percent_compl)) %>% 
  # View()
  count(wt = amount_of_occurences)
# 499

# 3) count how many in each bucket ----

View(count_weeks_per_vsl_permit_year_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b <-  
  count_weeks_per_vsl_permit_year_compl_p_short_cuts %>%
    add_count(year_region, 
              percentage_rank,
              name = "cnt_v_in_bucket")

# test
count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>% 
   filter(year_region == "2022 both") %>%
      select(year_region, percentage_rank, cnt_v_in_bucket) %>%
      unique() %>% 
  add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>% 
    View()  
# "2022 both"
# 12+17+85+3
# [1] 117

## 3a) total per year / region ----
# View(count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot <-
  count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b %>%
  select(year_region,
         percentage_rank,
         cnt_v_in_bucket) %>%
  unique() %>%
  # total cnt per year, region
  add_count(year_region, wt = cnt_v_in_bucket, name = "total_per_y_r")
# %>%
#   filter(year_reg == "2022 both")
  
# View(count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot)

