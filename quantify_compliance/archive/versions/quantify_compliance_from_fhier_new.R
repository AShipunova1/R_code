# get_data_from_param <- "csv"

source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# add year_region column ----
compl_clean_sa_vs_gom_m_int_c <-
  compl_clean_sa_vs_gom_m_int %>%
  dplyr::mutate(year_region = 
           paste(year, permit_sa_gom))

# functions
# name <- function(variables) {
#   
# }

# save vsl count ----

count_vessels <-
  compl_clean_sa_vs_gom_m_int %>% 
  select(vessel_official_number) %>% 
  unique() %>% 
  dim()
# 4017 vessels

count_weeks <-
  compl_clean_sa_vs_gom_m_int %>% 
  dplyr::count(compliant_, year, permit_sa_gom)
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
  dplyr::count(compliant_, year, permit_sa_gom)
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
  dplyr::glimpse()
# $ compliant_              <chr> "YES", "NO"

# separate vessels non-compliant at least once per year ----
non_compl_vessel_ids_per_y_r <-
  compl_clean_sa_vs_gom_m_int_c_short %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, year_region) %>%
  unique()

# View(non_compl_vessel_ids_per_y_r)

## split by year_region ----
all_compl_vs_non_compl_per_year_cnt_list <-
  split(compl_clean_sa_vs_gom_m_int_c_short,
        as.factor(compl_clean_sa_vs_gom_m_int_c_short$year_region))

# View(all_compl_vs_non_compl_per_year_cnt_list)

### test ----
all_compl_vs_non_compl_per_year_cnt_list[["2023 gom_only"]] %>%
  unique() %>% dim()
# [1] 1001   3

# If a vessel was non-compliant even once during a year, it is non_compliant for that year.
# remove non-compl vessels from compliant, to count each vessel once per year
# total unique vessels number vs. non-compl vessels

compl_only <-
  names(all_compl_vs_non_compl_per_year_cnt_list) %>% 
  purrr::map_df(
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
  dplyr::count(compliant_)
# 1 YES          995

compl_only_cnts <-
  compl_only %>% 
  dplyr::add_count(year_region, compliant_, name = "compl_vsls") %>% 
  select(-c(vessel_official_number, compliant_)) %>% 
  unique()

tail(compl_only_cnts)
# 4 2023 both     YES           87
# 5 2023 gom_only YES          995
# 6 2023 sa_only  YES          521

non_compl_only_cnts <-
  non_compl_vessel_ids_per_y_r %>%
  dplyr::add_count(year_region, name = "non_compl_vsls") %>%
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
  dplyr::mutate(total_vsl_ids_per_y_r = sum(compl_vsls, non_compl_vsls)) %>% 
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
  dplyr::mutate(percent_compl = compl_vsls * 100 / total_vsl_ids_per_y_r) %>% 
  dplyr::mutate(percent_non_compl = non_compl_vsls * 100 / total_vsl_ids_per_y_r)
# %>% 
#   dplyr::mutate(perc_labels = paste0(round(percent_compl, 1), "%"))

# View(vessels_cnt_per_year_reg_compl_tot_perc)

# vessels_cnt_per_year_reg_compl_tot_perc$year_region %>%
#   purrr::map(function(curr_year_region) {
#     browser()
#     vessels_cnt_per_year_reg_compl_tot_perc %>%
#       filter(year_region == curr_year_region) %>%
#       select(compl_vsls, non_compl_vsls) %>% pie()
#   })

r1 <-
  vessels_cnt_per_year_reg_compl_tot_perc %>%
  filter(year_region == "2022 both") %>%
  # select(compl_vsls, non_compl_vsls) %>%
  tidyr::pivot_longer(cols = c(percent_compl,
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
  purrr::map(function(curr_year_region) {
    # browser()
    curr_df <-    
    vessels_cnt_per_year_reg_compl_tot_perc %>%
      filter(year_region == curr_year_region) %>%
  tidyr::pivot_longer(cols = c(percent_compl,
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

# TODO: 
# done) add actual numbers to plots,
# keep only one legend
# done) plots for nc vessels with buckets of weeks

# Non compliant only ----

# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
count_weeks_per_vsl_permit_year_compl <-
  compl_clean_sa_vs_gom_m_int_c %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year, permit_sa_gom, vessel_official_number, name = "total_weeks_per_vessel")

# View(count_weeks_per_vsl_permit_year_compl)

## test 1a ----
count_weeks_per_vsl_permit_year_compl %>% 
 filter(vessel_official_number == "1000042" &
          year == "2022") %>% 
 select(year, compliant_, weeks_per_vessel_per_compl, total_weeks_per_vessel) %>% 
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022  YES                                50                     52
# 2 2022  NO                                  2                     52

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
  dplyr::mutate(percent_compl =
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

### test 1b ----
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
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc_percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----
count_weeks_per_vsl_permit_year_n_compl_p_short <-
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

str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,224 × 5] (S3: tbl_df/tbl/data.frame)

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <- 
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>% 
  filter(percent_n_compl_rank == '75-100%') %>%
  filter(year_region == "2023 sa_only") %>%
  dplyr::count(percent_compl, year_region,
        name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>% 
  # View()
  dplyr::count(wt = amount_of_occurences)
# 499

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-  
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
    dplyr::add_count(year_region, 
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>% 
   filter(year_region == "2022 both") %>%
      select(year_region, percent_n_compl_rank, cnt_v_in_bucket) %>%
      unique() %>% 
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>% 
    str()  
 # $ percent_n_compl_rank: chr [1:4] "75-100%" "25-49%" "0-24%" "50-74%"
 # $ cnt_v_in_bucket     : int [1:4] 12 17 85 3
 # $ total_per_y_r       : int [1:4] 117 117 117 117

# "2022 both"
# 12+17+85+3
# [1] 117

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  dplyr::add_count(year_region, name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  # filter(year_region == "2022 both") %>%
  filter(year_region == "2023 gom_only") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>% 
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0-24%                               66.7
# 2 50-74%                              33.3

# 5) plots ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_region %>%
  unique() %>%
  sort() %>% 
  purrr::map(function(curr_year_region) {
    # browser()
    total_non_compl_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_region == curr_year_region) %>%
      select(perc_vsls_per_y_r_b,
             percent_n_compl_rank,
             perc_labels) %>%
      unique()
    
    # title_vals <-
    #   count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
    #   filter(year_region == curr_year_region) %>%
    #   select(cnt_v_in_bucket, vsls_per_y_r) %>%
    #   unique()
    
    # y_p_title <-
    #   paste0(curr_year_region, ". ",
    #          title_vals$cnt_v_in_bucket, " non compl, ",
    #          title_vals$vsls_per_y_r, " total vsls")
    y_p_title <- curr_year_region
    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "Been non compliant",
           y = "% nc vsls per year & region") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)
    
    return(one_plot)
  })

# gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[5]]

super_title = "Percent distribution of non compliant vessels per year & region"

grid.arrange(grobs =
               gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc,
             top = super_title,
             # left = my_legend,
             ncol = 3)
# percent_distribution.png

# Per month, region ----
# test
# View(compl_clean_sa_vs_gom_m_int_c)

compl_clean_sa_vs_gom_m_int_c %>%
  dplyr::add_count(year_region,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  filter(year_region == "2023 gom_only" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  View()

count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c %>%
  dplyr::add_count(year_region,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  dplyr::add_count(year_region,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")
  
# View(count_weeks_per_vsl_permit_year_compl_month)

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  select(
    year_region,
    year_month,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    compliant_
  ) %>%
  unique() %>%
  filter(year_region == "2023 gom_only") %>% 
  View()

count_weeks_per_vsl_permit_year_compl_m <-
  compl_clean_sa_vs_gom_m_int_c %>%
  dplyr::add_count(year_region, year_month,
            vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl_m")

# test
# count_weeks_per_vsl_permit_year_compl_m %>% 
# filter(year_region == "2023 gom_only" &
#            compliant_ == "NO") %>%
#   select(vessel_official_number,
#          compliant_,
#          year_month,
#          weeks_per_vessel_per_compl_m) %>%
#   unique() %>%
#   dplyr::glimpse()  
# $ vessel_official_number       <chr> "1247024", "1247024", "1247024", "…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO"
# $ year_month                   <yearmon> May 2023, Apr 2023, Mar 2023, Feb …
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 3, 1, 1
  
count_weeks_per_vsl_permit_year_compl_m_tot <-
  count_weeks_per_vsl_permit_year_compl_m %>%
  dplyr::add_count(year_region,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_m")

count_weeks_per_vsl_permit_year_compl_m_tot %>%
  filter(year_region == "2023 gom_only" &
           vessel_official_number == "1247024") %>%
  # compliant_ == "NO") %>%
  select(
    vessel_official_number,
    compliant_,
    year_month,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_m
  ) %>%
  unique() %>%
  dplyr::arrange(year_month) %>% 
  View()

## 1) Month: percent compl weeks per vsl per month ----

count_weeks_per_vsl_permit_year_compl_m_tot_p <-
  count_weeks_per_vsl_permit_year_compl_m_tot %>%
  dplyr::mutate(percent_compl_m =
           weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_tot_p %>% 
  filter(year_region == "2023 gom_only" &
           vessel_official_number == "1247024") %>%
  # compliant_ == "NO") %>%
  select(
    vessel_official_number,
    compliant_,
    year_month,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_m,
    percent_compl_m
  ) %>%
  unique() %>%
  dplyr::arrange(year_month) %>% 
  View()

## 2a) Month: Only non-compl and fewer cols ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort <-
  count_weeks_per_vsl_permit_year_compl_m_tot_p %>% 
  filter(compliant_ == "NO") %>%
  select(
    year_region,
    year_month,
    vessel_official_number,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

## 2b) Month: get percentage "buckets" ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b <-
  get_p_buckets(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort,
                "percent_compl_m")

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b)

### test 2, by month ----
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b %>% 
  filter(percent_n_compl_rank == '75-100%') %>%
  filter(year_region == "2023 gom_only" &
           vessel_official_number == "1247024") %>%
  dplyr::add_count(percent_compl_m, year_region,
        name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl_m)) %>% 
  dplyr::add_count(wt = amount_of_occurences) %>% 
  View()
# 3

## 3) Month: count how many in each bucket ----

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b <-  
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b %>%
    dplyr::add_count(year_region, 
              year_month,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot <-
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b %>%
  select(year_month,
         year_region,
         percent_n_compl_rank,
         cnt_v_in_bucket) %>%
  unique() %>%
  dplyr::add_count(year_month, year_region, wt = cnt_v_in_bucket,
            name = "tot_v_per_m_y_r")

### tests 3, by month ----

# total 35 nc vsls in "Jan 2022 both"
compl_clean_sa_vs_gom_m_int_c %>% 
  filter(year_region == "2022 both") %>%
  filter(year_month == "Jan 2022") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>% 
  unique() %>% str()
# 35

# still true
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b %>%
  filter(year_region == "2022 both") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>% 
  unique() %>% str()
# 35

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot %>%
  filter(year_region == "2022 both") %>%
  filter(year_month == "Jan 2022") %>%
  dplyr::arrange(percent_n_compl_rank) %>% 
  str()
 # $ percent_n_compl_rank: chr [1:3] "0-24%" "25-49%" "75-100%"
 # $ cnt_v_in_bucket     : int [1:3] 9 6 20
 # $ tot_v_per_m_y_r     : int [1:3] 35 35 35

# 35
# T

## 4) Month: cnt percents of (3) ----
# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p <-
  nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot %>%
  # dplyr::add_count(year_region, 
  #           year_month,
  #           name = "vsls_per_y_r_m") %>% View()
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / tot_v_per_m_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4, by month ----

# names(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p)
nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p %>%
  filter(year_region == "2022 both") %>%
  # filter(year_region == "2023 gom_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>% 
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0-24%                               25.7
# 2 25-49%                              17.1
# 3 75-100%                             57.1
# 20*100/35 == 57.1

# 5) Month plots ----

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p)

nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r <-
  split(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p,
        as.factor(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p$year_region))

# View(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r)

sorted_year_regions <- names(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r) %>%
  sort()
# [1] "2022 both"     "2022 gom_only" "2022 sa_only" 
# [4] "2023 both"     "2023 gom_only" "2023 sa_only" 


# get_one_y_month_data_to_plot(nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r$`2023 gom_only`, "Apr 2023") %>% 
#   View()

# View(my_df_list[["2023 gom_only"]])
# curr_year_month <- "Apr 2023"

# my_df <- my_df_list[["2023 gom_only"]]

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)
    
    curr_year_region <- curr_data$year_region %>%
      unique()
    
    curr_tot_v_per_m_y_r <- curr_data$tot_v_per_m_y_r %>%
      unique()
    
    curr_title <- paste(
      # curr_year_region, 
                        curr_year_month,
                        curr_tot_v_per_m_y_r,
                        "total vsls")

    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           x = "Non compliant",
           y = "% nc vsls") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)
    
    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_regions %>%
  purrr::map(
    function(current_year_region) {
      # browser()
      curr_df <-
        nc_count_weeks_per_vsl_permit_year_compl_m_tot_p_sort_b_cnt_in_b_tot_p_y_r[[current_year_region]]
      
      curr_year_months <-
        curr_df %>%
        select(year_month) %>%
        unique() %>%
        as.data.frame()
      
      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # repeat the function for each year_month
        # see the function definition F2
        purrr::map(~ get_one_plot_by_month(curr_df,
                                    curr_year_month = .))
      
      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_region, list_of_plots)      
      return(res)
    }
  ) 

gg_month_nc_perc[[1]][[1]]
# "2022 both"
# gg_month_nc_perc[[5]][[5]]

super_title = "Percent distribution of non compliant vessels per year, month & region"

all_plots <-
  gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    super_title <- paste(super_title,
                         curr_year_reg_list[[1]])
    gridExtra::arrangeGrob(grobs =
                  curr_year_reg_list[[2]],
                top = super_title,
                ncol = 3) %>%
      return()
  })

# draw one plot to test
gridExtra::grid.arrange(all_plots[[5]])

# all plots per month to files ----
save_plots_list_to_pdf <-
  function(file_full_name,
           plots_list) {
    ggsave(
      file_full_name,
      plots_list,
      width = 20,
      height = 20,
      units = "cm"
    )
  }


gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    super_title <- paste(super_title,
                         curr_year_reg_list[[1]])
    
    # arrangeGrob creates an object to use later
    all_plots_curr_year_reg <-
      gridExtra::arrangeGrob(grobs =
                    curr_year_reg_list[[2]],
                  top = super_title,
                  ncol = 3)
    
    file_name_base <- paste0(curr_year_reg_list[[1]],
                        "_percent_distribution_per_month",
                        ".pdf")
    file_path <-
      r"(quantify_compliance\jun_9_2023_uniq_vsls\per_month)"
    
    # file.path adds the correct concatenation
    file_full_name <- file.path(my_paths$outputs,
                           file_path,
                           file_name_base)
    
    # see the function definition F2
    save_plots_list_to_pdf(file_full_name,
           all_plots_curr_year_reg)
  })

# [[1]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 both_percent_distribution_per_month.pdf"
# 
# [[2]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 gom_only_percent_distribution_per_month.pdf"
# 
# [[3]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2022 sa_only_percent_distribution_per_month.pdf"
# 
# [[4]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 both_percent_distribution_per_month.pdf"
# 
# [[5]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 gom_only_percent_distribution_per_month.pdf"
# 
# [[6]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_9_2023_uniq_vsls\\per_month/2023 sa_only_percent_distribution_per_month.pdf"

# test numbers ----
compl_clean_sa_vs_gom_m_int_c %>%
  filter(compliant_ == "NO") %>%
  filter(year_region == "2022 gom_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% dim()
# [1] 10  1
