# 2)
# do you see a progression through the months of 2022 of increasing non-compliance for SA vessels? ----
# 2022
# SA

library(grid)
source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

compl_clean_sa_vs_gom_m_int_c <- compl_clean_sa_vs_gom_m_int
# View(compl_clean_sa_vs_gom_m_int_c)

compl_data_sa_2022 <-
  compl_clean_sa_vs_gom_m_int_c %>%
  filter(year_permit == "2022 sa_only")

## add month only for plots ----
compl_data_sa_2022_m <-
  compl_data_sa_2022 %>%
  dplyr::mutate(month_name = format(year_month, "%b")) %>%
  dplyr::mutate(month_num = format(year_month, "%m"))

# get fewer fields ----
compl_data_sa_2022_m_short <-
  compl_data_sa_2022_m %>%
  select(vessel_official_number,
         compliant_,
         overridden_,
         # permitgroupexpiration,
         month_name,
         month_num)

# check
# compl_clean_sa_vs_gom_m_int_c %>%
#   select(permit_groupexpiration,  permitgroupexpiration) %>%
#   unique() %>%
#   str()
# permit_groupexpiration: chr [1:190] "06/30/2023" "08/31/2023" "09/30/2023" "05/31/2024" ...
#  $ permitgroupexpiration : POSIXct[1:190], format: "2023-06-30"

  #79 dplyr::count(compliant_, year, permit_sa_gom)

compl_data_sa_2022_m_short_compl_vs_nc_per_m %>% 
    select(compliant_, overridden_, month_name, month_num, compl_overr_v) %>%
  unique() %>% 
  View()

# count total ----
compl_data_sa_2022_m_short_total_vsl_m_check <- 
  compl_data_sa_2022_m_short %>%
  # Applying group_by & summarise
  dplyr::group_by(month_num) %>%
  summarise(count = n_distinct(vessel_official_number))

# View(compl_data_sa_2022_m_short_total_vsl_m_check)

compl_data_sa_2022_m_short_tot <-
  compl_data_sa_2022_m_short %>%
  # Applying group_by & summarise
  dplyr::group_by(month_num) %>%
  dplyr::mutate(tota_vsl_m = n_distinct(vessel_official_number)) %>% 
  dplyr::ungroup()

# View(compl_data_sa_2022_m_short_tot)

# check
# compl_data_sa_2022_m_short_tot %>% 
#     filter(month_num == "01") %>% 
#     select(tota_vsl_m) %>% 
#     unique()
# 1635

# check 2
names(compl_clean_sa_vs_gom_m_int_c)

compl_clean_sa_vs_gom_m_int_c %>%
  filter(year_month == "Jan 2022" &
           permit_sa_gom == "sa_only") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 1635

# compl/nc per month ----
# compl_data_sa_2022_m_short_compl_vs_nc_per_m <-

names(compl_data_sa_2022_m_short_tot)

compl_data_sa_2022_m_short_tot_ov <-
  compl_data_sa_2022_m_short_tot %>%
    dplyr::group_by(month_num) %>%
  dplyr::mutate(compl_overr = paste(compliant_, overridden_, sep = "_"))

# check
compl_clean_sa_vs_gom_m_int_c %>%
  filter(year_month == "Jan 2022" &
           permit_sa_gom == "sa_only" &
           compliant_ == "NO" &
           overridden_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 468   1


# add compl counts ----

## get compl, no compl, or both per month ----
names(compl_data_sa_2022_m_short_tot_ov)

compl_data_sa_2022_m_short_tot_ov_wide <-
  compl_data_sa_2022_m_short_tot_ov %>%
  unique() %>%
  dplyr::group_by(month_num) %>%
  tidyr::pivot_wider(
    names_from = vessel_official_number,
    values_from = compl_overr
  )

### check compl_data_sa_2022_m_short_is_compl_wide ----
compl_data_sa_2022_m_short_tot_ov %>% 
  filter(vessel_official_number == "SC9087BU") %>% View()

compl_data_sa_2022_m_short_tot_ov_wide %>% 
  dplyr::arrange(month_num) %>%
  select(month_name, SC9087BU) %>%
  filter(complete.cases(SC9087BU)) %>% 
  tail(10)

#    month_num month_name SC9087BU
#    <chr>     <chr>      <chr>   
#  1 06        June       YES_NO  
#  2 06        June       NO_YES  
#  3 07        July       YES_NO  
#  4 08        August     YES_NO  
#  5 08        August     NO_YES  
#  6 09        September  NO_YES  
#  7 10        October    YES_NO  
#  8 10        October    NO_YES  
#  9 11        November   YES_NO  
# 10 12        December   YES_NO  

# names(compl_data_sa_2022_m_short_tot_ov_wide) %>% 
#   head() %>% paste0(collapse = ", ")
# "compliant_, overridden_, month_name, month_num, tota_vsl_m, VI5498TB"

compl_data_sa_2022_m_short_tot_ov_long <-
  compl_data_sa_2022_m_short_tot_ov_wide %>%
  tidyr::pivot_longer(
    cols = -c(compliant_,
              overridden_,
              month_name,
              month_num,
              tota_vsl_m),
    values_to = "is_compl_overridden",
    names_to = "vessel_official_number"
  )

# View(compl_data_sa_2022_m_short_tot_ov_long)

# add compl counts
# compl_data_sa_2022_m_short_tot_ov_long %>%
#   select(month_name,
#          month_num,
#          tota_vsl_m,
#          is_compl_overridden) %>%
#   filter(complete.cases(is_compl_overridden)) %>%
#   dplyr::glimpse()

# compl_data_sa_2022_m_short_tot_ov_long %>% 
#   select(month_name,
#          month_num,
#          tota_vsl_m,
#          is_compl_overridden) %>%
#   filter(complete.cases(is_compl_overridden)) %>%
#     dplyr::group_by(month_num) %>%
#   summarise(n = n()) %>%
#   dplyr::mutate(Freq = n/sum(n)) %>% View()

# count uniq vessel ids with "no" for compl and "no" for "overidden?" per month ----
compl_data_sa_2022_m_short_tot_ov_cnt_c_o <-
  compl_data_sa_2022_m_short_tot_ov_long %>%
  filter(is_compl_overridden == "NO_NO") %>%
  dplyr::group_by(month_num) %>%
  dplyr::mutate(count_no_no = n_distinct(vessel_official_number)) %>% 
  dplyr::ungroup()

# names(compl_data_sa_2022_m_short_tot_ov_cnt_c_o)

# Fewer columns ----
compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no <-
  compl_data_sa_2022_m_short_tot_ov_cnt_c_o %>%
  select(-c(
    vessel_official_number,
    compliant_,
    overridden_,
    is_compl_overridden
  )) %>%
  unique()

names(compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no)

# add percentage to no_no ----
compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no_p <-
  compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no %>% 
  dplyr::mutate(percent_no_no = count_no_no * 100 / tota_vsl_m)

# plot no_no ----
# names(compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no_p)
compl_data_sa_2022_m_short_tot_ov_cnt_c_o_no_no_p %>%
  dplyr::arrange(month_num) %>%
  dplyr::mutate(month_name_tot = paste(month_name, tota_vsl_m)) %>%
  dplyr::mutate(month_name_order = fct_reorder(month_name_tot,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_no_no)) +
  geom_point(color = "red",
             cex = 10) +
  labs(title = "% of non_compliant and not overridden South Atlantic Only Permitted Vessels by month (2022)",
       x = "",
       y = "Percent") + 
  geom_text(aes(label = count_no_no))
            # ,
            # position = position_stack(vjust = 0.5)) 
# +
#   scale_x_discrete(labels = month_name_tot)

# Don't use overridden and use permitexp ----
## expire before_current_month by weeks ----
compl_data_sa_2022_m_exp_diff <-
  compl_data_sa_2022_m %>%
  dplyr::mutate(exp_w_end_diff =
           as.numeric(as.Date(permitgroupexpiration) - week_end + 1)) %>% 
  dplyr::mutate(exp_1_m = 
           dplyr::case_when(exp_w_end_diff <= 31 ~ "less_t_1m",
                     exp_w_end_diff > 31 ~ "more_t_1m"))

# select(permitgroupexpiration, week_end, exp_w_end_diff) %>% 
  # View()


# print_df_names(compl_data_sa_2022_m_exp_diff_short_wide_long)

# Problem: the same vessel is counted twice per month if in one week it was more and another - less than a month from expiration

# diff in month ----
compl_data_sa_2022_m_exp_diff_m <-
  compl_data_sa_2022_m %>%
  dplyr::mutate(exp_w_end_diff_m =
           interval(as.Date(year_month),
                    as.Date(permitgroupexpiration)) %/% months(1)) %>%
  dplyr::mutate(exp_1_m =
           dplyr::case_when(
             exp_w_end_diff_m <= 1 ~ "less_t_1m",
             exp_w_end_diff_m > 1 ~ "more_t_1m"
           ))

# interval(date_1, date_2) %/% months(1)    # Apply interval & months
  # select(permitgroupexpiration, year_month, exp_w_end_diff_m) %>% View()
# -6

# add total vessels per month ----
compl_data_sa_2022_m_exp_diff_m_tot <-
  compl_data_sa_2022_m_exp_diff_m %>%
  dplyr::group_by(month_num) %>%
  dplyr::mutate(distinct_vsls_m = n_distinct(vessel_official_number))

## fewer columns ----
compl_data_sa_2022_m_exp_diff_m_tot_short <-
  compl_data_sa_2022_m_exp_diff_m_tot %>% 
  select(vessel_official_number,
         compliant_,
         # overridden_,
         # permitgroupexpiration,
         exp_1_m,
         month_name,
         month_num,
         distinct_vsls_m)

## get compl, no compl, or both per month ----
compl_data_sa_2022_m_exp_diff_m_tot_short_wide <-
  compl_data_sa_2022_m_exp_diff_m_tot_short %>%
  dplyr::group_by(month_num, exp_1_m) %>%
  # can unique, because we are looking at vessels, not weeks
  unique() %>%
  tidyr::pivot_wider(
    names_from = vessel_official_number,
    values_from = compliant_,
    # make it "NO_YES" if both
    values_fn = ~ paste0(sort(.x), collapse = "_")
  ) %>% 
  dplyr::ungroup()

# View(compl_data_sa_2022_m_exp_diff_m_tot_short_wide)

### check compl_data_sa_2022_m_exp_diff_m_short_wide ----
compl_data_sa_2022_m_exp_diff_m_tot_short_wide %>%
  dplyr::arrange(month_num) %>%
  select(month_name, SC9087BU) %>%
  filter(complete.cases(SC9087BU)) %>% 
  tail(10)
#  5 Jul        YES     
#  6 Aug        NO_YES  
#  7 Sep        NO      
#  8 Oct        NO_YES  
#  9 Nov        YES     
# 10 Dec        YES     

## check before ----
compl_data_sa_2022_m %>%
  select(vessel_official_number, compliant_, month_name, month_num) %>%
  unique() %>%
  filter(vessel_official_number == "SC9087BU") %>%
  dplyr::arrange(month_num) %>%
  tail(10)
#  3 SC9087BU               YES        July       07
#  4 SC9087BU               NO         August     08
#  5 SC9087BU               YES        August     08
#  6 SC9087BU               NO         September  09
#  7 SC9087BU               YES        October    10
#  8 SC9087BU               NO         October    10
#  9 SC9087BU               YES        November   11
# 10 SC9087BU               YES        December   12
# correct

# print_df_names(compl_data_sa_2022_m_exp_diff_m_tot_short_wide, 6)
compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long <-
  compl_data_sa_2022_m_exp_diff_m_tot_short_wide %>%
  tidyr::pivot_longer(
    cols = -c(exp_1_m, month_name, month_num, distinct_vsls_m),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

# str(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long)

## get compl, no compl, or both per month with exp ----
compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt <-
  compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long %>%
  dplyr::group_by(month_num) %>%
  unique() %>%
  select(-vessel_official_number) %>%
  dplyr::add_count(month_name, exp_1_m, is_compl_or_both,
            name = "compl_or_not_cnt") %>% 
  unique() %>% 
  dplyr::ungroup()

# check compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt ----
compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt %>%
  filter(month_name == "Jan" &
           !(is_compl_or_both == "YES")) %>%
  dplyr::mutate(m_sum = sum(compl_or_not_cnt)) %>%
  dplyr::glimpse()
# $ exp_1_m          <chr> "more_t_1m", "more_t_1m", "less_t_1m"
# $ month_name       <chr> "Jan", "Jan", "Jan"
# $ month_num        <chr> "01", "01", "01"
# $ distinct_vsls_m  <int> 1635, 1635, 1635
# $ is_compl_or_both <chr> "NO", "NO_YES", "NO"
# $ compl_or_not_cnt <int> 517, 125, 61
# $ m_sum            <int> 703, 703, 703
# 703 non-c - correct

# View(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt)

## get total compl counts per month ----
# print_df_names(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt)
compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t <-
  compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt %>%
  dplyr::group_by(month_num, exp_1_m) %>%
  tidyr::pivot_wider(names_from = is_compl_or_both, values_from = compl_or_not_cnt) %>%
  select(-`NA`) %>% 
  replace(is.na(.), 0) %>% 
  dplyr::mutate(
    total_vsls_m_exp = sum(YES, NO, NO_YES),
    not_compl_m_exp = sum(NO, NO_YES)
  ) %>% 
  dplyr::ungroup()

compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t %>% 
  filter(month_name == "Jan") %>% 
  dplyr::glimpse()
# $ exp_1_m          <chr> "more_t_1m", "less_t_1m"
# $ month_name       <chr> "Jan", "Jan"
# $ month_num        <chr> "01", "01"
# $ distinct_vsls_m  <int> 1635, 1635
# $ YES              <int> 918, 14
# $ NO               <int> 517, 61
# $ NO_YES           <int> 125, 0
# $ total_vsls_m_exp <int> 1560, 75
# $ not_compl_m      <int> 642, 61
# 1560+75 == 1635
# What is a 100%? per expiration or total per month?

# View(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t)

# get percentage per month, exp ----
compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc <-
  compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t %>%
  dplyr::mutate(
    percent_yes = YES * 100 / total_vsls_m_exp,
    percent_tot_not = not_compl_m_exp * 100 / total_vsls_m_exp,
    percent_no = NO * 100 / total_vsls_m_exp,
    percent_no_yes = NO_YES * 100 / total_vsls_m_exp
  )

# write_csv(
#   compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc,
#   file.path(
#     my_paths$outputs,
#     r"(quantify_compliance\by_dual)",
#     "compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc.csv"
#   )
# )

plot(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc$month_num, compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc$percent_no)

print_df_names(compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc)

plot_perc_compl_per_m <-
  compl_data_sa_2022_m_exp_diff_m_tot_short_wide_long_compl_cnt_c_t_perc %>%
  dplyr::mutate(month_name_tot = paste0(month_name,
                                 " (",
                                 distinct_vsls_m,
                                 ")")) %>%
  dplyr::mutate(month_name_order = fct_reorder(month_name_tot,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_tot_not,
             color = exp_1_m)) +
  geom_point(cex = 12) +
  labs(title = "% of non_compliant South Atlantic Only Permitted Vessels by month and expiration (2022)",
       x = "",
       y = "Percent") +
  # change the Legend
  scale_color_discrete(name = "Permit exp",
                       labels = c("< 1 month", "> 1 month")) +
  # add "%" sign to Y ticks
  scale_y_continuous(labels = scales::label_percent(scale = 1))

plot_perc_compl_per_m

## w numbers ----
plot_perc_compl_per_m +
  geom_text(aes(label = total_vsls_m_exp),
            color = "black") +
  # footnote
  labs(caption = "Numbers on dots show total number of vessels per month / expiration. Numbers at month show total unique vessels per month.")

