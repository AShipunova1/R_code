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
  dplyr::mutate(month_name = format(year_month, "%B")) %>%
  dplyr::mutate(month_num = format(year_month, "%m"))

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

  #79 count(compliant_, year, permit_sa_gom)

## compl/nc per month ----
compl_data_sa_2022_m_short_compl_vs_nc_per_m <-
  compl_data_sa_2022_m_short %>%
  select(vessel_official_number,
         compliant_,
         overridden_,
         month_name,
         month_num) %>%
  unique() %>%
  add_count(compliant_, overridden_, month_name) %>%
  select(compliant_, month_name, month_num, n) %>%
  arrange(month_num) %>%
  unique()
# %>%
#   View()

# count total
compl_data_sa_2022_m_short_total_vsl_m_check <- 
  compl_data_sa_2022_m_short %>%
  # Applying group_by & summarise
  group_by(month_num) %>%
  summarise(count = n_distinct(vessel_official_number))

# View(compl_data_sa_2022_m_short_total_vsl_m_check)

compl_data_sa_2022_m_short_tot <-
  compl_data_sa_2022_m_short %>%
  # Applying group_by & summarise
  group_by(month_num) %>%
  mutate(tota_vsl_m = n_distinct(vessel_official_number)) %>% 
  ungroup()

View(compl_data_sa_2022_m_short_tot)

# check
# compl_data_sa_2022_m_short_tot %>% 
#     filter(month_num == "01") %>% 
#     select(tota_vsl_m) %>% 
#     unique()
# 1635

## Month: get nc vessel_ids ----
compl_data_sa_2022_m_short_nc_v <-
  compl_data_sa_2022_m_short %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number, month_num) %>%
  unique()

# check counts
# %>%
  # count(month_num, name = "nc_v_per_month")
# %>%
  # select(month_num, nc_v_per_month) %>%

## Month: get compl only vessel_ids ----

# df %>% group_by(a) %>% mutate(d = +(b %in% c))

View(compl_data_sa_2022_m_short)

compl_data_sa_2022_m_short %>%
  unique() %>%
  select(-permitgroupexpiration) %>%
  group_by(month_num) %>%
  tidyr::pivot_wider(names_from = vessel_official_number, values_from = compliant_) %>%
  View()
# Warning in View :
#   Values from `compliant_` are not uniquely identified; output will
# contain list-cols.
# • Use `values_fn = list` to suppress this warning.
# • Use `values_fn = {summary_fun}` to summarise duplicates.
# • Use the following dplyr code to identify duplicates.
#   {data} %>%
#   dplyr::group_by(permitgroupexpiration, month_name, month_num,
#   vessel_official_number) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L)

# check
# compl_data_sa_2022_m_short %>%
#   unique() %>%
#   dplyr::group_by(permitgroupexpiration,
#                   month_name,
#                   month_num,
#                   vessel_official_number) %>%
#   dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
#   dplyr::filter(n > 1L) %>%
#   View()

## get compl, no compl, or both per month ----
compl_data_sa_2022_m_short_is_compl_wide <-
  compl_data_sa_2022_m_short %>%
  unique() %>%
  dplyr::select(-permitgroupexpiration) %>%
  dplyr::group_by(month_num) %>%
  tidyr::pivot_wider(
    names_from = vessel_official_number,
    values_from = compliant_,
    # make it "NO_YES" if both
    values_fn = ~ paste0(sort(.x), collapse = "_")
  )

# View(compl_data_sa_2022_m_short_is_compl_wide)

### check compl_data_sa_2022_m_short_is_compl_wide ----
compl_data_sa_2022_m_short_is_compl_wide %>%
  arrange(month_num) %>%
  select(month_name, SC9087BU) %>%
  tail()
# 1 07        July       YES
# 2 08        August     NO_YES
# 3 09        September  NO
# 4 10        October    NO_YES
# 5 11        November   YES
# 6 12        December   YES

## check before ----
compl_data_sa_2022_m %>%
  select(vessel_official_number, compliant_, month_name, month_num) %>%
  unique() %>%
  filter(vessel_official_number == "SC9087BU") %>%
  arrange(month_num) %>%
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

# View(compl_data_sa_2022_m_short_is_compl_wide)
compl_data_sa_2022_m_short_is_compl <-
  compl_data_sa_2022_m_short_is_compl_wide %>%
  pivot_longer(
    cols = -c(month_name, month_num),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

names(compl_data_sa_2022_m_short_is_compl)

compl_data_sa_2022_m_short_is_compl_cnt <-
  compl_data_sa_2022_m_short_is_compl %>%
  ungroup() %>%
  select(-vessel_official_number) %>%
  add_count(month_name, is_compl_or_both,
            name = "count_by_m_c") %>%
  unique() %>%
  arrange(month_num)

  View(compl_data_sa_2022_m_short_is_compl_cnt)

## get total counts per month ----
compl_data_sa_2022_m_short_is_compl_cnt_tot <-
  compl_data_sa_2022_m_short_is_compl_cnt %>%
  group_by(month_num) %>%
  pivot_wider(names_from = is_compl_or_both, values_from = count_by_m_c) %>%
  mutate(total_vsls_m = sum(YES, NO, NO_YES),
         tot_not_compl_m = sum(NO, NO_YES))

names(compl_data_sa_2022_m_short_is_compl_cnt_tot)

# get percentage ----
compl_data_sa_2022_m_short_is_compl_cnt_tot_perc <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot %>%
  mutate(
    percent_yes = YES * 100 / total_vsls_m,
    percent_tot_not = tot_not_compl_m * 100 / total_vsls_m,
    percent_no = NO * 100 / total_vsls_m,
    percent_no_yes = NO_YES * 100 / total_vsls_m
  )

# write_csv(
#   compl_data_sa_2022_m_short_is_compl_cnt_tot_perc,
#   file.path(
#     my_paths$outputs,
#     r"(quantify_compliance\by_dual)",
#     "compl_data_sa_2022_m_short_is_compl_cnt_tot_perc.csv"
#   )
# )

plot(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc$month_num, compl_data_sa_2022_m_short_is_compl_cnt_tot_perc$percent_yes)

names(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc)

plot_perc_compl_per_m <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
  ungroup() %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_tot_not
             )) +
  geom_point(color = "red") +
  # geom_text(aes(label = paste0(round(percent, 1), "%")),
  #           position = position_stack(vjust = 0.5)) +
  labs(title = "% of non_compliant South Atlantic Only Permitted Vessels by month (2022)",
       x = "",
       y = "Percent") 

plot_perc_compl_per_m
# +
  # footnote
  # labs(caption = "Counted for unique vessels per month as (compliant only) * 100 / (total)") 
  geom_point(aes(x = month_name_order,
                 y = percent_no_yes,
             color = "yellow")) +
  geom_point(aes(x = month_name_order,
                 y = percent_no,
             color = "red"))

plot_perc_compl_per_m

names(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc)
# compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
#   ungroup() %>%
#   select(month_name, YES, tot_not_compl_m,  total_vsls_m, percent_yes) %>% View()

# check numbers
compl_data_sa_2022_m %>% 
        filter(year_month == "Jan 2022" & year_permit == "2022 sa_only") %>% select(vessel_official_number) %>% 
        unique() %>% 
        dim()
# 1635

# 932+703
# [1] 1635


names(compl_data_sa_2022_m_short_is_compl_cnt_tot_perc)
plot_perc_non_compl_per_m <-
  compl_data_sa_2022_m_short_is_compl_cnt_tot_perc %>%
  ungroup() %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num))) %>%
  ggplot(aes(x = month_name_order,
             y = percent_tot_not)) +
  geom_point(color = "red") +
  # geom_text(aes(label = paste0(round(percent, 1), "%")),
  #           position = position_stack(vjust = 0.5)) +
  labs(title = "% of compliant South Atlantic Only Permitted Vessels by month (2022)",
       x = "",
       y = "Percent") +
  # footnote
  labs(caption = "Counted for unique vessels per month as (compliant only) * 100 / (total)")

# do you see a progression through the months of 2022 of increasing non-compliance for SA vessels?
    
# check counts
# %>%
  # count(month_num, name = "nc_v_per_month")
# %>%
  # select(month_num, nc_v_per_month) %>%


compl_data_sa_2022_m_short_compl_vs_nc_per_m %>%
  mutate(month_name_order = fct_reorder(month_name,
                                        as.numeric(month_num)))




  ggplot()
  # x = fct_rev(fct_reorder(common_name,
  #                                  !!sym(count_field_name),
  #                                  .fun = max)),


      pivot_longer(
        cols = c(percent_compl,
                 percent_non_compl),
        names_to = "is_compliant",
        values_to = "percent"
      )
