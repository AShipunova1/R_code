# Quantify compliance
# Quantify program compliance for Gulf and dual Gulf/SA permitted vessels.

# Michelle Masi
# Some caveats I have run into, in trying to quantify - are that folks may be missing 1 of 100 reports (e.g.) and that makes them non-compliant at the time you pull the compliance report data
# proportion of trip_submitted
# 2022 - 90% compliance, but what about # of reports

# 2022
# dual + GOM vs. SA
# 2023
# dual + SA
library(grid)
source("~/R_code_github/quantify_compliance/quantify_compliance_start.R")

# remove 2023 gom_only ----
compl_clean_sa_vs_gom_m_int_filtered <-
  # from get_data
  compl_clean_sa_vs_gom_m_int %>%
  filter(!(year == '2023' & permit_sa_gom == "gom_only"))

# save vsl count for future checks ----

count_all_vessels <-
  compl_clean_sa_vs_gom_m_int %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels

count_not_gom23_vessels <-
compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int %>%
  select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  count(compliant_, year_permit) %>%
  arrange(year_permit, compliant_)
# vessels
#  NO         2022 gom_dual   304
#  YES        2022 gom_dual  1482
#  NO         2022 sa_only   1289
#  YES        2022 sa_only   1617
#  NO         2023 sa_dual   1628
#  YES        2023 sa_dual   2125

# by Year: ----
## year add total ----
# (both compl. and not, a vsl can be in both)

compl_clean_sa_vs_gom_m_int_filtered_tot <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  # group by per year and permit
  dplyr::group_by(year_permit) %>%
  # cnt distinct vessels in each group
  dplyr::mutate(total_vsl_y = 
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

# check
compl_clean_sa_vs_gom_m_int_filtered_tot %>%
  select(year_permit, total_vsl_y) %>%
  unique()
#   year_permit   tota_vsl_m
#   <chr>              <int>
# 1 2022 sa_only        2178
# 2 2022 gom_dual       1495
# 3 2023 sa_dual        2236

## expired or not? ----
end_of_2022 <- as.Date("12/31/2022", format = "%m/%d/%Y")
# str(end_of_2022)

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y <-
  compl_clean_sa_vs_gom_m_int_filtered_tot %>%
  # get difference in days
  dplyr::mutate(exp_w_end_diff_y =
           as.numeric(as.Date(permitgroupexpiration) -
                        end_of_2022)) %>%
  # create a column 
  dplyr::mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active"))

## count expiration by year, permit ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y %>%
  dplyr::group_by(year_permit, perm_exp_y) %>%
  # count distinct vessels per group
  dplyr::mutate(exp_y_tot_cnt = n_distinct(vessel_official_number))

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::select(vessel_official_number,
         year_permit,
         compliant_,
         total_vsl_y,
         perm_exp_y,
         exp_y_tot_cnt) %>%
  # can unique, because already counted
  unique()

## get compl_counts ----
### get compl, no compl, or both per year ----

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short %>%
  # group_by everything but
      dplyr::group_by_at(vars(-c("vessel_official_number", "compliant_"))) %>%
  # can unique, because we are looking at vessels, not weeks
  unique() %>%
  # more columns, a column per vessel
  tidyr::pivot_wider(
    names_from = vessel_official_number,
    values_from = compliant_,
    # make it "NO_YES" if both
    values_fn = ~ paste0(sort(.x), collapse = "_")
  ) %>%
  dplyr::ungroup()

# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide)

### count compl, no compl, or both per year, permit, active status ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide %>%
  # turn back to a longer format, vessel ids in one column
  pivot_longer(
    # all other columns are vessel ids, use them as a names
    cols = -c(year_permit, total_vsl_y, perm_exp_y, exp_y_tot_cnt),
    values_to = "is_compl_or_both",
    names_to = "vessel_official_number"
  )

# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long)

### get cnts for compl, no compl, or both per month with exp ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::group_by(year_permit, perm_exp_y) %>%
  unique() %>%
  # exclude vessel id
  dplyr::select(-vessel_official_number) %>%
  # count grouped by onther columns
  dplyr::add_count(year_permit, perm_exp_y, is_compl_or_both,
            name = "compl_or_not_cnt") %>%
  unique() %>%
  dplyr::ungroup()

# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)

#### check counts ----
# print_df_names(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] "year_permit, total_vsl_y, perm_exp_y, exp_y_tot_cnt, is_compl_or_both, compl_or_not_cnt"

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::select(year_permit, total_vsl_y, compl_or_not_cnt, is_compl_or_both) %>%
  dplyr::group_by(year_permit) %>%
  # get sums
  dplyr::mutate(sum_cnts = sum(compl_or_not_cnt)) %>%
  dplyr::filter(!total_vsl_y == sum_cnts) %>%
  unique() %>%
  dplyr::group_by(is_compl_or_both) %>%
  dplyr::mutate(sum_compl_or_not_cnt = sum(compl_or_not_cnt)) %>%
  dplyr::select(is_compl_or_both, sum_compl_or_not_cnt) %>%
  unique() %>%
  dplyr::glimpse()
# $ is_compl_or_both     <chr> "YES", "NO", "NO_YES"
# $ sum_compl_or_not_cnt <int> 890, 562, 727
# 890 + 562 + 727
# [1] 2179

### One vessel in 2 groups ----
# The number should be the same as the total number we got earlier. It is not, which means One vessel is in 2 perm_exp_y groups, has both expired and not expired permit in 2022.

# TODO: One vessel in 2 perm_exp_y - email
#   year_permit  tota_vsl_m sum_cnts
#   <chr>             <int> <int>
# 1 2022 sa_only       2178 2179
# ...
# https://stackoverflow.com/questions/51848578/how-to-find-values-shared-between-groups-in-a-data-frame
# "Or you can group by val and then check whether the number of distinct exp for that val is equal to the data frame level number of distinct exp"
# 

### check if a vessel is compliant and not at the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared = 
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::glimpse()
# 0 - OK

# check if a vessel permit is expired and not in the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared = 
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dplyr::glimpse()
# $ year_permit            <chr> "2022 sa_only", "2022 sa_only"
# $ total_vsl_y             <int> 2178, 2178
# $ perm_exp_y             <chr> "active", "expired"
# $ vessel_official_number <chr> "FL7825PU", "FL7825PU"
# $ is_compl_or_both       <chr> "NO", "YES"
# $ shared                 <lgl> TRUE, TRUE

# ERR: perm_exp_y             <chr> "active", "expired"
# ERR: is_compl_or_both       <chr> "NO", "YES"

test_FL7825PU <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(vessel_official_number == "FL7825PU")

data_overview(test_FL7825PU)
# permitgroupexpiration        2
# compliant_                   2
# permit_groupexpiration       2

test_FL7825PU %>%
  select(permit_sa_gom, year_permit, permitgroupexpiration) %>%
  unique()
#   permit_sa_gom year_permit  permitgroupexpiration
# 1 sa_only       2022 sa_only 2022-05-31 00:00:00
# 2 sa_only       2022 sa_only 2024-05-31 00:00:00
# TODO: redo all when fixed

### check total_vsl_y vs. sum_cnts (should be equal, see dbl FL7825PU above) ----
compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m = 
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1617 YES
# 2       1289 NO

compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
           as.numeric(as.Date(permitgroupexpiration) -
                        end_of_2022)) %>%
  mutate(perm_exp_y =
           case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  # group_by(compliant_, perm_exp_y) %>%
  # group_by(compliant_) %>%
  group_by(perm_exp_y) %>%
# 1707 + 472
# [1] 2179

  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1442 YES        active
# 2        887 NO         active
# 3        402 NO         expired
# 4        175 YES        expired
# YES: 1442+175
# [1] 1617
# NO: 887+402
# [1] 1289
# 1617+1289
# 2906

# today()
# [1] "2023-06-19"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1707 YES        active
# 2       1707 NO         active
# 3        472 NO         expired
# 4        472 YES        expired
# 1707 + 472
# 2179

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::mutate(
    compl_or_not =
      dplyr::case_when(is_compl_or_both == "YES" ~
                         "compliant",
                       .default = "non_compliant")
  ) %>%
  dplyr::group_by(year_permit, compl_or_not) %>%
  # add counts by compliant
  dplyr::mutate(cnt_y_p_c = sum(compl_or_not_cnt)) %>%
  dplyr::ungroup() %>%
  # add counts by permit expiration
  dplyr::group_by(year_permit, perm_exp_y) %>%
  dplyr::mutate(cnt_y_p_e = sum(compl_or_not_cnt)) %>%
  dplyr::ungroup()

# check cnts
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
#   # remove NAs
#   filter(year_permit == '2022 gom_dual' & perm_exp_y == 'expired') %>% View()

## add percents of total ----

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y %>%
  dplyr::select(year_permit,
         total_vsl_y,
         perm_exp_y,
         compl_or_not,
         cnt_y_p_c,
         cnt_y_p_e) %>%
  unique() %>%
  dplyr::mutate(perc_c_nc = cnt_y_p_c * 100 / total_vsl_y)

## red/green plots for compl vs. non compl vessels per year ----

gg_all_c_vs_nc_plots <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)
    
    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_permit)
    
    total_vsls <- unique(curr_df$total_vsl_y)
    
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()
    
    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()
    
    current_title <-
      paste0(
        y_r_title,
        " permitted (Total vsls: ",
        total_vsls,
        "; Active permits: ",
        active_permits$cnt_y_p_e,
        "; Expired permits: ",
        expired_permits$cnt_y_p_e,
        ")"
      )
    
    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc")
    
    return(one_plot)
    
  })

# 2023 plot
gg_all_c_vs_nc_plots[[3]]

main_title = "Percent unique compliant vs. non compliant vessels for 2022"

# combine plots for 2022
grid.arrange(gg_all_c_vs_nc_plots[[1]],
             gg_all_c_vs_nc_plots[[2]],
             top = main_title)

# Non compliant only ----

# start with the new data with expiration by year
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_permit_year_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  add_count(year_permit, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  add_count(year_permit, vessel_official_number, name = "total_weeks_per_vessel") %>%
  ungroup()

View(weeks_per_vsl_permit_year_compl_cnt)

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
 filter(vessel_official_number == "1000042" &
          year == "2022") %>%
 select(year, compliant_, weeks_per_vessel_per_compl, total_weeks_per_vessel) %>%
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  filter(year_permit == "2022 sa_only",
         compliant_ == "NO") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique()

glimpse(nc_2022_sa_only_test)
            # weeks_per_vessel_per…¹ total_weeks_per_vessel
# 1 VA9236AV 52 52
# 2 VA6784AD 24 24
# 3 VA4480ZY 44 44
# 4 SC9207BX 26 50
# 5 SC8907DF 14 40
# 6 SC8298DH 45 45

weeks_per_vsl_permit_year_compl_cnt %>%
  filter(year_permit == "2022 sa_only",
         compliant_ == "YES",
         vessel_official_number == "SC8907DF") %>%
  select(vessel_official_number,
         weeks_per_vessel_per_compl,
         total_weeks_per_vessel) %>%
  unique() %>%
  glimpse()
# 26  40

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

View(count_weeks_per_vsl_permit_year_compl_p)

# test
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
  glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc_percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,221 × 7] (S3: tbl_df/tbl/data.frame)
 # $ weeks_per_vessel_per_compl: int [1:3221] 52 24 44 26 14 45 5 41 52 27 ...
 # $ total_weeks_per_vessel    : int [1:3221] 52 24 44 50 40 45 41 45 52 52 ...
 # $ percent_compl             : num [1:3221] 100 100 100 52 35 ...

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  filter(percent_n_compl_rank == '75<= & <=100%') %>%
  filter(year_permit == "2022 sa_only") %>%
  count(percent_compl, year_permit,
        name = "amount_of_occurences") %>%
  arrange(desc(percent_compl)) %>%
  # View()
  count(wt = amount_of_occurences)
# 633
# [1] "2023-06-20"
# 634

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
    add_count(year_permit,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
   filter(year_permit == "2022 sa_only") %>%
      select(year_permit, percent_n_compl_rank, cnt_v_in_bucket) %>%
      unique() %>%
  add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  arrange(percent_n_compl_rank) %>%
    str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# "2022 sa_only"
# 633+85+172+399
# [1] 1289

# $ cnt_v_in_bucket     : int [1:4] 399 171 85 634
# 399 + 171 + 85 + 634
# 1289

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  add_count(year_permit, name = "vsls_per_y_r") %>%
  mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  filter(year_permit == "2022 sa_only") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1

# 5) plots ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  map(function(curr_year_permit) {
    # browser()
    total_non_compl_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_permit == curr_year_permit) %>%
      select(perc_vsls_per_y_r_b,
             percent_n_compl_rank,
             perc_labels,
             vsls_per_y_r) %>%
      unique()

    active_permits <- count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_permit == curr_year_permit) %>%
      filter(perm_exp_y == "active") %>%
      select(exp_y_tot_cnt)

  expired_permits <- count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      filter(year_permit == curr_year_permit) %>%
      filter(perm_exp_y == "expired") %>%
      select(exp_y_tot_cnt)

    
    curr_title_y_p <- make_year_permit_label(curr_year_permit)
    y_p_title <- paste0(curr_title_y_p,
                       " (Total non compliant vessels: ",
                       total_non_compl_df$vsls_per_y_r,
                       "; Acitve permits: ",
active_permits$exp_y_tot_cnt,
                       "; Expired permits: ",
expired_permits$exp_y_tot_cnt,
                       ")"
                       )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)

    "% of missing reports for non-compliant vessels"

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]]

# plot 2022 ----

super_title = "% of non-compliant vessels by permit (2022)"

# footnote = textGrob(
#   "X axes is % of missing reports for non-compliant vessels",
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -1.5,
#   just = "right",
#   x = 0.01, y = 0.99,
#   vjust = 1
# )

# grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[1]],
#              gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]],
#              top = super_title,
#              bottom = footnote)

### common y axes
yleft <- textGrob("% per permit", rot = 90, gp = gpar(fontsize = 10))
p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  map( ~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)

## SA23 ----

super_title = "% of non-compliant vessels (2023)"


grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]],
             top = super_title
             # ,
             # bottom = footnote
             )

# Per month, region ----
# super_title_per_m = "% non-compliant weeks per month for non-compliant vessels by permit type (2022)"

# by Month: ----
## add tot cnts per month, permit ----

compl_clean_sa_vs_gom_m_int_filtered_tot_m <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  group_by(year_permit, year_month) %>%
  mutate(total_vsl_m = n_distinct(vessel_official_number)) %>%
  ungroup()

### test tot month ----
compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  filter(year == "2022") %>%
  select(year_permit, year_month, total_vsl_m) %>%
  arrange(year_month, year_permit) %>%
  unique() %>% 
  tail()
# 1 2022 gom_dual Oct 2022          1167
# 2 2022 sa_only  Oct 2022          1722
# 3 2022 gom_dual Nov 2022          1152
# 4 2022 sa_only  Nov 2022          1677
# 5 2022 gom_dual Dec 2022          1131
# 6 2022 sa_only  Dec 2022          1657

## add the difference between expiration and week_end ----

# if we use a week_end, than a vessel which ends near the end of year will have its last week expired.
compl_clean_sa_vs_gom_m_int_c_exp_diff <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  mutate(exp_w_end_diff =
           as.numeric(as.Date(permitgroupexpiration) - week_start + 1))

## expired or not? ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff %>%
  mutate(perm_exp_m =
           case_when(exp_w_end_diff < 0 ~ "expired",
                     exp_w_end_diff >= 0 ~ "active"))

## count exp by month  ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d %>%
  group_by(year_permit, year_month, perm_exp_m) %>%
  mutate(exp_m_tot_cnt = n_distinct(vessel_official_number)) %>%
  ungroup()

# check
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  filter(year == "2022") %>%
  select(year_permit,
         year_month,
         perm_exp_m,
         exp_m_tot_cnt,
         total_vsl_m) %>%
  unique() %>%
  arrange(year_permit, year_month) %>%
  tail()
  # year_permit  year_month perm_exp_m exp_m_tot_cnt total_vsl_m
#   <chr>        <yearmon>  <chr>              <int>       <int>
# 1 2022 sa_only Oct 2022   active              1721        1722
# 2 2022 sa_only Oct 2022   expired                1        1722
# 3 2022 sa_only Nov 2022   active              1676        1677
# 4 2022 sa_only Nov 2022   expired                1        1677
# 5 2022 sa_only Dec 2022   active              1656        1657
# 6 2022 sa_only Dec 2022   expired                1        1657
# compare with the text for tot month above
  
# cnt unique total vessels per month, compl ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  group_by(year_permit, year_month, compliant_) %>%
  mutate(cnt_vsl_m_compl = n_distinct(vessel_official_number)) %>%
  ungroup()

# print_df_names(compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt)

## test tot cnts per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  unique() %>%
  filter(year_month == "Jan 2022") %>%
  glimpse()
# toc()
# $ year_month      <yearmon> Jan 2022, Jan 2022, Jan 2022, Jan 2022
# $ perm_exp_m      <chr> "active", "active", "active", "active"
# $ exp_m_tot_cnt   <int> 1635, 1635, 1192, 1192
# $ total_vsl_m     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760? It is more than total. TODO
  
# add counts of weeks per vessel by month, compl ----
count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  ungroup %>%
  add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_month %>% 
    # select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  # unique() %>%
  filter(year_month == "Dec 2022") %>%
  glimpse()
# Rows: 11,031
# $ compliant_                         <chr> "YES", "NO", "YES", "YES",…
# $ total_vsl_m                        <int> 1657, 1657, 1657, 1657, 16…
# $ perm_exp_m                         <chr> "active", "active", "activ…
# $ exp_m_tot_cnt                      <int> 1656, 1656, 1656, 1656, 16…
# $ cnt_vsl_m_compl                    <int> 1282, 434, 1282, 1282, 434…
# $ weeks_per_vessel_per_compl_m       <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…

# test
count_weeks_per_vsl_permit_year_compl_month %>%
filter(year_permit == "2022 sa_only" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

## 1) Month: percent compl weeks per vsl per month ----
# View(count_weeks_per_vsl_permit_year_compl_month)
count_weeks_per_vsl_permit_year_compl_m_p <-
  count_weeks_per_vsl_permit_year_compl_month %>%
  mutate(percent_compl_m =
           weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_per_compl_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_p %>%
  # year_permit == "2022 sa_only" &
  filter(year_month == "Dec 2022") %>% 
  filter(vessel_official_number == "NJ8126HN") %>%
  # compliant_ == "NO") %>%
  select(
    vessel_official_number,
    year_month,
    compliant_,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m
  ) %>%
  unique() %>%
  arrange(year_month) %>%
  glimpse()
# $ compliant_                         <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl_m       <int> 1, 3
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 25, 75

## 2a) Month: Only non-compl and fewer cols ----
# View(count_weeks_per_vsl_permit_year_compl_m_p)
count_weeks_per_vsl_permit_year_compl_m_p_nc <-
  count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    year_month,
    vessel_official_number,
    perm_exp_m,
    exp_m_tot_cnt,
    cnt_vsl_m_compl,
    total_vsl_m,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc)

## 2b) Month: get percentage "buckets" ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b <-
  # Use F2 to see the function definition
  get_p_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

### test 2, by month ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  filter(percent_n_compl_rank == '75<= & <=100%') %>%
  filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA9236AV") %>%
  add_count(percent_compl_m, year_permit,
        name = "amount_of_occurences") %>%
  arrange(desc(percent_compl_m)) %>%
  add_count(wt = amount_of_occurences) %>%
  View()
# $ amount_of_occurences         <int> 12, 12, 12, 12, 12, 12, 12, 12…
# $ n                            <int> 144, 144, 144, 144, 144, 144, …

## 3a) Month: count how many in each bucket ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
    add_count(year_permit,
              year_month,
              percent_n_compl_rank,
              name = "cnt_v_in_bucket")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

# check
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  group_by(year_permit,
           year_month,
           percent_n_compl_rank) %>%
  mutate(cnt_v_in_bucket1 =
           dplyr::n_distinct(vessel_official_number)) %>%
  filter(!(cnt_v_in_bucket == cnt_v_in_bucket1)) %>%
  dim()
# 0 - correct

## 3b) Month: count total in each bucket ?----
# count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot <-
#   count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
#   # select(-vessel_official_number
#     # year_month,
#     # year_permit,
#     # perm_exp_m,
#     # exp_m_tot_cnt,
#     # percent_n_compl_rank,
#     # cnt_v_in_bucket
#   # ) %>%
#   # unique() %>%
#   # add_count(year_month, year_permit, wt = cnt_v_in_bucket,
#             # name = "tot_v_per_m_y_r")
#   group_by(year_permit, year_month, compliant_, percent_n_compl_rank) %>% 
#   mutate(tot_v_per_m_y_r1 = dplyr::n_distinct(vessel_official_number)) %>% 
#   select(-vessel_official_number) %>% 
#   unique()

### tests 3, by month ----
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot)

compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>% str()
# total 703 nc vsls in "Jan 2022 sa_only"
# tot 1635 in Jan 2022

# 45 nc vsls in "Jan 2022 gom_dual"
# 45 * 100 / 1192 = 3.8%

# still true
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% dim()
#  703 1

## 4) Month: cnt percents of (3) ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / cnt_vsl_m_compl) %>%
  mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 1), "%"))

### test 4, by month ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Dec 2022") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  arrange(percent_n_compl_rank) %>%
  head()
# Dec 2022
# 1 25<= & <50%                         2.30
# 2 50<= & <75%                         4.15
# 3 75<= & <=100%                      93.5 

# Jan 2022
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0<= & <25%                          4.69
# 2 25<= & <50%                         4.13
# 3 50<= & <75%                         4.13
# 4 75<= & <=100%                      87.1

# 612*100/703 == 87.05548

# keep only fields needed to plot ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  unique()
  
# add column with Month name only (for plotting)
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short %>% 
  # remove a space and following digits
  mutate(month_only = str_replace(year_month, " \\d+", ""))

# check
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
# [1] 11766    15
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)
# [1] 107  12
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)

## 5) Month plots ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short$year_permit))

sorted_year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2022 sa_only"  "2023 sa_dual"

get_year_permit_titles <- function(permit, year) {
      paste0("% of non-compliant ",
             permit,
             " Permitted vessels by month",
             " (", year, ")"
             ) %>%
    return()
}

year_permit_titles <-
  data.frame(
    super_title_gom = get_year_permit_titles("Gulf + Dual", "2022"),
    super_title_sa = get_year_permit_titles("South Atlantic Only", "2022"),
    super_title_2023 = get_year_permit_titles("South Atlantic + Dual", "2023")
  )

names(year_permit_titles) <- sorted_year_permits

# TODO: simplify
get_expired_permit_numbers <- function(curr_data) {
  # browser()

  exp_filt <- curr_data %>%
    filter(perm_exp_m == "expired") %>% 
    unique()
  
  res = exp_filt$exp_m_tot_cnt
  
  if (dim(exp_filt)[1] == 0)
  {
      res = 0
  }
  
  return(res)
}

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)
    
    curr_month_name <- curr_data$month_only %>%
      unique()
    
    curr_year_permit <- curr_data$year_permit %>%
      unique()
    
    curr_tot_v_per_m_y_r <- curr_data$cnt_vsl_m_compl %>%
      unique()
    
    curr_m_tot_active <- curr_data %>%
      filter(perm_exp_m == "active") %>%
      select(exp_m_tot_cnt) %>%
      unique()

    # get_expired_permit_numbers
    cnt_expired <- get_expired_permit_numbers(curr_data) 

    curr_title <- paste0(
      curr_month_name,
      " (",
      curr_tot_v_per_m_y_r,
      " vsls; ",
      curr_m_tot_active$exp_m_tot_cnt,
      " act. p.; ",
      cnt_expired,
      " exp. p.)"
    )
    
    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           x = "",
           y = "") +
      # y = "% nc vsls") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100) +
      theme(plot.title = element_text(size = 10))
    
    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_permits %>%
  map(
    function(current_year_permit) {
      # browser()
      curr_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[[current_year_permit]]

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
        map(~ get_one_plot_by_month(curr_df,
                                    curr_year_month = .))

      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    }
  )

# check
# test_df <-
#   count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[["2022 sa_only"]]
# 
# get_one_plot_by_month(test_df,
#                       curr_year_month = "Aug 2022")

# gg_month_nc_perc[[1]][[2]]

footnote_text <- "In parenthesis are 1) # of non compliant vessels per month; 2) total active permits per month; 3) total expired permits per month;"

footnote <- textGrob(
  footnote_text,
  gp = gpar(fontface = 3, fontsize = 10),
  # justify left
  # hjust = 0,
  hjust = -0.5,
  # just = "right",
  x = 0.01,
  y = 0.99,
  vjust = 1
)

### common axes for Months ----
y_left <- textGrob("% per 'bucket'", 
                  rot = 90, 
                  gp = gpar(fontsize = 10))

x_bottom <- textGrob("'buckets' - distibution of % of non compliant weeks per vessel", 
                  gp = gpar(fontsize = 10))

all_plots_w_titles_list <-
  gg_month_nc_perc %>%
  # repeat for each entry
  map(function(curr_year_reg_list) {
    
    # browser()
    curr_year_permit <- curr_year_reg_list[[1]]
    
    curr_super_title <- year_permit_titles[[curr_year_permit]]
    
    all_plots_per_year_region <-
      gridExtra::arrangeGrob(
        grobs =
          curr_year_reg_list[[2]],
        top = paste0(curr_super_title, "\n", footnote_text),
        left = y_left,
        bottom = x_bottom,
        # footnote,
        ncol = 3
      )
    
    res <- list(curr_year_permit,
             all_plots_per_year_region)
    
    return(res)
  })

# warnings()
# ...
# 22: Removed 1 rows containing missing values (`geom_text()`).
# 23: Removed 1 rows containing missing values (`geom_col()`).

# draw one plot to test
gridExtra::grid.arrange(all_plots_w_titles_list[[2]][[2]])

# View(all_plots_w_titles_list)

## all plots per month to files ----
save_plots_list_to_files <-
  function(file_full_name,
           plots_list) {
    ggplot2::ggsave(
      file_full_name,
      plots_list,
      width = 30,
      height = 20,
      units = "cm"
    )
  }

all_plots_w_titles_list %>%
  purrr::map(function(curr_plot_list) {
    file_name_base <- paste0(curr_plot_list[[1]],
                             "_percent_distribution_per_month",
                             # ".pdf")
                             ".png")
    
    file_path <-
      r"(quantify_compliance\jun_21_2023\per_month)"
    
    # file.path adds the correct concatenation
    file_full_name <- file.path(my_paths$outputs,
                                file_path,
                                file_name_base)
    
    # see the function definition F2
    save_plots_list_to_files(file_full_name,
                             curr_plot_list[[2]])
  })

# [[1]]
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance\\jun_21_2023\\per_month/2022 gom_dual_percent_distribution_per_month.png"

# TODO: Clean up ----
# test numbers ----
compl_clean_sa_vs_gom_m_int_c %>%
  filter(compliant_ == "NO") %>%
  filter(year_permit == "2022 gom_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% dim()
# [1] 10  1
# ===
# 1)
# "% Non-Compliant Vessels in Jan 2022 (12345 permitted; 125 expired permits)". I realize that is a long title, so perhaps we can push the % non-compliant vessels to the main title, and those smaller titles over the figure could just start at "jan...". Having the # of expired permits (compared to the # of permits) in each figure would better explain if they haven't tried to renew, and therefore haven't had to submit reports in order to renew. That is pretty much our only means to get them to comply, in the SA.

# View(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)

# TODO: 1) get buckets for the blue plot
# see make_one_plot_compl_vs_non_compl
# $ year_permit       <chr> "2022 gom_dual"
# $ total_vsl_y        <int> 1495
# $ perm_exp_y        <chr> "active"
# $ not_compl_cnt_y_p <int> 304
# $ not_compl_cnt_e   <int> 281
# $ perc_non_compl_y            <dbl> 20.33445

# plot
gg_compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_perc <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_non_compl_perc_act$year_permit %>%
  unique() %>%
  sort() %>%
  map(function(curr_year_permit) {
    browser()
    total_non_compl_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      filter(year_permit == curr_year_permit) %>%
      unique()
    curr_title_y_p <- make_year_permit_label(curr_year_permit)
    active_compl <-
    total_non_compl_df %>%
    filter(is_compl_or_both == "YES") %>%
    filter(perm_exp_y == "active") %>%
    select(compl_or_not_cnt)

    y_p_title <- paste0(curr_title_y_p,
                       " (Total non compliant vessels: ",
                       total_non_compl_df$vsls_per_y_r,
                       "; Compl. vsls. with active permits: ",
                       active_compl,
                       ")"
                       )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      ylim(0, 100)

    "% of missing reports for non-compliant vessels"

    return(one_plot)
  })



### test ----
all_compl_vs_non_compl_per_year_cnt_list[["2022 sa_only"]] %>%
  unique() %>% dim()
# [1] 2906 3

# If a vessel was non-compliant even once during a year, it is non_compliant for that year.
# remove non-compl vessels from compliant, to count each vessel once per year
# total unique vessels number vs. non-compl vessels


### test ----
compl_only %>%
  filter(year_permit == "2022 sa_only") %>%
  dim()
# 889  3

## test

compl_only %>%
  filter(year_permit == "2022 sa_only") %>%
  count(compliant_)
# YES 889

# 1 2022 gom_dual       1191
# 2 2022 sa_only         889
# 3 2023 sa_dual         608

# 1 2022 sa_only            1289
# 2 2022 gom_dual            304
# 3 2023 sa_dual            1628

#   year_permit   compl_vsls non_compl_vsls
# 1 2022 gom_dual       1191            304
# 2 2022 sa_only         889           1289
# 3 2023 sa_dual         608           1628

# $ year_permit           <chr> "2022 gom_dual", "2022 sa_…
# $ compl_vsls            <int> 1191, 889, 608
# $ non_compl_vsls        <int> 304, 1289, 1628
# $ total_vsl_ids_per_y_r <int> 1495, 2178, 2236

# % of non-compliant South Atlantic Only Permitted Vessels by month (2022)
# % of non-compliant Gulf + Dual permitted vessels by month (2022)

# TODO: keep only one legend
