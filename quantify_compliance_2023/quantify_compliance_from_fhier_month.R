# Per month, region ----
# super_title_per_m = "% non-compliant weeks per month for non-compliant vessels by permit type (2022)"

# by Month: ----
## add tot cnts per month, permit ----

compl_clean_sa_vs_gom_m_int_tot_m <-
  compl_clean_sa_vs_gom_m_int %>%
  dplyr::group_by(year_permit, year_month) %>%
  # count distinct vessels per group
  dplyr::mutate(total_vsl_m = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

### test tot month ----
compl_clean_sa_vs_gom_m_int_tot_m %>%
  dplyr::filter(year == "2023") %>%
  dplyr::select(year_permit, year_month, total_vsl_m) %>%
  dplyr::arrange(year_month, year_permit) %>%
  unique() %>%
  tail()
# 1 2022 gom_dual Oct 2022          1167
# 2 2023 sa_dual  Oct 2022          1722
# 3 2022 gom_dual Nov 2022          1152
# 4 2023 sa_dual  Nov 2022          1677
# 5 2022 gom_dual Dec 2022          1131
# 6 2023 sa_dual  Dec 2022          1657

# 2023
# 1 2023 gom_only Oct 2023           895
# 2 2023 sa_dual  Oct 2023          1963
# 3 2023 gom_only Nov 2023           897
# 4 2023 sa_dual  Nov 2023          1966
# 5 2023 gom_only Dec 2023           902
# 6 2023 sa_dual  Dec 2023          1969

compl_clean_sa_vs_gom_m_int_tot_m |>
  filter(year_permit == "2023 sa_dual") |> 
  select(year_month, total_vsl_m) |>
  distinct() |> 
  arrange(year_month) |> 
  head()
# 1 Jan 2023          1967
# 2 Feb 2023          1958
# 3 Mar 2023          1954
# 4 Apr 2023          1968
# 5 May 2023          2020
# 6 Jun 2023          2026

# ## add the difference between expiration and week_start----
# 
# # If we use a week_end, than a vessel which ends near the end of year will have its last week expired.
# compl_clean_sa_vs_gom_m_int_c_exp_diff <-
#   compl_clean_sa_vs_gom_m_int_tot_m %>%
#   # add a column with difference in days
#   dplyr::mutate(exp_w_end_diff =
#                   as.numeric(as.Date(permit_groupexpiration) - week_start + 1))
# 
# ## expired or not? ----
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d <-
#   compl_clean_sa_vs_gom_m_int_c_exp_diff %>%
#   dplyr::mutate(perm_exp_m =
#                   dplyr::case_when(exp_w_end_diff < 0 ~ "expired",
#                             exp_w_end_diff >= 0 ~ "active"))
# 
# # View(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
# 
## Keep active only ----
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp <-
#   compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
#   filter(perm_exp_m == "active")
# 
# dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
# # [1] 185251     28
# # [1] 143767     27
# 
# dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp)
# [1] 185199     28
# [1] 143737     27

## expired: count if vessel is expired or not by year, permit and month  ----
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt <-
#   compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp %>%
#   dplyr::group_by(year_permit, year_month, perm_exp_m) %>%
#   # add a column counting distinct vessels per group
#   dplyr::mutate(exp_m_tot_cnt = n_distinct(vessel_official_number)) %>%
#   dplyr::ungroup()

# check
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
#   dplyr::filter(year == "2023") %>%
#   dplyr::select(year_permit,
#                 year_month,
#                 perm_exp_m,
#                 exp_m_tot_cnt,
#                 total_vsl_m) %>%
#   unique() %>%
#   dplyr::arrange(year_permit, year_month) %>%
#   tail() |>
#   head()

#   year_permit  year_month perm_exp_m exp_m_tot_cnt total_vsl_m
#   <chr>        <yearmon>  <chr>              <int>       <int>
# 1 2023 sa_dual Jul 2023   active              2035        2036
# 2 2023 sa_dual Aug 2023   active              2022        2023
# 3 2023 sa_dual Sep 2023   active              1985        1986
# 4 2023 sa_dual Oct 2023   active              1962        1963
# 5 2023 sa_dual Nov 2023   active              1965        1966
# 6 2023 sa_dual Dec 2023   active              1968        1969

# from now on use exp_m_tot_cnt instead of total_vsl_m
# For 2023 use all

#### how many are expired ----
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
#   filter(perm_exp_m == "expired") |>
#   select(perm_exp_m, exp_m_tot_cnt) |>
#   dplyr::distinct()
# 1 expired                1
# 0

# compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
#   # filter(perm_exp_m == "expired" &
#   #          !year_month == "Dec 2023") |>
#   # dplyr::glimpse()
#   filter(vessel_official_number == "1000164" &
#            year_month == "Nov 2023") |>
#   dim()
# # 0

#### check if expired and active permit is in the same month
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
#   dplyr::group_by(vessel_official_number, year_month) |>
#   mutate(active_or_expired = paste(sort(unique(perm_exp_m)),
#                                    collapse = " & ")) |>
#   filter(grepl("&", active_or_expired)) |>
#   dim()
  # 0

## cnt distinct total vessels per year, permit, month, compl ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d__compl_cnt <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d %>%
  dplyr::group_by(year_permit, year_month, compliant_) %>%
  dplyr::mutate(cnt_vsl_m_compl = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

### test tot cnts per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_c_exp_diff_d__compl_cnt %>%
  dplyr::select(
    permit_sa_gom,
    year_permit,
    year_month,
    total_vsl_m,
    compliant_,
    cnt_vsl_m_compl
  ) %>%
  unique() %>%
  dplyr::filter(year_month == "Jan 2023") %>%
  dplyr::glimpse()
# toc()
# $ year_month      <yearmon> Jan 2022, Jan 2022, Jan 2022, Jan 2022
# $ perm_exp_m      <chr> "active", "active", "active", "active"
# $ exp_m_tot_cnt   <int> 1635, 1635, 1192, 1192
# $ total_vsl_m     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760 is more than total. Some vessels can be both in a month, if compliance differs by week. For this analysis I used vessels having at least one week in the month  non-compliant.
# If we are going to use "yes only" than redo "yes, no, no_yes" division as for a year above.
# $ cnt_vsl_m_compl <int> 1052, 688, 1004, 42

# 2023:
# $ year_month      <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023, Jan 2023, …
# $ perm_exp_m      <chr> "active", "active", "active", "active", "active", "act…
# $ exp_m_tot_cnt   <int> 1967, 1967, 1967, 675, 1967, 675
# $ total_vsl_m     <int> 1967, 1967, 1967, 675, 1967, 675
# $ compliant_      <chr> "YES", "NO", "YES", "YES", "NO", "NO"
# $ cnt_vsl_m_compl <int> 1693, 322, 1693, 675, 322, 1

## Month: percent compl vessels per per month ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_month)

## add counts of weeks per vessel by month, compl ----
count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d__compl_cnt %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  ungroup %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  # select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  # unique() %>%
  filter(year_month == "Dec 2023") %>%
  glimpse()
# Rows: 11,369 2023
# Rows: 11,373 (with expired)

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
  filter(year_permit == "2023 sa_dual" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  dplyr::glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

# 2023
# $ vessel_official_number       <chr> "VA9447ZY", "VA8261ZY", "VA2031CK", "VA14…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "NO", "NO",…
# $ year_month                   <yearmon> Dec 2023, Dec 2023, Dec 2023, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 2, 4, 2, 2, 4, 4, 2, 4, 4, 4,…


## 1) Month: percent compl weeks per vsl per month ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_month)

count_weeks_per_vsl_permit_year_compl_m_p <-
  count_weeks_per_vsl_permit_year_compl_month %>%
  mutate(percent_compl_weeks_m =
                  weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_per_compl_m)

# View(count_weeks_per_vsl_permit_year_compl_m_p)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(year_month == "Dec 2023") %>%
  filter(vessel_official_number == "1026988") %>%
  select(
    vessel_official_number,
    year_month,
    compliant_,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_weeks_m
  ) %>%
  unique() %>%
  arrange(year_month) %>%
  glimpse()

# $ compliant_                         <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl_m       <int> 1, 3
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 25, 75

# 2023
# $ compliant_                         <chr> "NO", "YES"
# $ weeks_per_vessel_per_compl_m       <int> 3, 1
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 75, 25

# OLD ----

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
    # total_vsl_m,
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

### check 2, by month ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  filter(percent_n_compl_rank == "75<= & <=100%") %>%
  filter(year_permit == "2023 sa_dual" &
           vessel_official_number == "VA9236AV") %>%
  dplyr::add_count(percent_compl_m, year_permit,
                   name = "amount_of_occurences") %>%
  # sort in the descending order
  dplyr::arrange(desc(percent_compl_m)) %>%
  # sum
  dplyr::add_count(wt = amount_of_occurences) %>%
  dplyr::glimpse()
# $ amount_of_occurences         <int> 12, 12, 12, 12, 12, 12, 12, 12…
# $ n                            <int> 144, 144, 144, 144, 144, 144, …

### add 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 <-
  # Use F2 to see the function definition
  get_2_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b2)

## 3) Month: count how many in each bucket ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)
# [1] 11489    12
# [1] 11477    12
# [1] 6503   11

### 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_non_compl_2_buckets,
                   name = "cnt_v_in_bucket2")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
# check by counting in a different way
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  dplyr::group_by(year_permit,
                  year_month,
                  percent_n_compl_rank) %>%
  dplyr::mutate(cnt_v_in_bucket1 =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::filter(!(cnt_v_in_bucket == cnt_v_in_bucket1)) %>%
  dim()
# 0 - correct, no difference

### tests 3, by month ----
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot)

test_compare_with <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(year_permit == "2023 sa_dual") %>%
  filter(year_month == "Jan 2023") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# total 703 nc vsls in "Jan 2023 sa_dual"
# tot 1635 in Jan 2023
#
# 45 nc vsls in "Jan 2023 gom_dual"
# 45 * 100 / 1192 = 3.8%

# 688

# 322 (2023)

# still true?
test_res <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  filter(year_permit == "2023 sa_dual") %>%
  filter(year_month == "Jan 2023") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
#  703 1

test_compare_with[1] == test_res[1]
# TRUE

## 4) Month: cnt percents of (3) ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### 2 buckets ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_m_b2 = cnt_v_in_bucket2 * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_m_b2, 0), "%"))

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p)

### test 4, by month ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  filter(year_permit == "2023 sa_dual") %>%
  filter(year_month == "Dec 2023") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
# Dec 2023
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

# 2023
# 1 25<= & <50%                        10.4 
# 2 50<= & <75%                         9.75
# 3 75<= & <=100%                      79.9 

## 5) Month plots ----

## 5a) prepare the df for plotting ----
### keep only fields needed to plot ----

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
  dplyr::distinct()

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short <-
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p |> 
    select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  dplyr::distinct()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)

### add column with Month name only (for plotting) ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

# check
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
# [1] 11766    15
# [1] 11489    15
# [1] 11477    15
# [1] 6503   14 (2023)
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)
# [1] 107  12
# [1] 95 12
# [1] 42 11 (2023)
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)
# [1] 58 10
# [1] 26 10 (2023)

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)

### split the df by year_permit into a list ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short$year_permit))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2 <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short$year_permit))

### get used year_permits ----
sorted_year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2023 sa_dual"  "2023 sa_dual"
# [1] "2023 gom_only" "2023 sa_dual" 

### make titles ----
get_year_permit_titles <- 
  function(permit, year) {
  str_glue("% of non-compliant {permit} Permitted vessels by month ({year})")
}

year_permit_titles <-
  data.frame(
    super_title_gom = get_year_permit_titles("Gulf Only", "2023"),
    # super_title_sa = get_year_permit_titles("South Atlantic Only", "2023"),
    super_title_sa = get_year_permit_titles("South Atlantic + Dual", "2023")
  )

names(year_permit_titles) <- sorted_year_permits

### additional functions for Month plots ----
# TODO: simplify
# returns 0 or number of expired permits
get_expired_permit_numbers <- function(curr_data) {
  # browser()

  exp_filt <- curr_data %>%
    filter(perm_exp_m == "expired") %>%
    unique()

  res <- exp_filt$exp_m_tot_cnt

  # if filter(perm_exp_m == "expired") returned nothing
  if (dim(exp_filt)[1] == 0) {
    res <- 0
  }

  return(res)
}

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)

    curr_month_name <- unique(curr_data$month_only)

    curr_year_permit <- unique(curr_data$year_permit)

    curr_tot_v_per_m_y_r <- unique(curr_data$cnt_vsl_m_compl)

    curr_m_tot_active <- curr_data %>%
      filter(perm_exp_m == "active") %>%
      select(exp_m_tot_cnt) %>%
      unique()

    # see function definition F2
    cnt_expired <- get_expired_permit_numbers(curr_data)

    curr_title <- paste0(
      curr_month_name,
      " (",
      curr_tot_v_per_m_y_r,
      " total non-compliant vsls)"
      )
    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           # no labels for axes
           x = "",
           y = "") +
      # text on each bar
      geom_text(aes(label = perc_labels),
                # posintion - middle
                position = position_stack(vjust = 0.5)) +
      # Y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 10))

    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_permits %>%
  purrr::map(
    # for each year and permit pull a df from the list
    function(current_year_permit) {
      # browser()
      curr_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[[current_year_permit]]

      curr_year_months <-
        curr_df %>%
        dplyr::select(year_month) %>%
        unique() %>%
        as.data.frame()

      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # run the function for each month
        # see the function definition F2
        purrr::map(~ get_one_plot_by_month(curr_df,
                                            curr_year_month = .))
      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    })

footnote_text <- "In parenthesis are 1) # of non compliant vessels per month; 2) total active permits per month; 3) total expired permits per month;"

# footnote <- textGrob(
#   footnote_text,
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -0.5,
#   # just = "right",
#   x = 0.01,
#   y = 0.99,
#   vjust = 1
# )

### common axes for Months ----
y_left <- textGrob("% per 'bucket'",
                   # angle
                   rot = 90,
                   gp = gpar(fontsize = 10))

x_bottom <-
  textGrob("'buckets' - distibution of % of non compliant weeks per vessel",
           gp = gpar(fontsize = 10))

all_plots_w_titles_list <-
  gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    # get a name
    curr_year_permit <- curr_year_reg_list[[1]]

    # get a title by the name
    curr_super_title <- year_permit_titles[[curr_year_permit]]

    # add a subtitle
    whole_title <- curr_super_title
    # whole_title <-
    #   paste0(curr_super_title,
    #          # new line
    #          "\n",
    #          footnote_text)

    all_plots_per_year_region <-
      gridExtra::arrangeGrob(
        grobs =
          curr_year_reg_list[[2]],
        top = whole_title,
        left = y_left,
        bottom = x_bottom,
        ncol = 3
      )

    # combine the current year_permit and the plots in a list
    res <- list(curr_year_permit,
                all_plots_per_year_region)

    return(res)
  })

# warnings()

# draw one plot to test
gridExtra::grid.arrange(all_plots_w_titles_list[[2]][[2]])

# View(all_plots_w_titles_list)

## all plots per month to files ----

# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\quantify_compliance\08_31_2023"
# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\quantify_compliance\2023-09-01\per_month"

# add dir
plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

all_plots_w_titles_list %>%
  # repeat for each element of the list
  purrr::map(function(curr_plot_list) {
    file_name_base <- paste0(curr_plot_list[[1]],
                             "_percent_distribution_per_month",
                             ".png")

    # file.path adds the correct concatenation
    file_full_name <- file.path(plot_file_path_m,
                                file_name_base)

    # see the function definition F2
    save_plots_list_to_files(file_full_name,
                             # plots
                             curr_plot_list[[2]])
  })

# "~/R_files_local/my_outputs/quantify_compliance/2023-09-01/per_month/2023 sa_dual_percent_distribution_per_month.png"

# [[1]]
# [1] "~/R_files_local/my_outputs/quantify_compliance\\08_26_2023\\per_month/2022 gom_dual_percent_distribution_per_month.png"...

