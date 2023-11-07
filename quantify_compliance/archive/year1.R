# by Year: ----
## year add total ----
# (both compl. and not, a vsl can be in both)

add_total_cnt_in_gr <- function(my_df, group_by_col) {
  my_df %>%
    # group by per year and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate(total_vsl_y =
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered, "year_permit")

# check
compl_clean_sa_vs_gom_m_int_filtered_tot %>%
  select(year_permit, total_vsl_y) %>%
  unique()
#   year_permit   tota_vsl_m
#   <chr>              <int>
# 1 2022 sa_only        2178
# 2 2022 gom_dual       1495
# 3 2023 sa_dual        2236

# 1 2022 sa_only         2145
# 2 2022 gom_dual        1304
# 3 2023 sa_dual         2220

## expired or not? ----
end_of_2022 <- as.Date("12/31/2022", format = "%m/%d/%Y")

expired_or_not <- function(my_df) {
  my_df %>%
    # get difference in days
    dplyr::mutate(exp_w_end_diff_y =
                    as.numeric(as.Date(permitgroupexpiration) -
                                 end_of_2022)) %>%
    # create a column
    dplyr::mutate(
      perm_exp_y =
        dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                         exp_w_end_diff_y > 0 ~ "active")
    ) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_tot)

## count expiration by year, permit ----
count_expiration_by <- function(my_df, group_by_var) {
  my_df %>%
    dplyr::group_by_at(group_by_var) %>%
    # count distinct vessels per group
    dplyr::mutate(exp_y_tot_cnt = n_distinct(vessel_official_number)) %>%
    return()
}

group_by_var <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y,
                      group_by_var)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

## get compl_counts ----
### get compl, no compl, or both per year ----

get_compl_by <- function(my_df, group_by_for_compl) {
  my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = vessel_official_number,
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}

group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short,
               group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide)
# [1]    6 3662

### count compl, no compl, or both per year, permit, active status ----

count_by_cols <- function(my_df,
                          cols_names) {
  my_df %>%
    # turn back to a longer format, vessel ids in one column
    tidyr::pivot_longer(
      # all other columns are vessel ids, use them as names
      cols = !any_of(cols_names),
      values_to = "is_compl_or_both",
      names_to = "vessel_official_number"
    ) %>%
    return()
}

cols_names <-
  c("year_permit", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide,
                cols_names)

### get cnts for compl, no compl, or both per month with exp ----
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      unique() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by onther columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      unique() %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols <- c("year_permit", "perm_exp_y")
cols_to_cnt <- c("year_permit", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] 22  6

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
  dim()
# 0 OK
# unique() %>%
# dplyr::group_by(is_compl_or_both) %>%
# dplyr::mutate(sum_compl_or_not_cnt = sum(compl_or_not_cnt)) %>%
# dplyr::select(is_compl_or_both, sum_compl_or_not_cnt) %>%
# unique() %>%
# dplyr::glimpse()
# $ is_compl_or_both     <chr> "YES", "NO", "NO_YES"
# $ sum_compl_or_not_cnt <int> 890, 562, 727
# 890 + 562 + 727
# [1] 2179
# 0

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
  dim()
# dplyr::glimpse()
# 0 - OK

# check if a vessel permit is expired and not in the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dim()
# 0

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
# 1       1602 YES
# 2       1263 NO

compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  dplyr::mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  # dplyr::group_by(compliant_, perm_exp_y) %>%
  # dplyr::group_by(compliant_) %>%
  dplyr::group_by(perm_exp_y) %>%
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

# [1] "2023-08-26"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1694 YES        active
# 2       1694 NO         active
# 3        451 NO         expired
# 4        451 YES        expired
# 1694+451 = 2145

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration

add_total_cnts <-
  function(my_df, group_by_compl_cols, group_by_exp_cols) {
    my_df %>%
      # remove NAs
      dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
      dplyr::mutate(
        compl_or_not =
          dplyr::case_when(is_compl_or_both == "YES" ~
                             "compliant",
                           .default = "non_compliant")
      ) %>%
      dplyr::group_by_at(group_by_compl_cols) %>%
      # add counts by compliant
      dplyr::mutate(cnt_y_p_c = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      # add counts by permit expiration
      dplyr::group_by_at(group_by_exp_cols) %>%
      dplyr::mutate(cnt_y_p_e = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols1 <- c("year_permit", "compl_or_not")
group_by_cols2 <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt,
    group_by_cols1,
    group_by_cols2
  )

# check cnts
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
#   # remove NAs
#   filter(year_permit == '2022 gom_dual' & perm_exp_y == 'expired') %>% View()

## add percents of total ----
add_percents_of_total <-
  function(my_df, select_cols) {
    my_df %>%
      dplyr::select(all_of(select_cols)) %>%
      unique() %>%
      dplyr::mutate(perc_c_nc = cnt_y_p_c * 100 / total_vsl_y) %>%
      return()
  }

select_cols <- c(
  "year_permit",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 11  7

## red/green plots for compl vs. non compl vessels per year ----

title_permits <- data.frame(
  # title = c("SA Only", "GOM + Dual", "2023: SA + Dual"),
  title = c("2022: SA Only", "2022: GOM + Dual", "2023: SA + Dual"),
  year_permit = c("2022 sa_only",
                  "2022 gom_dual",
                  "2023 sa_dual"),
  second_part = c("Permitted Vessels",
                  "Permitted Vessels",
                  "Permitted Vessels")
)

# "Permitted SEFHIER Vessels"

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

    # 1st figure title: "SA Only Permitted Vessels (Total Permitted: 2178; Expired Permits: 472)"
    # 2nd figure title: "GOM + Dual Permitted Vessels (Total Permitted: 1495; Expired Permits: 303)"

    curr_title_permit <-
      title_permits %>%
      filter(year_permit == curr_year_permit)

    current_title <-
      paste(curr_title_permit$title,
             curr_title_permit$second_part)

    # current_title <-
    #   paste0(
    #     curr_title_permit$title,
    #     " ",
    #     curr_title_permit$second_part,
    #     " (Active Permits: ",
    #     active_permits$cnt_y_p_e,
    #     "; Expired Permits: ",
    #     expired_permits$cnt_y_p_e,
    #     ")"
    #   )

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
# gg_all_c_vs_nc_plots[[3]]

# 2022
# gg_all_c_vs_nc_plots[[1]]
# gg_all_c_vs_nc_plots[[2]]

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

# combine plots for 2022
grid.arrange(gg_all_c_vs_nc_plots[[1]],
             # gg_all_c_vs_nc_plots[[2]],
             top = main_title)

grid.arrange(gg_all_c_vs_nc_plots[[2]],
             top = main_title)

grid.arrange(gg_all_c_vs_nc_plots[[3]],
             top = main_title)

# Non compliant only ----

# start with the new data with expiration by year
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_permit_year_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   compliant_,
                   name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

# View(weeks_per_vsl_permit_year_compl_cnt)

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(vessel_official_number == "1000042" &
                  year == "2022") %>%
  dplyr::select(year,
                compliant_,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(year_permit == "2022 sa_only",
                compliant_ == "NO") %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()

head(nc_2022_sa_only_test)
# weeks_per_vessel_per…¹ total_weeks_per_vessel
# 1 VA9236AV 52 52
# 2 VA6784AD 24 24
# 3 VA4480ZY 44 44
# 4 SC9207BX 26 50
# 5 SC8907DF 14 40
# 6 SC8298DH 45 45

weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    year_permit == "2022 sa_only",
    compliant_ == "YES",
    vessel_official_number == "SC8907DF"
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# 26  40

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 185251     32

# test
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", year == "2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178
# 2145

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022",
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1289    Non compliant vsl
  dim()
# [1] 26466 non compliant weeks
# [1] 25662     1

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

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_permit,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

# str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,221 × 7] (S3: tbl_df/tbl/data.frame)
# $ weeks_per_vessel_per_compl: int [1:3221] 52 24 44 26 14 45 5 41 52 27 ...
# $ total_weeks_per_vessel    : int [1:3221] 52 24 44 50 40 45 41 45 52 52 ...
# $ percent_compl             : num [1:3221] 100 100 100 52 35 ...

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
# count in one bucket
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::count(percent_compl, year_permit,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 634
# 615

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::add_count(year_permit,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(year_permit,
                percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  unique() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# $ year_permit         : chr [1:4] "2022 sa_only" "2022 sa_only" "2022 sa_only" "2022 sa_only"
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 398 168 82 615
# $ total_per_y_r       : int [1:4] 1263 1263 1263 1263

# "2022 sa_only"
# 633+85+172+399
# [1] 1289

# $ cnt_v_in_bucket     : int [1:4] 399 171 85 634
# 399 + 171 + 85 + 634
# 1289
# correct

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  # cnt vessels per year, permit and compliance
  dplyr::add_count(year_permit,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1
# 1 0<= & <25%                         31.5
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.49
# 4 75<= & <=100%                      48.7

# 5) blue plots by year ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# "2022: % Non-Compliant GOM + Dual Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this]  "(Total Non-Compliant = 304 Vessels; Active Permits = 1192 Vessels)"
# "2022: % Non-Compliant SA Only Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this] "(Total Non-Compliant = 1289 Vessels; Active Permits = 1707 Vessels)"
# For plot 4:
# "2023: SA + Dual Permitted SEFHIER Vessels (Total Permitted: 2235 ; Total Noncompliant: 1628; Expired Permits: 1)"

blue_year_plot_titles <-
  data.frame(
    year_permit = c("2022 sa_only",
                    "2022 gom_dual",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    curr_title_y_p <- make_year_permit_label(curr_year_permit)

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      filter(year_permit == curr_year_permit)

    y_p_title <-
      paste0(
        curr_blue_year_plot_title$first_part,
        "Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        # " Vessels; Acitve permits = ",
        # active_permits$exp_y_tot_cnt,
        # "; Expired permits: ",
        # expired_permits$exp_y_tot_cnt,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 12))

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]]

## plot 2022 ----
ndash <- "\u2013"
super_title <- paste0(
  "2022: % Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

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

### common y axes ----
yleft <- textGrob("% per permit region",
                  # rotate
                  rot = 90,
                  gp = gpar(fontsize = 10))

p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  # remove individual x and y labels for each plot
  purrr::map(~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)

## SA23 ----

super_title <- "% of non-compliant vessels (2023)"


gridExtra::grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]],
                        top = super_title
                        )
                        # ,
                        # bottom = footnote
