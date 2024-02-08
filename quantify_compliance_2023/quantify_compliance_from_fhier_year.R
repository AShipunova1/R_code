# by Year: ----
## year add total ----
# (both compl. and not, a vsl can be in both)

add_total_cnt_in_gr <- function(my_df, group_by_col) {
  my_df %>%
    # group by per year and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate(total_vsl_y_by_year_perm =
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

compl_clean_sa_vs_gom_m_int_tot <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int__join_metrics, 
                      c("permit_sa_gom_dual", "year"))

# check
res1 <-
  compl_clean_sa_vs_gom_m_int__join_metrics |>
  select(vessel_official_number, year, permit_sa_gom_dual) |>
  distinct() |>
  count(year, permit_sa_gom_dual, name = "total_vsl_y_by_year_perm") |>
  arrange(total_vsl_y_by_year_perm)

# res1
# 1 2022  dual                                    302
# 2 2023  dual                                    315
# 3 2022  gom_only                                977
# 4 2023  gom_only                               1147
# 5 2023  sa_only                                2177
# 6 2022  sa_only                                2231

res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(permit_sa_gom_dual, year, total_vsl_y_by_year_perm) |>
  distinct() |>
  arrange(total_vsl_y_by_year_perm)

diffdf::diffdf(res1, res2)
# T

## get vessel counts by compliance (compl_counts) ----
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

# all columns except...
group_by_for_compl <- 
  vars(-c("vessel_official_number", "compliant_"))

# print_df_names(compl_clean_sa_vs_gom_m_int_tot)

# using all fields
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide <-
  compl_clean_sa_vs_gom_m_int_tot |>
  dplyr::select(
    vessel_official_number,
    year,
    permit_sa_gom_dual,
    compliant_,
    total_vsl_y_by_year_perm
  ) |>
  dplyr::distinct() |>
  get_compl_by(group_by_for_compl)

# [1]    2 3374
names(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide) |> 
  head() |> 
  glimpse()
 # chr [1:6] "permit_sa_gom_dual" "total_vsl_y_by_year_perm" "VI5498TB" "VA9447ZY" ...
 
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
  c("year",
    "permit_sa_gom_dual",
    "total_vsl_y_by_year_perm",
    "perm_exp_y",
    "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long <-
  count_by_cols(
    compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide,
    c("year", "permit_sa_gom_dual", "total_vsl_y_by_year_perm")
  )

# check compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long |> 
  arrange(vessel_official_number) |> 
  head()
#   permit_sa_gom_dual   total_vsl_y_by_year_perm vessel_official_number is_compl_or_both
#   <chr>                            <int> <chr>                  <chr>           
# 2022  gom_only                           977  1000042                NO_YES
# 2023 gom_only                      1147 1000042                YES
# 2022  sa_only                           2231 1000042                NA

#### save sa_dual filter ----
sa_dual_filter <-
  rlang::quo(permit_sa_gom_dual == "sa_only" |
          (year == "2023" & permit_sa_gom_dual == "dual"))

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long |>
  filter(!!sa_dual_filter) |>
  select(vessel_official_number, is_compl_or_both) |>
  dplyr::distinct()

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa)
# [1] 5793    2
# [1] 3372    2 sa_dual
# [1] 6614    2 both years
# [1] 7677    2

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa$vessel_official_number)
# vessel_official_number 4016

n_distinct(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa$is_compl_or_both)
# 4
# compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa$is_compl_or_both |> unique()
# [1] "YES"    "NO"     "NO_YES" NA      

compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c <-
  compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa |>
  filter(is_compl_or_both %in% c("NO", "NO_YES"))

dim(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long_sa_non_c)
# 370 2
# [1] 1545    2 no_yes
# 1859    both years
# 2210    

### get cnts for compl, no compl, or both per month with exp ----
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      unique() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by other columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      unique() %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols <- c("year", "permit_sa_gom_dual")
cols_to_cnt <- c("year", "permit_sa_gom_dual", "is_compl_or_both")

# print_df_names(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long)
# [1] "year, permit_sa_gom_dual, total_vsl_y_by_year_perm, vessel_official_number, is_compl_or_both"

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long,
                 group_by_cols,
                 cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)
# [1] 21  7
# [1] 7 4 (no exp)
# 22 5 both years
# [1] 23  5

#### check counts ----
# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)
# [1] "permit_sa_gom_dual, total_vsl_y_by_year_perm, is_compl_or_both, compl_or_not_cnt"

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::select(year,
                permit_sa_gom_dual,
                total_vsl_y_by_year_perm,
                compl_or_not_cnt,
                is_compl_or_both) %>%
  dplyr::group_by(year, permit_sa_gom_dual) %>%
  # get sums
  dplyr::mutate(sum_cnts = sum(compl_or_not_cnt)) %>% 
  # dplyr::filter(!total_vsl_y_by_year_perm == sum_cnts) %>%
  ungroup() |>
  # nrow()
  # 0 OK
  unique() %>%
  dplyr::group_by(is_compl_or_both) %>%
  dplyr::mutate(sum_compl_or_not_cnt = sum(compl_or_not_cnt)) %>%
  dplyr::select(is_compl_or_both, sum_compl_or_not_cnt) %>%
  unique() %>%
  ungroup() |>
  dplyr::glimpse()
# $ is_compl_or_both     <chr> "YES", "NO", "NO_YES"
# $ sum_compl_or_not_cnt <int> 1752, 742, 2350
# $ sum_compl_or_not_cnt <int> 1825, 370, 1177 (2023)
# $ sum_compl_or_not_cnt <int> 4102, 845, 2202 (both years with processed)

### One vessel in 2 groups ----
# The number should be the same as the total number we got earlier. It is not, which means One vessel is in 2 perm_exp_y groups, has both expired and not expired permit in 2023.

### check if a vessel is compliant and not at the same time
# 'n_distinct(is_compl_or_both)' calculates the count of distinct values in the 'is_compl_or_both' column for each row.
# 'n_distinct(.$is_compl_or_both)' calculates the count of distinct values in the 'is_compl_or_both' column for the entire data frame.
# The equality comparison '== n_distinct(.$is_compl_or_both)' checks if the count of distinct values is the same for the entire data frame and each group.

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt)

# compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long %>%
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide_long |> 
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  ungroup() |>
  nrow()
# 0 - OK

# check if a vessel permit is expired and not in the same time
# compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long %>%
#   dplyr::filter(!is.na(is_compl_or_both)) %>%
#   dplyr::group_by(vessel_official_number) %>%
#   dplyr::mutate(shared =
#                   dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
#   dplyr::filter(shared == TRUE) %>%
#   dplyr::arrange(vessel_official_number) %>%
#   ungroup() |> 
#   dim()
# 2
# 1000164 is both exp and not

### check total_vsl_y_by_year_perm vs. sum_cnts (should be equal, see dbl FL7825PU) ----
compl_clean_sa_vs_gom_m_int %>%
  # dplyr::filter(!!sa_dual_filter) %>%
  dplyr::group_by(year, compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(year, tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
#   year  tota_vsl_m compliant_
#   <chr>      <int> <chr>     
# 1 2022        2959 YES       
# 2 2022        1390 NO        
# 3 2023        3177 YES       
# 4 2023        1558 NO        

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
      return()
  }

group_by_cols1 <- c("year", "permit_sa_gom_dual", "compl_or_not")
# group_by_cols2 <- c("permit_sa_gom_dual", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y <-
  add_total_cnts(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt,
                 group_by_cols1)

# glimpse(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y)

## add percents of total ----
add_percents_of_total <-
  function(my_df, select_cols) {
    my_df %>%
      dplyr::select(all_of(select_cols)) %>%
      distinct() %>%
      dplyr::mutate(perc_c_or_not = 100 * cnt_y_p_c / total_vsl_y_by_year_perm) %>%
      return()
  }

select_cols <- c(
  "year",
  "permit_sa_gom_dual",
  "total_vsl_y_by_year_perm",
  "compl_or_not",
  "cnt_y_p_c"
)

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y)

compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

# View(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc)

dim(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 11  8
# 7 8 (sa_dual)
# [1] 4 5 no exp
# [1] 12  6

## plots for compl vs. non compl vessels per year ----

# "Permitted SEFHIER Vessels"

gg_all_c_vs_nc_plots <-
  compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc$permit_sa_gom_dual %>%
  unique() %>%
  # repeat for each permit_sa_gom_dual and each year
  purrr::map(function(curr_permit_sa_gom_dual) {
    c(my_year1, my_year2) |>
      map(\(curr_year) {
        # browser()
        curr_df <-
          compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
          dplyr::filter(permit_sa_gom_dual == curr_permit_sa_gom_dual &
                          year == curr_year)
        
        # See function definition F2
        total_vsls <- unique(curr_df$total_vsl_y_by_year_perm)
        
        curr_title_permit <-
          title_permits %>%
          filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)
        
        current_title <-
          str_glue("{curr_year}: {curr_title_permit$title} {curr_title_permit$second_part}")
        
        one_plot <-
          curr_df %>%
          dplyr::select(compl_or_not, perc_c_or_not) %>%
          unique() %>%
          # See function definition F2
          make_one_plot_compl_vs_non_compl(current_title,
                                           is_compliant = "compl_or_not",
                                           percent = "perc_c_or_not")
        
        return(one_plot)
        
      })
  })

# View(gg_all_c_vs_nc_plots[[1]])
sa_only <- gg_all_c_vs_nc_plots[[1]]
dual <- gg_all_c_vs_nc_plots[[2]]
gom_only <- gg_all_c_vs_nc_plots[[3]]

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

my_grobs_list <- list(sa_only[[1]],
                       sa_only[[2]],
                       dual[[2]])

# combine plots for 2023
grid.arrange(grobs = my_grobs_list,
             top = main_title)

# see the function definition F2
my_grobs_list |>
  map(\(plot_name) {
    # browser()
    file_name_part <-
      plot_name$labels$title |>
      str_replace_all(" ", "_") |>
      str_replace("(^[0-9]+):_(.+)", "\\2_\\1") |>
      str_replace("_permitted_vessels", "") |>
      tolower()
    
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("compl_vs_nonc_plots_{file_name_part}.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             plot_name)
    
  })

# [1] "2024-02-07/compl_vs_nonc_plots_sa_only_permitted_vessels_2023.png"

# Non compliant only ----
# compl_clean_sa_vs_gom_m_int_tot |> print_df_names()

# start with the new data with expiration by year
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
# View(compl_clean_sa_vs_gom_m_int_tot)

weeks_per_vsl_permit_year_compl_cnt1 <-
  compl_clean_sa_vs_gom_m_int_tot %>%
  group_by(year,
           permit_sa_gom_dual) |> 
  dplyr::add_count(vessel_official_number,
                   compliant_,
                   name = "weeks_per_vessel_per_compl_year") %>%
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

# View(weeks_per_vsl_permit_year_compl_cnt1)

# check
compl_clean_sa_vs_gom_m_int_tot |>
  select(year, permit_sa_gom_dual, week_num, vessel_official_number, compliant_) |>
  distinct() |>
  add_count(year, permit_sa_gom_dual, vessel_official_number, compliant_) |>
  filter(!!sa_dual_filter, n == 2) |>
  glimpse()

  # dplyr::filter(!!sa_dual_filter,
  #               compliant_ == "NO") %>%


View(weeks_per_vsl_permit_year_compl_cnt)

## test 1a ----
# have both comp and not
weeks_per_vsl_permit_year_compl_cnt %>% 
  select(vessel_official_number, compliant_) |>
  distinct() |> 
    group_by(vessel_official_number) |> 
    filter(n_distinct(compliant_) > 1) |> 
    arrange(vessel_official_number) |> head()

weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(vessel_official_number == "1020822" &
                  year == "2023") %>%
  dplyr::select(year,
                compliant_,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()
# 1 2023  YES  47  52
# 2 2023  NO  5  52

# 1 2023  YES  160  208
# 2 2023  NO  48  208


# 1000042
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

nc_2023_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(!!sa_dual_filter,
                compliant_ == "NO") %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()

head(nc_2023_sa_only_test)
#   vessel_official_number weeks_per_vessel_per_compl total_weeks_per_vessel
#   <chr>                                       <int>                  <int>
# 1 VA9447ZY  11                     11
# 2 VA8261ZY  8                     52
# 3 VA2031CK  33                     33
# 4 VA1460CJ  50                     50
# 5 VA1267CJ  2                     48

weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    !!sa_dual_filter,
    # compliant_ == "YES",
    vessel_official_number == "SC8907DF"
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                compliant_,
                total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# $ weeks_per_vessel_per_compl <int> 42, 10
# $ compliant_                 <chr> "NO", "YES"
# $ total_weeks_per_vessel     <int> 52, 52

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
# glimpse(weeks_per_vsl_permit_year_compl_cnt)

count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  group_by(compliant_) |>
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel) |>
  ungroup()

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 185251     32
# [1] 143767     31 (2023)

# test
# count_weeks_per_vsl_permit_year_compl_p$permit_sa_gom_dual |>
#   unique()
# [1] "sa_only"  "dual"     "gom_only"

# count_weeks_per_vsl_permit_year_compl_p$permit_sa_gom_dual |>
#   unique()
# [1] "2023 sa_dual"  "2023 gom_only"

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year) %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178
# 2421 (2023)

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom_dual %in% c("sa_only", "dual"), 
         year == my_year,
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1289    Non compliant vsl
  # 1545 (2023)
  dim()
# [1] 26466 non compliant weeks
# 24302 (2023)

### test 1b ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2023") %>%
  select(
    year,
    permit_sa_gom_dual,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ year                       <chr> "2023", "2023"
# $ permit_sa_gom_dual              <chr> "sa_only", "sa_only"
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 47, 5
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 90.384615, 9.615385

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year,
    permit_sa_gom_dual,
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
  dplyr::filter(!!sa_dual_filter) %>%
  dplyr::count(percent_compl, year, permit_sa_gom_dual,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 634
# 500

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::add_count(year,
                   permit_sa_gom_dual,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  dplyr::filter(!!sa_dual_filter) %>%
  dplyr::select(year,
                permit_sa_gom_dual,
                percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  unique() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# 2023
#  $ cnt_v_in_bucket     : int [1:4] 673 227 146 500
#  $ total_per_y_r       : int [1:4] 1546 1546 1546 1546
# > 673+227+146+500
# [1] 1546

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  # cnt vessels per year, permit and compliance
  dplyr::add_count(year,
    permit_sa_gom_dual,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  dplyr::filter(!!sa_dual_filter) %>%
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
# 2023
# 1 0<= & <25%                         43.5 
# 2 25<= & <50%                        14.7 
# 3 50<= & <75%                         9.44
# 4 75<= & <=100%                      32.3 

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
    permit_sa_gom_dual = c("2022 sa_only",
                    "2022 gom_only",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$permit_sa_gom_dual %>%
  unique() %>%
  sort() %>%
  # repeat for each permit_sa_gom_dual
  # TODO: and year!
  purrr::map(function(curr_permit_sa_gom_dual) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)

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
    curr_title_y_p <- make_permit_sa_gom_dual_label(curr_permit_sa_gom_dual)

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)

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

# View(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc)
# perc1 <- 
# gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[1]]
# TODO: ?
  
perc2 <- 
gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]]

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

plot_perc_23 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)

## SA23 ----

super_title <- "% of non-compliant vessels (2023)"

gridExtra::grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]],
                        top = super_title
                        )
                        # ,
                        # bottom = footnote

