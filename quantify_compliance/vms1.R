# Above compliance metrics, to assess pre and post VMS requirement or vs increase in VMS ----
# compliance (just Gulf + dual permitted vessels; assess Feb 2022 (=pre-t), March 2022 (VMS implementation), and Sept 2022 (when 80% vessels had a registered VMS))

compl_clean_sa_vs_gom_m_int_filtered |>
  select(year_month) |>
  distinct()
# dim(compl_clean_sa_vs_gom_m_int_filtered)

compl_clean_sa_vs_gom_m_int_filtered_vms <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(year_permit == "2022 gom_dual" &
           year_month %in% c("Feb 2022",
                             "Mar 2022",
                             "Sep 2022"))
dim(compl_clean_sa_vs_gom_m_int_filtered_vms)
# [1] 12677    25

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered_vms, "year_month")
# glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt %>%
  select(year_month, total_vsl_y) %>%
  unique()
# 1 Sep 2022          1144
# 2 Mar 2022          1031
# 3 Feb 2022          1034

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

# glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp)

group_by_var <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp,
                      group_by_var)

# dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    year_month,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short)
# [1] 3319    7

## get compl_counts ----
### get compl, no compl, or both per period ----
group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short,
               group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide)
# [1]    6 1264

### count compl, no compl, or both per period, permit, active status ----
cols_names <-
  c("year_permit", "year_month", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide,
                cols_names)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long)
# [1] 7554    7

### get cnts for compl, no compl, or both per month with exp ----
group_by_cols <- c("year_month", "perm_exp_y")
cols_to_cnt <- c("year_month", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt)
# [1] 23  7

### check if a vessel is compliant and not at the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  glimpse()
# $ year_month             <yearmon> Sep 2022, Mar 2022, Feb 2022
# $ total_vsl_y            <int> 1144, 1031, 1034
# $ perm_exp_y             <chr> "expired", "expired", "expired"
# $ exp_y_tot_cnt          <int> 62, 108, 112
# $ vessel_official_number <chr> "657209", "657209", "657209"
# $ is_compl_or_both       <chr> "YES", "NO_YES", "NO"
# $ shared                 <lgl> TRUE, TRUE, TRUE

  # dim()
# 3
# TODO: fix

### check if a vessel permit is expired and not in the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dim()
# 0 ok

### check total_vsl_y vs. sum_cnts ----
compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1249 YES
# 2        117 NO
# 1249 + 117 = 1366
# TODO: what to compare with?

compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  mutate(perm_exp_y =
           case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  group_by(perm_exp_y) %>%
  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1140 YES        active
# 2        119 YES        expired
# 3       1140 NO         active
# 4        119 NO         expired
# 1140 + 119
# 1259
# TODO: what does it mean?

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration
group_by_compl_cols <- c("year_month", "compl_or_not")
group_by_exp_cols <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt,
    group_by_compl_cols,
    group_by_exp_cols
  )

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot)
# [1] 17 10

## add percents of total ----
select_cols <- c(
  "year_month",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc)
# [1] 12  7

# plots VMS:
gg_all_c_vs_nc_plots_vms <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc$year_month %>%
  unique() %>%
  sort() %>%
  # repeat for each year_month
  purrr::map(function(curr_year_month) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc %>%
      dplyr::filter(year_month == curr_year_month)

    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_month)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # current_title <-
    #   paste0(
    #     "GOM + Dual",
    #     " ",
    #     curr_year_month,
    #     " (Total Permitted: ",
    #     total_vsls,
    #     "; Expired Permits: ",
    #     expired_permits$cnt_y_p_e,
    #     ")"
    #   )

    current_title <-
      paste0(
        "GOM + Dual",
        " ",
        curr_year_month
      )

    # current_title <-
    #   paste0(
    #     "GOM + Dual",
    #     " ",
    #     curr_year_month,
    #     " (Total Permitted: ",
    #     total_vsls,
    #     ")"
    #   )

    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc",
                                       default_percen_labels = FALSE)

    return(one_plot)
  })

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

# combine plots for 2022
gg_arranged_plots_vms <-
  grid.arrange(
    gg_all_c_vs_nc_plots_vms[[1]],
    gg_all_c_vs_nc_plots_vms[[2]],
    gg_all_c_vs_nc_plots_vms[[3]],
    top = main_title
  )
# class(gg_all_c_vs_nc_plots_vms)

vms_plot_file_path <-
  file.path(plot_file_path, "vms")
create_dir_if_not(vms_plot_file_path)

## save VMS green and red plots ----
save_plots_list_to_files(file.path(vms_plot_file_path,
                                   "vms_3_months.png"),
                         gg_arranged_plots_vms)

# gg_all_c_vs_nc_plots_vms |>
#   map(function(current_plot) {
#     # create a clean_name
#     # browser()
#     clean_name <-
#       stringr::str_replace_all(current_plot$labels$title,
#                                "[^a_zA-z0-9]+", "_")
#     save_plots_list_to_files(file.path(vms_plot_file_path,
#                                        paste0(clean_name, ".png")),
#                              # plots
#                              current_plot)
#   })


# Non compliant only ----
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_year_month_vms_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt |>
  # compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::add_count(year_month, vessel_official_number, compliant_, name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year_month, vessel_official_number, name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

dim(weeks_per_vsl_year_month_vms_compl_cnt)
# [1] 12677    31

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
weeks_per_vsl_year_month_vms_compl_cnt_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc)
# [1] 12677    32

# check
weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  filter(vessel_official_number == "FL3327TJ") %>%
  select(
    year_month,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  glimpse()
# $ year_month                 <yearmon> Sep 2022, Sep 2022
# $ compliant_                 <chr> "NO", "YES"
# $ weeks_per_vessel_per_compl <int> 1, 3
# $ total_weeks_per_vessel     <int> 4, 4
# $ percent_compl              <dbl> 25, 75

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

weeks_per_vsl_year_month_vms_compl_cnt_perc_short <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_month,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short)
# [1] 159   7

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts <-
  get_p_buckets(weeks_per_vsl_year_month_vms_compl_cnt_perc_short,
                "percent_compl")

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts)
# [1] 159   8

### test 2 ----
# count in one bucket
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_month == "Mar 2022") %>%
  dplyr::count(percent_compl, year_month,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # glimpse()
  # $ percent_compl        <dbl> 100, 75
  # $ year_month           <yearmon> Mar 2022, Mar 2022
  # $ amount_of_occurences <int> 18, 2

  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 55 all
# 20 March

# 3) count how many in each bucket ----

  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b <-
    weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
    dplyr::add_count(year_month,
                     percent_n_compl_rank,
                     name = "cnt_v_in_bucket")

### test 3 ----
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
    # dplyr::filter(year_permit == "2022 sa_only") %>%
    dplyr::select(year_month,
                  percent_n_compl_rank,
                  cnt_v_in_bucket) %>%
    unique() %>%
    dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_period") %>%
    dplyr::arrange(percent_n_compl_rank) %>%
    glimpse()
# $ year_month           <yearmon> Sep 2022, Mar 2022, Feb 2022, Sep 2022, Mar 202…
# $ percent_n_compl_rank <chr> "25<= & <50%", "25<= & <50%", "25<= & <50%", "50<= …
# $ cnt_v_in_bucket      <int> 26, 32, 26, 6, 8, 6, 15, 20, 20
# $ total_per_period     <int> 159, 159, 159, 159, 159, 159, 159, 159, 159

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
  # cnt vessels per period and compliance
  dplyr::add_count(year_month,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()

# 1 25<= & <50%                         55.3
# 2 25<= & <50%                         53.3
# 3 25<= & <50%                         50
# 4 50<= & <75%                         12.8
# 5 50<= & <75%                         13.3
# 6 50<= & <75%                         11.5

# 5) blue plots by year ----

blue_year_plot_titles <-
  data.frame(
    first_part = c(
      "GOM + Dual Permitted Vessels\n("
    )
  )

gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc$year_month %>%
  unique() %>%
  sort() %>%
  # repeat for each year_month
  purrr::map(function(curr_year_month) {
    # browser()
    curr_df <-
      weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_month == curr_year_month)

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
    # curr_title_y_p <- make_year_month_label(curr_year_month)

    # curr_blue_year_plot_title <-
    # blue_year_plot_titles %>%
    # filter(year_month == curr_year_month)

    # y_p_title <-
    #   paste0(
    #     curr_year_month,
    #     " (Total Non-Compliant = ",
    #     total_non_compl_df$vsls_per_y_r,
    #     " Vessels; Acitve permits = ",
    #     active_permits$exp_y_tot_cnt,
    #     "; Expired permits: ",
    #     expired_permits$exp_y_tot_cnt,
    #     " Vessels)"
    #   )

    # y_p_title <-
    #   paste0(
    #     curr_year_month,
    #     " (Total Non-Compliant = ",
    #     total_non_compl_df$vsls_per_y_r,
    #     " Vessels)"
    #   )

    y_p_title <-
      paste0(
        curr_year_month,
        " (Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 10))

    return(one_plot)
  })

# main_blue_title <- "% non compliant vessels per period"
ndash <- "\u2013"
main_blue_title <- paste0(
  "% Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

grid.arrange(gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[1]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[2]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[3]],
             top = main_blue_title)

