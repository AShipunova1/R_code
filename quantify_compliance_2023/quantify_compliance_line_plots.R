#### Current file:  ~/R_code_github/quantify_compliance2023/quantify_compliance_from_fhier_line_plots.R  ----

percent_names <- paste0(seq(0, 100, by = 10), "%")

geom_text_size = text_sizes[["geom_text_size"]]
geom_text_size <- 5
axis_title_size <- text_sizes[["axis_text_x_size"]]
axis_title_size <- 12

# Per month, region ----
## add total vessels per month and region ----
# (both compl. and not, a vsl can be in both)

add_cnt_in_gr <-
  function(my_df, 
           group_by_col, 
           cnt_col_name = "total_vsl_m_by_year_perm") {
    my_df %>%
      # group by per month and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate({{cnt_col_name}} :=
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

group_by_col <- c("year_permit", "year_month")
compl_clean_sa_vs_gom_m_int_tot <-
  add_cnt_in_gr(compl_clean_sa_vs_gom_m_int, group_by_col)

# check
res1 <-
  compl_clean_sa_vs_gom_m_int |>
  select(vessel_official_number, year_permit, year_month) |>
  distinct() |>
  count(year_permit, year_month, name = "total_vsl_m_by_year_perm") |>
  arrange(total_vsl_m_by_year_perm)

tail(res1)
# 1 2023 sa_dual Dec 2023                       1969
# 2 2023 sa_dual Sep 2023                       1986
# 3 2023 sa_dual May 2023                       2020
# 4 2023 sa_dual Aug 2023                       2023
# 5 2023 sa_dual Jun 2023                       2026
# 6 2023 sa_dual Jul 2023                       2036

res2 <-
  compl_clean_sa_vs_gom_m_int_tot |>
  select(year_permit, year_month, total_vsl_m_by_year_perm) |>
  distinct() |>
  arrange(total_vsl_m_by_year_perm)

all.equal(res1, res2)
# T

compl_clean_sa_vs_gom_m_int_tot |>
  filter(year_permit == "2023 sa_dual") |> 
  select(year_month, total_vsl_m_by_year_perm) |>
  distinct() |> 
  arrange(year_month) |> 
  head()
# 1 Jan 2023          1967
# 2 Feb 2023          1958
# 3 Mar 2023          1954
# 4 Apr 2023          1968
# 5 May 2023          2020
# 6 Jun 2023          2026

## count vessels per month, region and compl ----
group_by_col <- c("year_permit", "year_month", "compliant_")

compl_clean_sa_vs_gom_m_int_tot__compl_cnt <-
  add_cnt_in_gr(compl_clean_sa_vs_gom_m_int_tot, 
                group_by_col,
                "cnt_vsl_m_compl")

print_df_names(compl_clean_sa_vs_gom_m_int_tot__compl_cnt)
### test tot cnts per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_tot__compl_cnt %>%
  dplyr::select(
    permit_sa_gom,
    year_permit,
    year_month,
    total_vsl_m_by_year_perm,
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
# $ total_vsl_m_by_year_perm     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760 is more than total. Some vessels can be both in a month, if compliance differs by week. For this analysis I used vessels having at least one week in the month  non-compliant.
# If we are going to use "yes only" than redo "yes, no, no_yes" division as for a year above.
# $ cnt_vsl_m_compl <int> 1052, 688, 1004, 42

# 2023:
# $ year_month      <yearmon> Jan 2023, Jan 2023, Jan 2023, Jan 2023, Jan 2023, …
# $ perm_exp_m      <chr> "active", "active", "active", "active", "active", "act…
# $ exp_m_tot_cnt   <int> 1967, 1967, 1967, 675, 1967, 675
# $ total_vsl_m_by_year_perm     <int> 1967, 1967, 1967, 675, 1967, 675
# $ compliant_      <chr> "YES", "NO", "YES", "YES", "NO", "NO"
# $ cnt_vsl_m_compl <int> 1693, 322, 1693, 675, 322, 1

## Month: percent compl vessels per per month ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_month)

compl_clean_sa_vs_gom_m_int_tot__compl_cnt



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

# print_df_names(compl_clean_sa_vs_gom_m_int_tot_exp_y_cnt_short)

# compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide <-
#   get_compl_by(compl_clean_sa_vs_gom_m_int_tot_exp_y_cnt_short,
#                group_by_for_compl)

# View(compl_clean_sa_vs_gom_m_int_tot_exp_y_short_wide)
# [1]    6 3377

# using all fields
compl_clean_sa_vs_gom_m_int_c_cnt_tot_wide <-
  compl_clean_sa_vs_gom_m_int_tot_exp_y_cnt |>
  dplyr::select(
    vessel_official_number,
    year_permit,
    compliant_,
    total_vsl_m_by_year_perm
  ) |>
  dplyr::distinct() |>
  get_compl_by(group_by_for_compl)


# non compliant by month ----
line_df_23_monthly_nc_plot_l <-
  names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2) |>
  # count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2023 sa_dual` |>
  # filter(percent_non_compl_2_buckets == "< 50%") |>
  purrr::map(
    function(line_df_permit) {
      line_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2[[line_df_permit]]
      
      curr_title_permit <-
        title_permits %>%
        filter(year_permit == line_df_permit)
      
      one_plot <-
        line_df |>
        ggplot(aes(
          x = as.Date(year_month),
          y = cnt_vsl_m_compl,
          color = plot_colors$non_compliant_by_month
        )) +
        geom_point(color = plot_colors$non_compliant_by_month,
                   size = 5) +
        geom_line(color = plot_colors$non_compliant_by_month,
                  linewidth = 1) +
        theme_bw() +
        # text under the dot
        geom_text(
          aes(label = cnt_vsl_m_compl),
          # vjust = -0.4,
          # hjust = -0.5,
          # hjust = -0.2,
          vjust = -1,
          color = plot_colors$non_compliant_by_month,
          size = geom_text_size
        ) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        theme(
          legend.position = "none",
          axis.text.x =
            element_text(size = axis_title_size),
          axis.text.y =
            element_text(size = axis_title_size)
        ) +
        labs(x = "Months (2023)",
             y = "Number of Vessels") +
        labs(
          title = str_glue(
            "The Number of Non-Compliant {curr_title_permit} Permitted Vessels Each Month in 2023"
          )
        )
    })

gom_line_df_23_monthly_nc_plot_l <- line_df_23_monthly_nc_plot_l[[1]]
sa_line_df_23_monthly_nc_plot_l <- line_df_23_monthly_nc_plot_l[[2]]

sa_line_df_23_monthly_nc_plot_l

# non compliant by month percent of total ----

count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual <-
  count_weeks_per_vsl_permit_year_compl_m_p |>
  filter(year_permit == "2023 sa_dual")

count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual |>
  select(year_month,
         total_vsl_m_by_year_perm,
         cnt_vsl_m_compl,
         compliant_) |>
  dplyr::distinct()

# View(count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short)
# [1] 24  4

count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short_percent <-
  count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short |>
  dplyr::group_by(year_month) |>
  mutate(percent_of_total = 100 * cnt_vsl_m_compl / total_vsl_m_by_year_perm)

glimpse(count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short_percent)

line_df_23_gom_monthly_nc_percent_plot_color = plot_colors$non_compliant_by_month

line_df_23_gom_monthly_nc_percent_plot <-
  count_weeks_per_vsl_permit_year_compl_m_p_2023_sa_dual_c_cnts_short_percent |>
  filter(compliant_ == "NO") |> 
  ggplot(aes(
    x = as.Date(year_month),
    y = percent_of_total,
    color = line_df_23_gom_monthly_nc_percent_plot_color
  )) +
  geom_point(color = line_df_23_gom_monthly_nc_percent_plot_color,
             size = 4) +
  geom_line(color = line_df_23_gom_monthly_nc_percent_plot_color,
            linewidth = 1) +
  theme_bw() +
  # text under the dot
  geom_text(
    aes(label = paste0(round(percent_of_total, 1), "%")),
    # vjust = -0.4,
    hjust = -0.3,
    color = line_df_23_gom_monthly_nc_percent_plot_color,
    size = 6
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    legend.position = "none",
    axis.text.x =
      element_text(size = axis_title_size),
    axis.text.y =
      element_text(size = axis_title_size)
  ) +
  ylim(0, 50) +
  labs(x = "Months (2023)",
       y = "Proportion of Non-Compliant Vessels") +
  # labs(title = "The Percent of Non-Compliant GOM + Dual Permitted Vessels Each Month in 2023") +
  expand_limits(x = as.Date("12/31/23", "%m/%d/%y"))
# 
#   coord_cartesian(xlim = c(as.Date(year_month), NA))

# dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
# as.Date(dates, "%m/%d/%y")


line_df_23_gom_monthly_nc_percent_plot

# save to files ----

plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

plot_file_path_lines <-
  file.path(plot_file_path, "line_plots")
create_dir_if_not(plot_file_path_lines)

file_full_name <- file.path(plot_file_path_lines,
                            "line_df_23_sa_good_plot.png")

# see the function definition F2
save_plots_list_to_files(file_full_name,
                         # plots
                         line_df_23_sa_good_plot)

# test_df |> 
#   filter(year_month == "Jan 2023") |> 
#   View()

count_weeks_per_vsl_permit_year_compl_m_p_nc |>
  filter(year_month == "Jun 2023" &
             year_permit == "2023 sa_dual" &
             percent_compl_m < 50) |> 
  summarise(n_distinct(vessel_official_number))
# print_df_names(test_df)
# 37

max_min_text <- "{cnt_v_in_bucket2} v / {cnt_vsl_m_compl} tot nc v"

# test_df <-
#   count_weeks_per_vsl_permit_year_compl_m_p_nc |>
#   filter(year_permit == "2023 sa_dual")
# 
# min_max_val <-
#   test_df |>
#   dplyr::group_by(percent_non_compl_2_buckets) |>
#   mutate(
#     max_dot_y = max(perc_vsls_per_m_b2),
#     min_dot_y = min(perc_vsls_per_m_b2)
#   ) |>
#   dplyr::ungroup() |>
#   mutate(
#     max_dot_month =
#       dplyr::case_when(
#         perc_vsls_per_m_b2 == max_dot_y &
#           percent_non_compl_2_buckets == "< 50%" ~ year_month
#       ),
#     min_dot_month =
#       dplyr::case_when(
#         perc_vsls_per_m_b2 == min_dot_y &
#           percent_non_compl_2_buckets == "< 50%" ~ year_month
#       )
#   ) |>
#   mutate(
#     max_dot_text =
#       dplyr::case_when(
#         !is.na(max_dot_month) ~ str_glue(max_min_text)
#       ),
#     min_dot_text =
#       dplyr::case_when(
#         !is.na(min_dot_month) ~ str_glue(max_min_text)
#       )
#   )

# test_plot +
#   annotate(
#     "text",
#     x = as.Date(min_max_val$max_dot_month),
#     y = min_max_val$max_dot_y,
#     # mean of reports_cnts, rounded to 2 decimals
#     label = min_max_val$max_dot_text
#     # ,
#     # color = "#0570B0",
#     # angle = 90
#   ) +
#   annotate(
#     "text",
#     x = as.Date(min_max_val$min_dot_month),
#     y = min_max_val$min_dot_y,
#     # mean of reports_cnts, rounded to 2 decimals
#     label = min_max_val$min_dot_text
#     # ,
#     # color = "#0570B0",
#     # angle = 90
#   )

# test_df |> select(
#   year_permit,
#   year_month,
#   compliant_,
#   # perm_exp_m,
#   exp_m_tot_cnt,
#   cnt_vsl_m_compl,
#   cnt_v_in_bucket2,
#   perc_vsls_per_m_b2,
#   percent_non_compl_2_buckets
# ) |>
#   dplyr::arrange(year_month) |>
#   View()
# |>
#   write_csv("month_with_numbers_gom_23.csv")

# count_weeks_per_vsl_permit_year_compl_m_p_nc |>
#   filter(year_month == "May 2023" &
#            year_permit == "2023 sa_dual" &
#            percent_compl_m < 50) |>
#   View()

