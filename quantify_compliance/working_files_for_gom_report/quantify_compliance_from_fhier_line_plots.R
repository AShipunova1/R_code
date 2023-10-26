## Month, line plots with dots ----
test_df <- count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 gom_dual`

# test_df_percent_n_compl_rank <- sort(unique(test_df$percent_n_compl_rank))
# q_colors = length(test_df_percent_n_compl_rank)
# library(viridis)
# mypalette = viridis(q_colors, option = "D")
# # mypalette <- rainbow(length(gom_all_cnt_indexes))
# names(mypalette) <- test_df_percent_n_compl_rank
# mypalette
# 
# month_unique <- 
#   test_df$year_month |> 
#   unique() 
# 
# month_labels <-
#   factor(month_unique,
#          labels = format(month_unique, "%b"))
# 
# month_labels_all <-
#   factor(test_df$year_month,
#          labels = format(month_unique, "%b"))
# 
pecent_names <- paste0(seq(0, 100, by = 10), "%")

glimpse(test_df)
# [1] 24 10

test_plot <-
  test_df |>
  filter(percent_non_compl_2_buckets == "< 50%") |>
  # |> View()
  ggplot(aes(
    x = as.Date(year_month),
    y = cnt_v_in_bucket2,
    color = percent_non_compl_2_buckets
  )) +
  geom_point() +
  geom_line() +
  theme_bw() +
  # text on dots
  geom_text(aes(label = cnt_v_in_bucket2), vjust = -0.3) +
  geom_text(aes(label = cnt_vsl_m_compl),
            vjust = 1.3,
            color = "blue") +
  # scale_y_continuous(breaks = seq(0, 100, by = 10),
  # labels = pecent_names) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(legend.position = "none") +
  labs(size = "Groups of percentage",
       x = "Months (2022)",
       y = "Number of Vessels"
       # y = "Number of Non-Compliant Vessels That Were Compliant More Than 50%",
       # y = "Percent of non-compliant vessels been non-compliant less than half a month",
       # title = "Distribution of number of weeks when a vessel was non compliant (2022 GOM + dual)")
       ) +
  labs(title = "The Number of Non-Compliant Vessels Each Month\nThat Were Compliant More Than 50% of a Month in 2022") +
  # theme(plot.title = element_text(lineheight = 0.9)) +
  labs(caption = "(The blue number is a total number of non-compliant vessels per month.)")
# guides(color = guide_legend(title = "nc weeks")) +
# ylim(0, 100)

test_plot

plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

plot_file_path_lines <-
  file.path(plot_file_path, "line_plots")
create_dir_if_not(plot_file_path_lines)

file_full_name <- file.path(plot_file_path_lines,
                            "gom_2022_mostly_right.png")

# see the function definition F2
save_plots_list_to_files(file_full_name,
                         # plots
                         test_plot)

test_df |> 
  filter(year_month == "Jan 2022") |> 
  View()

count_weeks_per_vsl_permit_year_compl_m_p_nc |> 
  filter(year_month == "Jun 2022" &
             year_permit == "2022 gom_dual" &
             percent_compl_m < 50) |> 
  summarise(n_distinct(vessel_official_number))
# print_df_names(test_df)

max_min_text <- "{cnt_v_in_bucket2} v / {cnt_vsl_m_compl} tot nc v"

min_max_val <-
  test_df |>
  group_by(percent_non_compl_2_buckets) |>
  mutate(
    max_dot_y = max(perc_vsls_per_m_b2),
    min_dot_y = min(perc_vsls_per_m_b2)
  ) |>
  ungroup() |>
  mutate(
    max_dot_month =
      case_when(
        perc_vsls_per_m_b2 == max_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      ),
    min_dot_month =
      case_when(
        perc_vsls_per_m_b2 == min_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      )
  ) |>
  mutate(
    max_dot_text =
      case_when(
        !is.na(max_dot_month) ~ str_glue(max_min_text)
      ),
    min_dot_text =
      case_when(
        !is.na(min_dot_month) ~ str_glue(max_min_text)
      )
  )

test_plot +
  annotate(
    "text",
    x = as.Date(min_max_val$max_dot_month),
    y = min_max_val$max_dot_y,
    # mean of reports_cnts, rounded to 2 decimals
    label = min_max_val$max_dot_text
    # ,
    # color = "red",
    # angle = 90
  ) +
  annotate(
    "text",
    x = as.Date(min_max_val$min_dot_month),
    y = min_max_val$min_dot_y,
    # mean of reports_cnts, rounded to 2 decimals
    label = min_max_val$min_dot_text
    # ,
    # color = "red",
    # angle = 90
  )

test_df |> select(
  year_permit,
  year_month,
  compliant_,
  # perm_exp_m,
  exp_m_tot_cnt,
  cnt_vsl_m_compl,
  cnt_v_in_bucket2,
  perc_vsls_per_m_b2,
  percent_non_compl_2_buckets
) |>
  arrange(year_month) |>
  View()
# |>
#   write_csv("month_with_numbers_gom_22.csv")

count_weeks_per_vsl_permit_year_compl_m_p_nc |>
  filter(year_month == "May 2022" &
           year_permit == "2022 gom_dual" &
           percent_compl_m < 50) |>
  View()
