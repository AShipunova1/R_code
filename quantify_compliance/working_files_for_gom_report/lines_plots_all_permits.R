## Month, line plots with dots ----
year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2)

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2)

pecent_names <- paste0(seq(0, 100, by = 10), "%")

line_plots_3 <- 
  year_permits |> 
  map(function(current_year_permit) {
    # browser()
    current_df <-
      count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2[[current_year_permit]]

    current_year_label <-
      make_year_permit_label(current_year_permit)

    split_current_year_label <-
      str_split(current_year_label, " ", 2)
    current_year <- split_current_year_label[[1]][1]
    current_permit_region <- split_current_year_label[[1]][2]

    x_title <- "Months ({current_year})"

    main_title <-
      "The Number of Non-Compliant Vessels ({current_permit_region}) Each Month\nThat Were Compliant More Than 50% of a Month in {current_year}"

    one_lines_plot <-
      current_df |>
      filter(percent_non_compl_2_buckets == "< 50%") |>
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
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      theme(legend.position = "none") +
      labs(size = "Groups of percentage",
           x = str_glue(x_title),
           y = "Number of Vessels") +
      labs(title = str_glue(main_title)) +
      labs(caption = "(The blue number is a total number of non-compliant vessels per month.)")

    return(one_lines_plot)
  }
)

# line_plots_3

plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

plot_file_path_lines <-
  file.path(plot_file_path, "line_plots")
create_dir_if_not(plot_file_path_lines)

names(line_plots_3) <- year_permits

map(year_permits,
    function(current_year_permit){
      file_full_name <-
        file.path(plot_file_path_lines,
                  paste0(current_year_permit,
                         "_mostly_right.png"))

      # see the function definition F2
      save_plots_list_to_files(file_full_name,
                               line_plots_3[[current_year_permit]])
    })

# test 
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
