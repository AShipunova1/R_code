#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_line_plots.R  ----

## Month, line plots with dots ----
# line_df_22_gom <- count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 gom_dual`

# line_df_22_sa <- count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 sa_only`

pecent_names <- paste0(seq(0, 100, by = 10), "%")

# dim(line_df_22_gom)
# [1] 24 10

# dim(line_df_22_sa)
# [1] 24 10

geom_text_size = text_sizes[["geom_text_size"]]
geom_text_size <- 5
axis_title_size <- text_sizes[["axis_text_x_size"]]
axis_title_size <- 12

line_df_22_gom_good_plot_l <-
  names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2) |> 
  purrr::map(function(line_df_permit) {
    line_df <- count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2[[line_df_permit]]
    
    one_plot <- 
      line_df |>
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
      # on top
      geom_text(aes(label = cnt_v_in_bucket2),
                vjust = -0.3,
                size = geom_text_size) +
      # under the dot
      geom_text(
        aes(label = cnt_vsl_m_compl),
        vjust = 1.3,
        color = plot_colors$compliant,
        size = geom_text_size
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      theme(
        legend.position = "none",
        plot.caption =
          element_text(size = text_sizes[["plot_caption_text_size"]]),
        axis.text.x =
          element_text(size = axis_title_size),
        axis.text.y =
          element_text(size = axis_title_size)
      ) +
      labs(size = "Groups of percentage",
           x = "Months (2022)",
           y = "Number of Vessels") +
      labs(title = stringr::str_glue("The Number of Non-Compliant Vessels ({line_df_permit}) Each Month\nThat Were Compliant More Than 50% of a Month in 2022")) +
      # theme(plot.title = element_text(lineheight = 0.9)) +
      labs(caption = "(The blue number is a total number of non-compliant vessels per month.)")
    
    return(one_plot)
  })

line_df_22_gom_good_plot <- line_df_22_gom_good_plot_l[[1]]
line_df_22_sa_good_plot <- line_df_22_gom_good_plot_l[[2]]

# non compliant by month ----
line_df_22_monthly_nc_plot_l <-
  names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2) |>
  # count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 gom_dual` |>
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
        labs(x = "Months (2022)",
             y = "Number of Vessels") +
        labs(
          title = str_glue(
            "The Number of Non-Compliant {curr_title_permit} Permitted Vessels Each Month in 2022"
          )
        )
    })

gom_line_df_22_monthly_nc_plot_l <- line_df_22_monthly_nc_plot_l[[1]]
sa_line_df_22_monthly_nc_plot_l <- line_df_22_monthly_nc_plot_l[[2]]

sa_line_df_22_monthly_nc_plot_l

# GOM non compliant by month percent of total ----

count_weeks_per_vsl_permit_year_compl_m_p_2022_gom <-
  count_weeks_per_vsl_permit_year_compl_m_p |>
  filter(year_permit == "2022 gom_dual")

count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_2022_gom |>
  select(year_month,
         total_vsl_m,
         cnt_vsl_m_compl,
         compliant_) |>
  dplyr::distinct()

# View(count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short)
# [1] 24  4

count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short_percent <-
  count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short |>
  dplyr::group_by(year_month) |>
  mutate(percent_of_total = 100 * cnt_vsl_m_compl / total_vsl_m)

glimpse(count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short_percent)

line_df_22_gom_monthly_nc_percent_plot_color = plot_colors$non_compliant_by_month

line_df_22_gom_monthly_nc_percent_plot <-
  count_weeks_per_vsl_permit_year_compl_m_p_2022_gom_c_cnts_short_percent |>
  filter(compliant_ == "NO") |> 
  ggplot(aes(
    x = as.Date(year_month),
    y = percent_of_total,
    color = line_df_22_gom_monthly_nc_percent_plot_color
  )) +
  geom_point(color = line_df_22_gom_monthly_nc_percent_plot_color,
             size = 4) +
  geom_line(color = line_df_22_gom_monthly_nc_percent_plot_color,
            linewidth = 1) +
  theme_bw() +
  # text under the dot
  geom_text(
    aes(label = paste0(round(percent_of_total, 1), "%")),
    # vjust = -0.4,
    hjust = -0.3,
    color = line_df_22_gom_monthly_nc_percent_plot_color,
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
  labs(x = "Months (2022)",
       y = "Proportion of Non-Compliant Vessels") +
  # labs(title = "The Percent of Non-Compliant GOM + Dual Permitted Vessels Each Month in 2022") +
  expand_limits(x = as.Date("12/31/22", "%m/%d/%y"))
# 
#   coord_cartesian(xlim = c(as.Date(year_month), NA))

# dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
# as.Date(dates, "%m/%d/%y")


line_df_22_gom_monthly_nc_percent_plot

# save to files ----

plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

plot_file_path_lines <-
  file.path(plot_file_path, "line_plots")
create_dir_if_not(plot_file_path_lines)

file_full_name <- file.path(plot_file_path_lines,
                            "line_df_22_sa_good_plot.png")

# see the function definition F2
save_plots_list_to_files(file_full_name,
                         # plots
                         line_df_22_sa_good_plot)

# test_df |> 
#   filter(year_month == "Jan 2022") |> 
#   View()

count_weeks_per_vsl_permit_year_compl_m_p_nc |> 
  filter(year_month == "Jun 2022" &
             year_permit == "2022 gom_dual" &
             percent_compl_m < 50) |> 
  summarise(n_distinct(vessel_official_number))
# print_df_names(test_df)

max_min_text <- "{cnt_v_in_bucket2} v / {cnt_vsl_m_compl} tot nc v"

min_max_val <-
  test_df |>
  dplyr::group_by(percent_non_compl_2_buckets) |>
  mutate(
    max_dot_y = max(perc_vsls_per_m_b2),
    min_dot_y = min(perc_vsls_per_m_b2)
  ) |>
  dplyr::ungroup() |>
  mutate(
    max_dot_month =
      dplyr::case_when(
        perc_vsls_per_m_b2 == max_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      ),
    min_dot_month =
      dplyr::case_when(
        perc_vsls_per_m_b2 == min_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      )
  ) |>
  mutate(
    max_dot_text =
      dplyr::case_when(
        !is.na(max_dot_month) ~ str_glue(max_min_text)
      ),
    min_dot_text =
      dplyr::case_when(
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
    # color = "#0570B0",
    # angle = 90
  ) +
  annotate(
    "text",
    x = as.Date(min_max_val$min_dot_month),
    y = min_max_val$min_dot_y,
    # mean of reports_cnts, rounded to 2 decimals
    label = min_max_val$min_dot_text
    # ,
    # color = "#0570B0",
    # angle = 90
  )

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
#   write_csv("month_with_numbers_gom_22.csv")

# count_weeks_per_vsl_permit_year_compl_m_p_nc |>
#   filter(year_month == "May 2022" &
#            year_permit == "2022 gom_dual" &
#            percent_compl_m < 50) |>
#   View()


