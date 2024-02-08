# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2023 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 

# Calculate the percentage of never compliant entries from all entries in each year

# all SA vessels (compl and not compl alike) ----
count_weeks_per_vsl_permit_year_compl_p_sa <-
  count_weeks_per_vsl_permit_year_compl_p |>
  select(-c(week_start, week_num)) |>
  distinct() |> 
  dplyr::filter(!!sa_dual_filter)

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 298147     10
dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6628    8

count_weeks_per_vsl_permit_year_compl_p_sa |> 
  select(year, permit_sa_gom_dual) |> 
  distinct()
# 1 2022  sa_only           
# 2 2023  sa_only           
# 3 2023  dual              

total_sa_dual_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_sa |> 
  group_by(year) |>
  mutate(cnts = n_distinct(vessel_official_number)) |>
  ungroup() |> 
  select(year, cnts) |> 
  distinct()
# 1 2022   2231
# 2 2023   2436

# 100% non compliant out of total ----

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6628    8

count_weeks_per_vsl_permit_year_compl_p_sa |>
  filter(percent_compl == "100",
         compliant_ == "NO",
         year == my_year2) |>
  select(vessel_official_number) |>
  distinct() |>
  nrow()
# 372 ok

count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_sa |>
  group_by(year, permit_sa_gom_dual) |>
  mutate(perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100))) |>
  mutate(
    perc_nc_100_gr_name =
      case_when(!!never_reported_filter ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |>
  mutate(group_100_vs_rest =
           case_when(!!never_reported_filter ~
                       1,
                     .default = 2)) |>
  ungroup() |>
  group_by(perc_nc_100_gr_name, year, permit_sa_gom_dual) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  # filter(permit_sa_gom_dual == "dual", compliant_ == "YES",
  #        year == "2023") |>
  # filter(vessel_official_number == "TX6550AU") |>
  # glimpse()
  #
  dplyr::mutate(perc_of_perc =
                  group_vsl_cnt * 100 / total_vsl_y_by_year_perm) |>
  dplyr::ungroup()

count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
  filter(vessel_official_number == "1020822") |>
  glimpse()
# $ year                       <chr> "2022", "2022", "2023", "2023"
# $ perc_of_perc               <dbl> 79.33662, 79.33662, 83.46348, 83.46348

# curr_year <- my_year2
# curr_permit_sa_gom_dual <- "sa_only"

make_one_plot_100c <- 
  function(curr_year, curr_permit_sa_gom_dual) {
  
  curr_df <- 
    count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
    filter(year == curr_year,
           permit_sa_gom_dual == curr_permit_sa_gom_dual) |>
    select(
      # vessel_official_number,
      year,
      total_vsl_y_by_year_perm,
      group_100_vs_rest,
      perc_nc_100_gr_name,
      group_vsl_cnt,
      permit_sa_gom_dual,
      # compliant_,
      perc_of_perc
    ) |>
    distinct()

  curr_title_permit <-
    title_permits %>%
    filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)
  
  curr_title <-
    stringr::str_glue(
      "Never reported {curr_title_permit$title} {curr_title_permit$second_part} in {curr_year}\n out of all compliant and non compliant (total vsls = {curr_df$total_vsl_y_by_year_perm})"
    )
  
  one_plot <-
    curr_df |>
    ggplot(aes(
      x = perc_nc_100_gr_name,
      y = round(perc_of_perc, 0),
      fill = as.factor(group_100_vs_rest)
    )) +
    geom_col() +
    scale_fill_manual(
      # use custom colors
      values =
        c("2" = plot_colors[["compliant"]],
          "1" = plot_colors[["non_compliant"]]),
      labels = unique(
        count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$perc_nc_100_gr_name
      )
    ) +
    theme(legend.position = "none") +
    theme(
      axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
      axis.text.x =
        element_text(size = text_sizes[["axis_text_x_size"]]),
      axis.text.y =
        element_text(size = text_sizes[["axis_text_y_size"]])
    ) +
    # no x and y titles for individual plots
    labs(
      title = curr_title,
      y = "",
      # y = "% of All Vessels",
      x = ""
    ) +
    ylim(0, 100)
  
  # Add percent numbers on the bars
  curr_tot_100_plot <-
    one_plot +
    geom_text(aes(label =
                    paste0(round(perc_of_perc, 0), "%")),
              # in the middle of the bar
              position =
                position_stack(vjust = 0.5),
              size = text_sizes[["geom_text_size"]])
  
  return(curr_tot_100_plot)
}

sa_dual_tot_100_plots <-
  count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$year |>
  unique() |> 
  map(\(curr_year) {
    count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$permit_sa_gom_dual |>
      unique() |>
      map(\(curr_permit_sa_gom_dual) {
        cat(c(curr_year, curr_permit_sa_gom_dual), "\n")
        make_one_plot_100c(curr_year, curr_permit_sa_gom_dual)
      })
  })

# sa_dual_tot_100_plots

# drop dual 2022
my_grobs_list <- list(sa_dual_tot_100_plots[[1]][[1]],
                      sa_dual_tot_100_plots[[2]][[1]],
                      sa_dual_tot_100_plots[[2]][[2]])

# combine plots ----
grid.arrange(grobs = my_grobs_list)

my_grobs_list |>
  map(\(plot_name) {
    # browser()
    curr_permit_sa_gom_dual <-
      unique(plot_name$data$permit_sa_gom_dual)
    curr_year <- unique(plot_name$data$year)
    
    file_name_part <-
      str_glue("{curr_permit_sa_gom_dual}_{curr_year}") |>
      tolower()
    
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("{file_name_part}_100nc_plot.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             plot_name,
                             my_width = 20,
                             my_height = 10)
  })

# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_outputs/quantify_compliance_2023/2024-02-07/dual_2023_100nc_plot.png"

