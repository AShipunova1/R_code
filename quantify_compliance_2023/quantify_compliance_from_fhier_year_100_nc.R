# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2023 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 

# Calculate the percentage of never compliant entries from all entries in each year
# all SA vessels (compl and not compl alike) ----

## fewer fields ----
count_weeks_per_vsl_permit_year_compl_p_short <- 
  count_weeks_per_vsl_permit_year_compl_p |>
  select(-c(week_start, 
            week_num,
            year_month,
            permit_sa_gom_dual)) |>
  distinct()
  
# Explanations:
# This code filters rows based on whether the column 'permit_sa_gom_dual_both' contains the string "sa".

# 1. Use the filter function from dplyr along with str_detect from stringr to filter rows.
# 2. Apply str_detect to 'permit_sa_gom_dual_both' column, checking if it contains the string "sa".
# 3. The resulting data frame contains only rows where 'permit_sa_gom_dual_both' contains "sa".

count_weeks_per_vsl_permit_year_compl_p_sa <-
  count_weeks_per_vsl_permit_year_compl_p_short |>
  filter(str_detect(permit_sa_gom_dual_both, "sa"))

# check
count_weeks_per_vsl_permit_year_compl_p_sa %>%
  group_by(vessel_official_number) %>%
  filter(n() >
           1) %>%
  summarize(n = n())
  
 # 5 1020822                    4
#  8 1022458                    3
# 10 1023960                    2
   # arrange(desc(n))
     # 4

count_weeks_per_vsl_permit_year_compl_p_sa |> 
  filter(vessel_official_number == "1020822") |> 
  glimpse()
# $ year_permit_sa_gom_dual    <chr> "2022 sa_only", "2022 sa_only", "2023 sa_dual",…
# $ percent_compl              <dbl> 63.461538, 36.538462, 90.384615, 9.615385

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 298147     10
dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6092    9

count_weeks_per_vsl_permit_year_compl_p_sa |> 
  select(year, permit_sa_gom_dual_both, year_permit_sa_gom_dual) |> 
  distinct()
#   year  permit_sa_gom_dual_both year_permit_sa_gom_dual
#   <chr> <chr>                   <chr>                  
# 1 2022  sa_only                 2022 sa_only           
# 2 2023  sa_dual                 2023 sa_dual           

# without sa_dual
# 1 2022  sa_only                 2022 sa_only           
# 2 2023  sa_only                 2023 sa_only           

total_sa_dual_vessels <-
  count_weeks_per_vsl_permit_year_compl_p_sa |> 
  group_by(year) |>
  mutate(cnts = n_distinct(vessel_official_number)) |>
  ungroup() |> 
  select(year, cnts) |> 
  distinct()
# 1 2022   2231
# 2 2023   2177

# 100% non compliant out of total ----

## add columns ----

# Explanations:
# This code creates a logical filter using the rlang::quo function to filter rows based on specific conditions.

# 1. Define a logical condition using the rlang::quo function.
# 2. The condition checks if 'perc_nc_100_gr' is equal to 2 and the lowercase of 'compliant_' is "no".
# 3. The resulting filter is stored in the variable 'never_reported_filter'.

never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

dim(count_weeks_per_vsl_permit_year_compl_p_sa)
# [1] 6092    9

count_weeks_per_vsl_permit_year_compl_p_sa |>
  filter(percent_compl == "100",
         compliant_ == "NO",
         year == my_year2) |>
  select(vessel_official_number) |>
  # distinct() |>
  nrow()
# 360

count_weeks_per_vsl_permit_year_compl_p_sa |>
  filter(percent_compl == "100",
         year == my_year2) |>
  count(compliant_)
#   compliant_     n
#   <chr>      <int>
# 1 NO           360
# 2 YES          814

# non_compliant_only ----
# Explanations:
# This code calculates various metrics related to compliance percentages and vessel counts.

# 1. Count the number of weeks per vessel, permit, year, and compliance status.
# 2. Group the data by 'year_permit_sa_gom_dual'.
# 3. Calculate the 'perc_nc_100_gr' by finding the interval of 'percent_compl' within [1, 100].
# 4. Create a new column 'perc_nc_100_gr_name' based on the logical condition specified in 'never_reported_filter'.
# 5. Create a new column 'group_100_vs_rest' based on the logical condition specified in 'never_reported_filter'.
# 6. Ungroup the data.
# 7. Group the data by 'perc_nc_100_gr_name' and 'year_permit_sa_gom_dual'.
# 8. Calculate the number of distinct vessels ('group_vsl_cnt') in each group.
# 9. Calculate the 'perc_of_perc' by multiplying 'group_vsl_cnt' by 100 and dividing by 'total_vsl_y_by_year_perm'.
# 10. Ungroup the data.

count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_sa |>
  group_by(year_permit_sa_gom_dual) |>
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
  group_by(perc_nc_100_gr_name, year_permit_sa_gom_dual) |>
  mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  dplyr::mutate(perc_of_perc =
                  group_vsl_cnt * 100 / total_vsl_y_by_year_perm) |>
  dplyr::ungroup()

# check
count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
  filter(vessel_official_number == "1020822") |>
  distinct() |> 
  select(-contains("month")) |> 
  distinct() |> 
  glimpse()
# compl, sa_only
# $ year                       <chr> "2022", "2022", "2023", "2023"
# $ perc_of_perc               <dbl> 79.33662, 79.33662, 83.46348, 83.46348

# Plots ----

# Explanations:
# This function prepares information for creating a plot that visualizes compliance percentages
# for vessels that never reported. It extracts relevant details from the input data and constructs
# a title for the plot.

# 1. Split the curr_year_permit string into a list using a space as a delimiter.
# 2. Extract the year and permit_sa_gom_dual from the split list.
# 3. Filter the title_permits data frame to get details related to the current permit.
# 4. Extract the unique total_vsl_y_by_year_perm values from the input data.
# 5. Construct the plot title using string interpolation.

# One plot:
# 1. Use ggplot to create a plot with aesthetics specified for x-axis, y-axis, and fill.
# 2. Add a bar geometry (geom_col) to represent the data.
# 3. Use scale_fill_manual to set custom colors for compliance values.
# 4. Set labels for fill colors based on unique values of perc_nc_100_gr_name.
# 5. Adjust theme settings, such as removing the legend and customizing axis text.
# 6. Add plot titles and labels, while removing x and y-axis titles.
# 7. Set y-axis limits to ensure values range from 0 to 100.

make_one_plot_100c <-
  function(curr_df,
           curr_year_permit) {
    # browser()
    
    curr_year_permit_list <-
      str_split_1(curr_year_permit, " ")
    curr_year <- curr_year_permit_list[[1]]
    curr_permit_sa_gom_dual <- curr_year_permit_list[[2]]
    
    # cat(curr_year_permit)
    
    curr_title_permit <-
      title_permits %>%
      filter(permit_sa_gom_dual == curr_permit_sa_gom_dual)
    
    curr_total_vsl_y_by_year_perm <-
      curr_df$total_vsl_y_by_year_perm |>
      unique()
    
    curr_title <-
      stringr::str_glue(
        "Total {curr_title_permit$long_title} Permitted Vessels That Never Reported")
    
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
      labs(title = curr_title,
           y = "",
           # y = "% of All Vessels",
           x = "") +
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

# Explanations:
# This code generates a list of plots (sa_dual_tot_100_plots) using the make_one_plot_100c function.
# 1. Extract unique combinations of year and permit from the data.
# 2. Use purrr::map to iterate over each unique combination of year and permit.
# 3. Within the map function, filter the data to select rows for vessels that never reported and are non-compliant.
# 4. Select relevant columns from the filtered data, ensuring uniqueness.
# 5. Call the make_one_plot_100c function with the filtered data and the current year_permit combination.
# 6. Collect the resulting plots in a list (sa_dual_tot_100_plots).
sa_dual_tot_100_plots <-
  count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc$year_permit_sa_gom_dual |>
  unique() |>
  map(\(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_compl_p_sa__tot_perc |>
      filter(year_permit_sa_gom_dual == curr_year_permit) |>
      filter(compliant_ == "NO") |> 
      select(
        # vessel_official_number,
        year,
        total_vsl_y_by_year_perm,
        group_100_vs_rest,
        perc_nc_100_gr_name,
        group_vsl_cnt,
        year_permit_sa_gom_dual,
        perc_of_perc
      ) |>
      distinct()
    
    make_one_plot_100c(curr_df,
                       curr_year_permit)
  })

sa_dual_tot_100_plots

sa_dual_tot_100_plots_flat_list <-
  make_flat_plot_list(sa_dual_tot_100_plots)

# Explanations:
# Extract unique names for each plot in the sa_dual_tot_100_plots_flat_list.
# 1. For each element (plot) in the list, extract the year_permit_sa_gom_dual value.
# 2. Replace spaces with underscores, convert to lowercase, and remove unnecessary characters.
# 3. Ensure uniqueness of the resulting names for each plot.

flat_plot_list_names <-
  sa_dual_tot_100_plots_flat_list |>
  map(\(x) {
    x$data$year_permit_sa_gom_dual |>
      str_replace_all(" ", "_") |>
      tolower() |>
      str_replace("_permitted_vessels", "") |>
      str_replace_all("[^a-z0-9_]", "_") |> 
      unique()
  })
  
names(sa_dual_tot_100_plots_flat_list) <-
    flat_plot_list_names

my_grobs_list <- sa_dual_tot_100_plots_flat_list

# combine plots ----
grid.arrange(grobs = my_grobs_list)

# Explanations:
# Iterate through each plot name in the flat_plot_list_names.
# 1. For each plot, create a file name by combining the plot_file_path with the plot_name.
#    - This file name is used to save the plot image.
# 2. Use the save_plots_list_to_files function to save the corresponding plot in my_grobs_list.
#    - Set specific parameters for the output file, such as width, height, etc.

flat_plot_list_names |>
  map(\(plot_name) {
    # browser()
    
    file_full_name_c_nc <-
      file.path(plot_file_path,
                str_glue("100nc_plot_{plot_name}.png"))
    
    save_plots_list_to_files(file_full_name_c_nc,
                             my_grobs_list[[plot_name]],
                             my_width = 20,
                             my_height = 10)
  })

# [1] "~/R_files_local/my_outputs/quantify_compliance_2023/2024-02-09/100nc_plot_2023_sa_dual.png"

