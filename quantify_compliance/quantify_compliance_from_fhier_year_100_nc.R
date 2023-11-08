# run from quantify_compliance_start.R after quantify_compliance_from_fhier_year.R

# SA vessels that never reported anything ----
# Jessica wants to see 1 more figure for the SA, that is the proportion of SA vessels that never reported anything - whereas, your compliance for all of 2022 means of the 54% non-compliant, they may only be missing 1 week in the whole year. 
# print_df_names(count_weeks_per_vsl_permit_year_compl_p)

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_sa_22' by filtering an existing data frame.
# Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p' to the next operation.
# The filter function is used to select rows where the column year_permit is equal to "2022 sa_only". The result is a filtered data frame for the specified year and permit condition.
count_weeks_per_vsl_permit_year_compl_p_sa_22 <-
  count_weeks_per_vsl_permit_year_compl_p |>
  dplyr::filter(year_permit == "2022 sa_only")

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# by applying a series of operations to the existing data frame
count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100 <-
  
  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Select specific columns from the data frame
  dplyr::select(vessel_official_number,
                compliant_,
                year_permit,
                percent_compl) |>
  
  # Remove duplicate rows
  dplyr::distinct() |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Further filter the data frame to select rows where 'percent_compl' is equal to 100
  dplyr::filter(percent_compl == 100)

dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)
# 487

# All vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p$vessel_official_number))
# 3669

# All SA 2022 vessels
length(unique(count_weeks_per_vsl_permit_year_compl_p_sa_22$vessel_official_number))
# 2152
# in metrics:
# Total Vessels With SA Only
# 2275
# 2275 - 2152 = 123?

# Create a new data frame 'sa_22_non_c_vessels' by applying a series of operations to the existing data frame
sa_22_non_c_vessels <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Select a specific column 'vessel_official_number'
  dplyr::select(vessel_official_number) |>
  
  # Remove duplicate rows
  dplyr::distinct()

# Create a new data frame 'sa_22_vessels' by applying a series of operations to the existing data frame
sa_22_vessels <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_sa_22' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_sa_22 |>
  
  # Select a specific column 'vessel_official_number'
  dplyr::select(vessel_official_number) |>
  
  # Remove duplicate rows
  dplyr::distinct()

# Calculate the percentage of never compliant entries from all non-compliant entries

# Calculate the total number of non-compliant entries that are never compliant
# by multiplying the number of rows in 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# with 100 and dividing it by the number of rows in 'sa_22_non_c_vessels'
percent_of_never_compl_from_all_non_c <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_non_c_vessels)[[1]]
# [1] 41.87446 %

# Calculate the percentage of never compliant entries from all entries in 2022
# In this code, the variable percent_of_never_compl_from_all_sa_2022 is calculated by dividing the number of rows in the data frame count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100 (representing non-compliant entries that are never compliant) by the number of rows in the data frame sa_22_vessels (representing all entries in the year 2022) and then multiplying the result by 100 to obtain the percentage of never compliant entries from all entries in 2022.

# Calculate the total number of non-compliant entries that are never compliant
# by multiplying the number of rows in 'count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100'
# with 100 and dividing it by the number of rows in 'sa_22_vessels'
percent_of_never_compl_from_all_sa_2022 <- 
  dim(count_weeks_per_vsl_permit_year_compl_p_sa_22_non_100)[[1]] * 100 / dim(sa_22_vessels)[[1]]
# [1] 22.63011 %

# SA vessels 2022 vessels cnt / percent compl ----

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_short' 
# by applying a series of operations to the existing data frame

# The data frame count_weeks_per_vsl_permit_year_compl_p is passed to the next operation using the pipe operator.
# Specific columns are selected using the select function from dplyr.
# Duplicate rows are removed using the distinct function.
# The result is a data frame with a subset of columns from the original data frame, and duplicate rows are eliminated.

count_weeks_per_vsl_permit_year_compl_p_short <- 
  count_weeks_per_vsl_permit_year_compl_p |>
  dplyr::select(
    vessel_official_number,
    compliant_,
    year_permit,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) |>
  dplyr::distinct()

# data_overview(count_weeks_per_vsl_permit_year_compl_p_short)
# vessel_official_number     3669

# Create a new data frame 'count_weeks_per_vsl_permit_year_compl_p_short_count' 
# by applying a series of operations to the existing data frame 'count_weeks_per_vsl_permit_year_compl_p_short'
count_weeks_per_vsl_permit_year_compl_p_short_count <- 

  # Use the pipe operator to pass 'count_weeks_per_vsl_permit_year_compl_p_short' to the next operation
  count_weeks_per_vsl_permit_year_compl_p_short |>
  
  # Filter the data frame to select rows where 'compliant_' is "NO"
  dplyr::filter(compliant_ == "NO") |>
  
  # Further filter the data frame to select rows where 'year_permit' is "2022 sa_only"
  dplyr::filter(year_permit == "2022 sa_only") |>
  
  # Select specific columns 'vessel_official_number' and 'percent_compl'
  dplyr::select(vessel_official_number, percent_compl) |>
  
  # Add a count column 'vessels_cnt' based on the 'percent_compl' values
  dplyr::add_count(percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count, 2)

## add columns ----
# This code performs a series of data manipulation operations on the existing data frame count_weeks_per_vsl_permit_year_compl_p_short_count to create a new data frame, count_weeks_per_vsl_permit_year_compl_p_short_count_perc. Each step is explained as follows:

# The data frame count_weeks_per_vsl_permit_year_compl_p_short_count is passed to the next operation using the pipe operator.
# A new column 'total_vessels' is added to calculate the count of distinct 'vessel_official_number'.
# Columns 'perc_nc_100_gr' and 'perc_nc_100_gr_name' are added based on 'percent_compl' values and a defined interval.
# The data frame is grouped by 'perc_nc_100_gr'.
# A new column 'group_vsl_cnt' is added to calculate the count of distinct 'vessel_official_number' within each group.
# The column 'vessel_official_number' is removed.
# Duplicate rows are removed.
# The 'perc_of_perc' column is calculated based on 'perc_nc_100_gr' values.
# The data frame is ungrouped.

count_weeks_per_vsl_permit_year_compl_p_short_count_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  dplyr::mutate(total_vessels = n_distinct(vessel_official_number)) |>
  # Add columns 'perc_nc_100_gr' and 'perc_nc_100_gr_name' based on 'percent_compl' values
  dplyr::mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100)),
    perc_nc_100_gr_name =
      dplyr::case_when(perc_nc_100_gr == 2 ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |>
  dplyr::group_by(perc_nc_100_gr) |>
  dplyr::mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  # Calculate the 'perc_of_perc' based on 'perc_nc_100_gr' values
  dplyr::mutate(
    perc_of_perc =
      dplyr::case_when(
        perc_nc_100_gr == 2 ~
          vessels_cnt * 100 / total_vessels,
        perc_nc_100_gr == 1 ~
          sum(vessels_cnt) * 100 / total_vessels
      )
  ) |>
  dplyr::ungroup()

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)

# Create a ggplot object named 'nc_sa_22_100_plot' using a series of dplyr and ggplot functions.

# Use the pipe operator to pass the output of the previous function to the next function.

nc_sa_22_100_plot <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>

  # Select specific columns from the data frame.
  dplyr::select(
    perc_nc_100_gr,
    perc_nc_100_gr_name,
    group_vsl_cnt,
    perc_of_perc
  ) |>

  # Remove duplicate rows based on selected columns.
  dplyr::distinct() |>

  # Create a ggplot object with specified aesthetics.
  ggplot(aes(
    x = perc_nc_100_gr_name,   # X-axis variable
    y = round(perc_of_perc, 0),  # Y-axis variable with rounding
    fill = as.factor(perc_nc_100_gr)  # Fill aesthetic for grouping
  )) +

  # Add a column plot to the ggplot object.
  geom_col() +

  # Manually set the fill colors for different factors.
  scale_fill_manual(
    values = c(
      # "1" = "pink",
      # "2" = "red"
      "1" = "skyblue1",  # Custom color for factor 1
      "2" = "#0570B0"    # Custom color for factor 2
    ),
    name = "Non compliant",  # Legend title
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +

  # Remove the legend.
  theme(legend.position = "none") +

  # Customize text size for y-axis title and labels.
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x = element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y = element_text(size = text_sizes[["axis_text_y_size"]])
  ) +

  # Set the plot titles and y-axis label.
  labs(
    title = stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
    y = "Non compliant in 2022 (%)",
    x = ""  # No label for x-axis
  ) +

  # Set the y-axis limits to be between 0 and 100.
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_perc)
# Add percent numbers on the bars
# In this code, the 'nc_sa_22_100_plot' ggplot object is further modified by adding text labels to the bars. The geom_text function is used to display labels on the plot, and it is configured to show the rounded percentage values on top of the bars. The position_stack function is used to position the labels in the middle of the bars, and the text size is set to a predefined value specified in text_sizes[["geom_text_size"]].

nc_sa_22_100_plot <- nc_sa_22_100_plot +

  # Add text labels to the plot.
  geom_text(
    aes(
      label = paste0(round(perc_of_perc, 0), "%")
    ),
    # Position the text labels in the middle of the bars.
    position = position_stack(vjust = 0.5),
    # Set the text size for the labels.
    size = text_sizes[["geom_text_size"]]
  )

# show the plot
nc_sa_22_100_plot

# Save the ggplot object 'nc_sa_22_100_plot' as a PNG image file using 'ggsave'.

ggsave(
  file = "sa_22_nc_100.png",  # File name
  plot = nc_sa_22_100_plot,   # The ggplot object to be saved
  device = "png",            # File format (PNG)
  path = file.path(my_paths$outputs, r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),  # Output directory
  width = 20,                # Image width in centimeters
  height = 10,               # Image height in centimeters
  units = "cm"               # Unit of measurement for width and height
)

## 100% non compliant, less than 100 and compliant ----

# Calculate the number of distinct vessel official numbers in the data.
# Create a new object named 'total_vessels_c_n_nc' by first filtering the data.
total_vessels_c_n_nc <-
  count_weeks_per_vsl_permit_year_compl_p_short |>
  
  # Select the 'vessel_official_number' column.
  dplyr::select(vessel_official_number) |>

  # Remove duplicate rows based on the selected column and get the dimensions.
  dplyr::distinct() |>
  dim()
# vessel_official_number     3669

nc_sa_22_100_plot <-
  # Calculate the count of weeks per VSL (vessel) permit year for non-compliant permits
  count_weeks_per_vsl_permit_year_compl_p_short_count_perc |>
  # Select specific columns from the data frame
  dplyr::select(perc_nc_100_gr,
               perc_nc_100_gr_name,
               group_vsl_cnt,
               perc_of_perc) |>

  # Remove duplicate rows from the data frame
  dplyr::distinct() |>
  # Create a ggplot object with specified aesthetics
  # Define the x-axis using 'perc_nc_100_gr_name'
  ggplot(aes(
    x = perc_nc_100_gr_name,
    # Define the y-axis by rounding 'perc_of_perc' to the nearest integer
    y = round(perc_of_perc, 0),
    # Define fill colors based on 'perc_nc_100_gr'
    fill = as.factor(perc_nc_100_gr)
  )) +
  # Add a bar chart (column chart) to the plot
  geom_col() +
  # Manually set fill colors for the chart
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "1" = "skyblue1", # Set color for '1'
        "2" = "#0570B0"   # Set color for '2'
      ),
    # Legend title
    # Set the legend title to "Non compliant"
    name = "Non compliant",
    # Set legend labels
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_perc$perc_nc_100_gr_name)
  ) +
  # Remove the legend from the plot
  theme(legend.position = "none") +
  # Customize the appearance of the plot
  theme(
    axis.title.y = element_text(size = text_sizes[["axis_text_y_size"]]),
    axis.text.x =
      element_text(size = text_sizes[["axis_text_x_size"]]),
    axis.text.y =
      element_text(size = text_sizes[["axis_text_y_size"]])
  ) +
  # Set plot titles and labels
  # Set plot title using a formatted string
  # Set y-axis label
  # Set x-axis label to an empty string
  labs(title = 
         stringr::str_glue("Non compliant SA vsls in 2022 (total non compliant = {count_weeks_per_vsl_permit_year_compl_p_short_count_perc$total_vessels})"),
       y = "Non compliant in 2022 (%)",
       x = "") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100)

# plot(count_weeks_per_vsl_permit_year_compl_p_short_count)
## Less than 100% ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 <- 
  # Filter the data frame based on a condition
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  
  # Keep only rows where 'vessels_cnt' is less than 100
  dplyr::filter(vessels_cnt < 100)

perc_non_compl_plot_less_100 <-
  # Create a ggplot plot using the specified data frame and aesthetics
  ggplot(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
         # Define x-axis using 'vessels_cnt'
         aes(x = vessels_cnt,
             # Define y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add a line to the plot with the specified color
  geom_line(color = "deepskyblue") +
  # Set plot titles and labels
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100", 
       # Set x-axis label
       x = "Vessel count", 
       # Set y-axis label
       y = "% nc vsls") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100) +
  # Customize the size of the individual plot's title
  theme(plot.title =
          element_text(size = 12))

perc_non_compl_plot_less_100 <- 
  # Add additional layers to the existing plot
  perc_non_compl_plot_less_100 +
  # Add points to the plot
  # Display data points on the plot
  geom_point() +
  # text on dots
  # on top
  # Add text labels to the data points
  # Display the 'percent_compl' value rounded to one decimal place as text labels
  # Adjust the vertical justification of the labels
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = -0.3)

# Calculate the maximum value in the 'percent_compl' column of the data frame
max_percent_compl_less_100 <-
  max(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 98.07692

# Calculate the minimum value in the 'percent_compl' column of the data frame
min_percent_compl_less_100 <-
  min(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100$percent_compl)
# [1] 1.923077


# Add additional layers to the existing plot
perc_non_compl_plot_less_100_hline <-
  # Add horizontal lines to the plot
  perc_non_compl_plot_less_100 +
  # Add a horizontal line at the 'min_percent_compl_less_100' value
  geom_hline(yintercept = min_percent_compl_less_100,
             # Set the color of the line to red
             color = "red") +
  # Add a horizontal line at the 'max_percent_compl_less_100' value
  geom_hline(yintercept = max_percent_compl_less_100,
             # Set the color of the line to red
             color = "red")

# perc_non_compl_plot_less_100 + geom_smooth(method = "lm", se = FALSE)

perc_non_compl_plot_less_100_ann <- 
  # Add additional layers to the existing plot
  perc_non_compl_plot_less_100_hline +  
  
  # Add text annotations to the plot
  annotate(
    "text",
    label = paste0(round(min_percent_compl_less_100, 1), "%"),  # Display the minimum percentage value as a text label
    x = 53,  # Set the x-coordinate for the label
    y = min_percent_compl_less_100 + 3,  # Set the y-coordinate for the label slightly above the minimum value
    size = 4,  # Set the size of the text
    colour = "red"  # Set the text color to red
  ) +
  
  # Add another text annotation to the plot
  annotate(
    "text",
    label = paste0(round(max_percent_compl_less_100, 1), "%"),  # Display the maximum percentage value as a text label
    x = 53,  # Set the x-coordinate for the label
    y = 100,  # Set the y-coordinate for the label at the top of the plot
    size = 4,  # Set the size of the text
    colour = "red"  # Set the text color to red
  )


perc_non_compl_plot_less_100_ann

# split by group ----
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100 |>
  
  # Calculate the total vessel count
  dplyr::mutate(vessels_cnt_tot = sum(vessels_cnt)) |>
  
  # Create groups for vessel counts
  dplyr::mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6))) |>
  
  # Count the number of entries in each vessel count group
  dplyr::add_count(vessel_cnt_group, wt = vessels_cnt, name = "vessel_cnt_group_num")  |>
  
  # Create descriptive names for the vessel count groups
  dplyr::mutate(vessel_cnt_group_name =
           dplyr::case_when(
             vessel_cnt_group == 1 ~
               paste0("<= 5 vessels (",
                      vessel_cnt_group_num,
                      " v)"),
             .default = paste0("> 5 vessels (",
                               vessel_cnt_group_num,
                               " v)")
           )) |>
  
  # Create groups for compliance percentages
  dplyr::mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 75))) |>
  
  # Count the number of entries in each compliance percentage group
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  
  # Create descriptive names for the compliance percentage groups
  dplyr::mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--75% non compliant ({percent_group_num} v.)"),
        percent_group == 3 ~ str_glue("75--98% non compliant ({percent_group_num} v.)")
      )
  )

# Count the number of entries in each vessel count group and sum the 'vessels_cnt' column
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

# Count the number of entries in each compliance percentage group and sum the 'vessels_cnt' column
count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  # Create a ggplot plot with specified aesthetics
  # Define the x-axis using 'vessels_cnt'
  ggplot(aes(x = vessels_cnt, 
             # Define the y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add colored lines to the plot based on the 'vessel_cnt_group' factor
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  # Add data points to the plot with a dark blue color
  geom_point(color = "darkblue") +
  # Add text labels to the data points with a blue color
  # Display 'percent_compl' rounded to one decimal place as text labels
  # Adjust the vertical position of the labels
  # Set the text color to blue
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +
  # Set plot titles and labels
  # Set plot title
  # Set x-axis label
  # Set y-axis label
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # y axes 0 to 100
  ylim(0, 100)
  
ggplot(
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100,
  # Define the x-axis using 'vessels_cnt'
  aes(x = vessels_cnt)
) +
  # Create a histogram plot with specified bin width of 2
  geom_histogram(binwidth = 2)

# split by group all ----
count_weeks_per_vsl_permit_year_compl_p_short_count_gr <-
  count_weeks_per_vsl_permit_year_compl_p_short_count |>
  
  # Calculate the total count of distinct vessel official numbers
  dplyr::mutate(vessels_cnt_tot = n_distinct(vessel_official_number)) |>
  
  # Select all columns except 'vessel_official_number'
  dplyr::select(-vessel_official_number) |>
  
  # Remove duplicate rows from the data frame
  dplyr::distinct() |>
  
  # Create groups for vessel counts based on specified intervals
  dplyr::mutate(vessel_cnt_group = base::findInterval(vessels_cnt, c(0, 6, 450))) |>
  
  # Count the number of entries in each vessel count group and sum the 'vessels_cnt' column
  dplyr::add_count(vessel_cnt_group, wt = vessels_cnt, name = "vessel_cnt_group_num") |>
  
  # Create descriptive names for the vessel count groups
  dplyr::mutate(
    vessel_cnt_group_name =
      dplyr::case_when(
        vessel_cnt_group == 1 ~
          str_glue("{vessel_cnt_group}: 1--5 vessels ({vessel_cnt_group_num} v)"),
        vessel_cnt_group == 2 ~
          str_glue(
            "{vessel_cnt_group}: 6--450 vessels ({vessel_cnt_group_num} v)"
          ),
        vessel_cnt_group == 3 ~
          str_glue(
            "{vessel_cnt_group}: 451--500 vessels ({vessel_cnt_group_num} v)"
          )
      )
  ) |>
  
  # Create groups for compliance percentages based on specified intervals
  dplyr::mutate(percent_group = base::findInterval(percent_compl, c(0, 50, 99))) |>
  
  # Count the number of entries in each compliance percentage group and sum the 'vessels_cnt' column
  dplyr::add_count(percent_group, wt = vessels_cnt, name = "percent_group_num") |>
  
  # Create descriptive names for the compliance percentage groups
  dplyr::mutate(
    percent_group_name =
      dplyr::case_when(
        percent_group == 1 ~ str_glue("1--50% non compliant  ({percent_group_num} v.)"),
        percent_group == 2 ~ str_glue("50--98% non compliant ({percent_group_num} v.)"),
        percent_group == 3 ~ str_glue("99--100% non compliant ({percent_group_num} v.)")
      )
  )

# This code takes the count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr data frame and uses the dplyr::count() function to count the number of entries in each vessel count group (vessel_cnt_group_name). The wt parameter is set to vessels_cnt to sum the 'vessels_cnt' column within each group. The result is a summary of counts for each unique vessel_cnt_group_name along with the sum of 'vessels_cnt' in each group.

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(vessel_cnt_group_name, wt = vessels_cnt)
# 1 <= 5 vessels (240 v)    388
# 2 > 5 vessels (21 v)      288
# 388 + 288 = 676

# w/o weight
# 1 <= 5 vessels (240 v)    240
# 2 > 5 vessels (21 v)       21

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  dplyr::count(percent_group_name, wt = vessels_cnt)
# 1 0--50% non compliant    539
# 2 50--75% non compliant    80
# 3 75--98% non compliant    57
# 539 + 80 + 57 = 676

# w/o weight
# 1 0--50% non compliant    176
# 2 50--75% non compliant    44
# 3 75--98% non compliant    41
# 176 + 44 + 41 = 261

# View(count_weeks_per_vsl_permit_year_compl_p_short_count)

count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |> 
  # Define the x-axis using 'vessels_cnt'
  ggplot(aes(x = vessels_cnt,
             # Define the y-axis using 'percent_compl'
             y = percent_compl)) +
  # Add colored lines based on 'vessel_cnt_group' factor
  geom_line(aes(colour = factor(vessel_cnt_group))) +
  # Add data points in dark blue
  geom_point(color = "darkblue") +

  # Display 'percent_compl' rounded to one decimal place as text labels
  # Adjust the vertical position of the labels
  # Set the text color to blue
  geom_text(aes(label = round(percent_compl, 1)),
            vjust = 1.3,
            color = "blue") +

  # Set plot title
  # Set x-axis label
  # Set y-axis label
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls") +
  # Limit the y-axis to a range of 0 to 100
  ylim(0, 100)  
  

# facets ----
# p <- ggplot(mtcars, aes(mpg, wt)) +
#   geom_point() +
#   facet_wrap(~ cyl)
# 
# mean_wt <- data.frame(cyl = c(4, 6, 8), wt = c(2.28, 3.11, 4.00))
# p + geom_hline(aes(yintercept = wt), mean_wt)
# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr)

# This code defines a variable named labs that contains a set of labels for a plot. The labels specify the plot title, x-axis label, and y-axis label. These labels are intended to be used when customizing the appearance of a plot in ggplot.

labs <- 
  labs(title = "Non compliant SA vessels (2022) number by percent of non compliant where % non compliant < 100",
       x = "Vessel count",
       y = "% nc vsls")

# Define a variable 'p' and create a ggplot plot using the data frame
# 'count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr'.
p <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  # Define the x-axis using 'vessels_cnt'
  # Define the y-axis using 'percent_compl'
  ggplot(aes(x = vessels_cnt,
             y = percent_compl)) +

  # Add data points in dark green
  geom_point(color = "darkgreen") +
  # facet_wrap(vars(vessel_cnt_group_name), scales = "free_x")
  # Create multiple subplots based on 'vessel_cnt_group_num' and 
  # 'percent_group_name' with labels for each subplot.
  facet_wrap(vars(vessel_cnt_group_num, percent_group_name), labeller = "label_both") +
  # Include custom labels previously defined in the 'labs' variable.
  labs

# Define a variable 'p' and create a ggplot plot using the data frame
# 'count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr'.
# Define the x-axis using 'vessels_cnt'
# Define the y-axis using 'percent_compl'
p <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_less_100_gr |>
  ggplot(
    aes(x = vessels_cnt,
        y = percent_compl)
  ) +
  # Add data points in dark green
  geom_point(color = "darkgreen") +
  facet_wrap(vars(percent_group_name), scales = "free_x") +
  # Create multiple subplots based on 'percent_group_name' with labels for each subplot.
  # Include custom labels previously defined in the 'labs' variable.
  labs  

# facet plots for all non compliant SA 2022 ----
# A variable labs_all is defined to store custom labels for a ggplot2 plot.
# The labs function is used to set the title, x-axis label, and y-axis label for the plot.
# The title is set to "Number of SA permitted vessels grouped by percent of non-compliant time in 2022".
# The x-axis label is set to "Vessel count".
# The y-axis label is set to "Percent of non-compliant in 2022".
# These custom labels will be used to annotate the ggplot2 plot and provide context and information to the viewer.

labs_all <- 
  labs(title = "Number of SA permitted vessels grouped by percent of non compliant time in 2022",
       x = "Vessel count",
       y = "Percent of non conmpliant in 2022")

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
# All percents ----
# This code is using the pipe operator (|>) to create a new data frame count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot based on the count_weeks_per_vsl_permit_year_compl_p_short_count_gr data frame. Here's what each line of code does:
# 
# count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot <-
# 
# This line initializes a new data frame called count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot.
# count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
# 
# The pipe operator (|>) takes the result of the expression on the left and passes it as the first argument to the function on the right.
# dplyr::group_by(vessel_cnt_group) |>
# 
# This line groups the data by the vessel_cnt_group column using the dplyr::group_by function.
# dplyr::mutate( max_in_vsl_group = max(vessels_cnt), min_in_vsl_group = min(vessels_cnt) ) |>
# 
# Within the grouped data, this line calculates two new columns:
# max_in_vsl_group: Computes the maximum value of the vessels_cnt column within each group.
# min_in_vsl_group: Computes the minimum value of the vessels_cnt column within each group, using the dplyr::mutate function.
# dplyr::ungroup()
# 
# This line ungroups the data, removing the grouping structure created earlier by the dplyr::group_by function.
# So, the code is essentially creating a new data frame that groups the original data by vessel_cnt_group, calculates the maximum and minimum values of vessels_cnt within each group, and then ungroups the data. The resulting data frame is stored in count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot.
# 

count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  dplyr::group_by(vessel_cnt_group) |>
  dplyr::mutate(
    max_in_vsl_group = max(vessels_cnt),
    min_in_vsl_group = min(vessels_cnt)
  ) |>
  dplyr::ungroup()

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot)

## All by vessel count ---

# Create a ggplot visualization using the pipe operator and the dataset.
count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  # Initialize the ggplot object with aesthetics mapping for x, y, and point size.
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  # Add points to the plot with a dark red color.
  geom_point(color = "darkred") +
  # Create facets in the plot based on the levels of the "vessel_cnt_group_name" variable
  # with independent x-axis scales.
  facet_wrap(vars(vessel_cnt_group_name), scales = "free_x") +
  # Add additional labeling and styling to the plot (the specifics of 'labs_all' are not provided).
  labs_all +
  # Customize the x-axis scale with specific tick marks based on data from another dataset.
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(
      max(
        count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
      )
    ))
  ))


## All By percent ----

# View(count_weeks_per_vsl_permit_year_compl_p_short_count_gr)
plot_all_by_percent <- 
  count_weeks_per_vsl_permit_year_compl_p_short_count_gr |>
  ggplot(aes(x = vessels_cnt,
             y = percent_compl,
             cex = vessel_cnt_group_num)) +
  geom_point(color = "darkred") +
  # ggplot2::facet_grid(
  #   cols = vars(percent_group_name),
  #   scales = "free_x",
  #   space = "free_x",
  #   margins = "vessels_cnt"
  # ) +
facet_wrap(vars(percent_group_name),
           scales = "free_x",
           nrow = 1) +
  scale_x_continuous(breaks = seq(
    min(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$min_in_vsl_group
    ),
    max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group
    ),
    by = floor(log10(max(
      count_weeks_per_vsl_permit_year_compl_p_short_count_gr_for_plot$max_in_vsl_group)))
  )) +
  labs_all +
  labs(cex = "Vsl num")

plot_all_by_percent

ggsave(
  file = "sa_22_nc_perc_vsl_cnt_by_percent.png",
  plot = plot_all_by_percent,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 40,
  height = 20,
  units = "cm"
)

# 100% non compliant out of total ----
count_weeks_per_vsl_permit_year_compl_p_short_count_tot <- 
  count_weeks_per_vsl_permit_year_compl_p_short |> 
  dplyr::filter(year_permit == "2022 sa_only") |> 
  dplyr::select(vessel_official_number, compliant_, percent_compl) |> 
  dplyr::add_count(compliant_, percent_compl, name = "vessels_cnt")

head(count_weeks_per_vsl_permit_year_compl_p_short_count_tot, 2)
#   vessel_official_number compliant_ percent_compl vessels_cnt
#   <chr>                  <chr>              <dbl>       <int>
# 1 VI5498TB               YES                  100         990
# 2 VA9236AV               NO                   100         487

## add columns ----
never_reported_filter <-
  rlang::quo(perc_nc_100_gr == 2 &
               tolower(compliant_) == "no")

count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot |>
  dplyr::mutate(total_vessels = n_distinct(vessel_official_number)) |> 
  # dplyr::mutate(percent_compl_compl = ) |> 
  dplyr::mutate(
    perc_nc_100_gr = base::findInterval(percent_compl, c(1, 100))) |> 
  # dplyr::group_by(perc_nc_100_gr, compliant_) |> str()
  dplyr::mutate(perc_nc_100_gr_name =
      dplyr::case_when(!!never_reported_filter ~
                  "Never Reported",
                .default = "Reported At Least 1 Time")
  ) |> 
  dplyr::mutate(group_100_vs_rest =
      dplyr::case_when(!!never_reported_filter ~
                  1,
                .default = 2)
  ) |> 
  dplyr::group_by(perc_nc_100_gr_name) |>
  dplyr::mutate(group_vsl_cnt = n_distinct(vessel_official_number)) |>
  dplyr::select(-vessel_official_number) |>
  dplyr::distinct() |>
  dplyr::mutate(
    perc_of_perc =
          group_vsl_cnt * 100 / total_vessels
  ) |>
  dplyr::ungroup()

glimpse(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
nc_sa_22_tot_100_plot <-
  count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc |>
  dplyr::select(group_100_vs_rest,
         perc_nc_100_gr_name,
         group_vsl_cnt,
         perc_of_perc) |>
  dplyr::distinct() |>
  ggplot(aes(x = perc_nc_100_gr_name,
             y = round(perc_of_perc, 0),
             fill = as.factor(group_100_vs_rest))) +
  geom_col() +
  scale_fill_manual(
    # use custom colors
    values =
      c(
        # "1" = "pink",
        # "2" = "red"
        "2" = "skyblue1",
        "1" = "#0570B0"
      ),
    # Legend title
    name = "Non compliant",
    labels = unique(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$perc_nc_100_gr_name)
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
  labs(title = 
         stringr::str_glue("Never reported SA vsls in 2022 out of all compliant and non compliant (total vsls = {count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc$total_vessels})"),
       y = "",
       # y = "% of All Vessels",
       x = "") +
  ylim(0, 100)

# print_df_names(count_weeks_per_vsl_permit_year_compl_p_short_count_tot_perc)
# Add percent numbers on the bars
nc_sa_22_tot_100_plot <-
  nc_sa_22_tot_100_plot +
  geom_text(aes(label =
                  paste0(round(perc_of_perc, 0), "%")),
            # in the middle of the bar
            position =
              position_stack(vjust = 0.5),
            size = text_sizes[["geom_text_size"]])

nc_sa_22_tot_100_plot

ggsave(
  file = "sa_22_tot_100nc_plot.png",
  plot = nc_sa_22_tot_100_plot,
  device = "png",
  path = file.path(my_paths$outputs,
                   r"(quantify_compliance\vsl_cnt_by_perc_non_compl)"),
  width = 20,
  height = 10,
  units = "cm"
)

