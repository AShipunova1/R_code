# quantify_compliance_functions

# Create a list called 'text_sizes' that contains named elements with different font sizes.
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 15,
  axis_text_y_size = 15,
  plot_caption_text_size = 12,
  ### common axes for Months ----
  y_left_fontsize = 10
)

# Define a function named 'get_non_compl_week_counts_percent' that accepts two arguments: 'my_df' (a data frame) and 'vessel_id_col_name' (a column name).
# 
# Start a pipeline to apply subsequent operations to the 'my_df' data frame.
# 
# Count the number of non-compliant weeks per vessel for each 'year_month' using 'count' from the dplyr package. The '!!sym()' function is used to interpret 'vessel_id_col_name' as a symbol.
# 
# Count the occurrence of unique combinations of 'year_month' and 'nc_weeks_per_vessl_m'.
# 
# Pivot the data to have a wide format, creating a column for each value of 'nc_weeks_per_vessl_m' in each 'year_month'.
# 
# Calculate the total number of non-compliant vessels per month by summing columns 2 to 6.
# 
# Reshape the data into a long format, with a row for each 'non_compl_weeks' and its count in a month.
# 
# Calculate the percentage of non-compliant vessels in a month, rounding the result to two decimal places.
# 
# Return the resulting data frame, which represents non-compliant week counts and percentages.
# 

get_non_compl_week_counts_percent <- function(my_df, vessel_id_col_name) {
  # browser()
    my_df %>%
    # Count the number of non-compliant weeks per vessel for each year_month.
    dplyr::count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # Count the occurrence of each unique combination of year_month and nc_weeks_per_vessl_m.
    dplyr::count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    tidyr::pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # Calculate the total number of non-compliant vessels per month.
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%

    # Reshape the data to have a row for each 'non_compl_weeks' and its count in a month.
    tidyr::pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # Calculate the percentage of non-compliant vessels in a month.
    mutate(percent_nc = round(
      100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month,
      digits = 2
    )) %>%
    return()
}

# Define a function called 'perc_plots_by_month' that takes two arguments:
# 1. 'my_df': A data frame.
# 2. 'current_year_month': The specific year_month for which to create a percentage plot.
perc_plots_by_month <-
  function(my_df, current_year_month) {
    # Uncomment the following line to enable debugging using the 'browser()' function.
    # browser()

    # Extract the total number of non-compliant vessels for the specified year_month.
    total_nc_vsl_per_month <-
      my_df %>%
      filter(year_month == current_year_month) %>%
      select(total_nc_vsl_per_month) %>%
      unique()

    # Create a title for the plot indicating the current year_month and total non-compliant vessels.
    month_title <-
      paste0(current_year_month,
             ": ",
             total_nc_vsl_per_month[[1]],
             " total nc vsls")
    
    my_df %>%
      # Filter the data frame to include only rows for the specified year_month.
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc)) +
      
      # Create a column plot with custom fill color.
      geom_col(fill = plot_colors$nc_bucket) +
      
      # Add text labels to the bars with the percentage values.
      geom_text(aes(label = paste0(percent_nc, "%")),
                position = position_dodge(width = 0.9)
                ) +
      
      # Customize the plot's title and axis title text sizes. 
      # Use the values from the 'text_sizes' list to specify the text sizes.
      theme(plot.title = text_sizes$plot_title_text_size,
            axis.title = text_sizes$axis_title_text_size
      ) +

      # Set the y-axis limits to be between 0 and 100.
      ylim(0, 100) +

      # Set plot labels, including the dynamic 'month_title'.
      labs(title = month_title,
           # x-axis label.
           x = "Num of nc weeks",  
           # y-axis label.
           y = "") %>%
      
      return()
  }

# Define a function called 'make_year_permit_label' that takes a single argument, 'curr_year_permit'.
# This function takes a string ('curr_year_permit') and performs a series of text transformations on it:
# It replaces the substring "_dual" with " + dual" using the 'stringr::str_replace' function.
# It replaces underscores ("_") with spaces using 'stringr::str_replace'.
# It converts the entire string to uppercase using 'toupper'.
# The resulting modified string is then returned by the function.

make_year_permit_label <- function(curr_year_permit) {
    curr_year_permit %>%
    
    # Replace "_dual" with " + dual".
    stringr::str_replace("_dual", " + dual") %>%
    
    # Replace underscores ("_") with spaces.
    stringr::str_replace("_", " ") %>%
    
    # Convert the entire string to uppercase.
    toupper() %>%
    
    # Return the resulting modified string.
    return()
}

# Define a function called 'make_one_plot_compl_vs_non_compl' that takes several arguments.
# This function generates a bar plot using ggplot2, where the bars represent compliance percentages. It has several optional parameters, allowing you to customize the appearance of the plot, including the title, color scheme, axis labels, and whether to display a legend. The function also offers the option to display percent labels on the bars.

# This function is designed to create a plot comparing compliant vs. non-compliant data. It takes various arguments for customization:
# 
# 'my_df': The data frame containing the data.
# 'current_title': The title for the plot.
# 'is_compliant': The column name for the 'is_compliant' data.
# 'percent': The column name for the percentage data.
# 'no_legend': A flag to control whether to display a legend.
# 'percent_label_pos': The position of the percentage labels.
# 'default_percent_labels': A flag to control the default display of percentage labels.
# 'geom_text_size': The font size for text labels on the plot.
# The function then creates a ggplot plot, customizes various plot elements, and adds percentage labels to the bars. It returns the resulting plot for further use.
# 

make_one_plot_compl_vs_non_compl <-
  function(my_df,
           current_title = "",
           is_compliant = "is_compliant",
           percent = "percent",
           no_legend = FALSE,
           percent_label_pos = 0.5,
           default_percent_labels = TRUE,
           geom_text_size = text_sizes[["geom_text_size"]],
           legend_labels = c("Yes", "No")
  ) {
    
    one_plot <-
      my_df %>%
      # ggplot(): Initializes a new ggplot object.
      # aes(): Specifies the aesthetics (aesthetics mapping) for the plot.
      # x = !!sym(is_compliant): Maps the x - axis to a variable specified by the value of is_compliant. The !!sym() syntax is used to unquote is_compliant, allowing it to be evaluated within the context of the ggplot.
      # fill = !!sym(is_compliant): Maps the fill (color) aesthetic to the same variable as the x-axis, based on the compliance status. 
      # Again, !!sym() is used to unquote is_compliant.

      ggplot(aes(
        x = !!sym(is_compliant),
        y = !!sym(percent),
        fill = !!sym(is_compliant)
      )) +
      # Add a column/bar plot
      geom_col() +
      theme(
        axis.text.x =
          element_text(size = text_sizes[["axis_text_x_size"]]),
        axis.text.y =
          element_text(size = text_sizes[["axis_text_y_size"]])
      ) +
      # # Add percent numbers on the bars
      #     one_plot <-
      # one_plot + annotate("text", x = 4, y = 25, label = "Some text")
      #
      # geom_text(aes(label =
      #                 paste0(round(!!sym(percent), 1), "%")),
      #           # in the middle of the bar
      #           position =
      #             position_stack(vjust = percent_label_pos)
      #           ) +
      # no x and y titles for individual plots
    
    # Set the plot title and remove x and y axis labels
    labs(title = current_title,
         x = "",
         y = "") +
      
      # Define manual color fill scale
      scale_fill_manual(
        # use custom colors
        values =
          c("compliant" = plot_colors[["compliant"]],
            "non_compliant" = plot_colors[["non_compliant"]]),
        # Legend title
        name = "Is compliant?",
        # Legend labels
        labels = legend_labels
      ) +
      
      # Define manual x-axis tick labels
      scale_x_discrete(labels = legend_labels) +
      # scale_y_continuous(limits = c(0, 100), labels = scales::percent)
      # Set the y-axis limits between 0 and 100
      ylim(0, 100)
    # +
    # scale_y_continuous(labels = scales::label_percent(scale = 1))
    
    # Create a 'label_percent' vector by applying the rounding and '%' symbol to 'percent' column
    label_percent <- purrr::map(my_df[[percent]],
                         ~ paste0(round(.x, 1), "%"))
    
    # Conditionally add percent numbers to the bars based on 'default_percent_labels'
    if (default_percent_labels) {
      one_plot <-
        one_plot +
        geom_text(
          aes(label =
                paste0(round(!!sym(
                  percent
                ), 1), "%")),
          # in the middle of the bar
          position =
            position_stack(vjust = percent_label_pos),
          size = geom_text_size
        )
      
    } else {
      one_plot <-
        one_plot + annotate("text",
                            x = 1:2,
                            y = 20,
                            label = label_percent)
    }
    
    
    # Conditionally remove the legend from the plot based on 'no_legend'
    # to use with grid arrange multiple plots
    if (no_legend) {
      one_plot <- one_plot +
        theme(legend.position = "none")
    }
    
    # Return the 'one_plot' as the result of the function
    return(one_plot)
  }

# percent buckets
# Define a function named 'get_p_buckets' with two parameters 'my_df' and 'field_name'.
# Depending on the range in which the field_name value falls, the corresponding bucket label is assigned to each row in the percent_n_compl_rank column.
get_p_buckets <- function(my_df, field_name) {

  # Modify 'my_df' by adding a new column 'percent_n_compl_rank' using 'mutate' from dplyr
  # The column values are determined based on the 'field_name' value using 'case_when'
  my_df %>% 
    dplyr::mutate(
      # Create a new column 'percent_n_compl_rank'
      percent_n_compl_rank = 
        dplyr::case_when(
          # Use '!!sym(field_name)' to dynamically refer to 'field_name' within the context of 'my_df'
          # If 'field_name' < 25, set 'percent_n_compl_rank' to '0<= & <25%', etc.
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) & !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) & !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>% 
    # Return the modified data frame
    return()
}

# percent buckets by 50%
# Define a function named 'get_2_buckets' with two parameters 'my_df' and 'field_name'
get_2_buckets <- function(my_df, field_name) {
  # Modify 'my_df' by adding a new column 'percent_non_compl_2_buckets' using 'mutate' from dplyr
  my_df %>%
    # Use 'dplyr::mutate' to create or modify columns in 'my_df'
    dplyr::mutate(

      # Create a new column 'percent_non_compl_2_buckets'
      percent_non_compl_2_buckets =
        dplyr::case_when(
          # Use '!!sym(field_name)' to dynamically refer to 'field_name' within the context of 'my_df'
          # If 'field_name' < 50, set 'percent_non_compl_2_buckets' to '< 50%'       
          !!sym(field_name) < 50 ~ '< 50%',
          # If 'field_name' is greater than or equal to 50, set 'percent_non_compl_2_buckets' to '>= 50%'
          50 <= !!sym(field_name) ~ '>= 50%'
        )) %>%
    # Return the modified data frame
    return()
}

# saves to PNG, PDF etc. depending on an extension in "file_full_name"
save_plots_list_to_files <-
  function(file_full_name,
           plots_list,
           my_width = 30,
           my_height = 20) {
    ggplot2::ggsave(
      file_full_name,
      plots_list,
      width = my_width,
      height = my_height,
      units = "cm"
    )
  }
