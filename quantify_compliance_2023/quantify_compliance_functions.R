# quantify_compliance_functions

# Create a list called 'text_sizes' that contains named elements with different font sizes.
text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 15,
  axis_text_y_size = 15,
  plot_caption_text_size = 12,
  # common axes for Months
  y_left_fontsize = 10
)

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
