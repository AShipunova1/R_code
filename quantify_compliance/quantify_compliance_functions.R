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
    count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # Count the occurrence of each unique combination of year_month and nc_weeks_per_vessl_m.
    count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # Calculate the total number of non-compliant vessels per month.
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%

    # Reshape the data to have a row for each 'non_compl_weeks' and its count in a month.
    pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # Calculate the percentage of non-compliant vessels in a month.
    mutate(percent_nc = round(
      100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month,
      digits = 2
    )) %>%
    return()
}

perc_plots_by_month <-
  function(my_df, current_year_month) {
    # browser()
    # month_title = current_year_month
    total_nc_vsl_per_month <-
      my_df %>%
      filter(year_month == current_year_month) %>%
      select(total_nc_vsl_per_month) %>%
      unique()

    # month_title = paste0(current_year_month, " Total non-compliant vessels: ", total_nc_vsl_per_month[[1]])
    month_title = paste0(current_year_month, ": ", total_nc_vsl_per_month[[1]], " total nc vsls")

    my_df %>%
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc)) +
      geom_col(fill = plot_colors$nc_bucket) +
      geom_text(aes(label = paste0(percent_nc, "%")),
                position = position_dodge(width = 0.9)
                # ,
                # vjust = -0.5
                ) +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 9)
            ) +
      ylim(0, 100) +
      labs(title = month_title,
           # x = "",
           x = "Num of nc weeks",
           y = "") %>%
      # TODO: axes text
      return()
  }

make_year_permit_label <- function(curr_year_permit) {
  curr_year_permit %>%
    stringr::str_replace("_dual", " + dual") %>%
    stringr::str_replace("_", " ") %>%
    toupper() %>%
    return()
}

make_one_plot_compl_vs_non_compl <-
  function(my_df,
           current_title = "",
           is_compliant = "is_compliant",
           percent = "percent",
           no_legend = FALSE,
           percent_label_pos = 0.5,
           default_percen_labels = TRUE,
           geom_text_size = text_sizes[["geom_text_size"]]
           ) {
    # browser()
    one_plot <-
      my_df %>%
      ggplot(aes(x = !!sym(is_compliant),
                 y = !!sym(percent),
                 fill = !!sym(is_compliant))) +
      geom_col() +
      theme(axis.text.x = 
              element_text(size = text_sizes[["axis_text_x_size"]]),
            axis.text.y = 
              element_text(size = text_sizes[["axis_text_y_size"]])) +
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
      labs(title = current_title,
           x = "",
           y = "") +
      scale_fill_manual(
        # use custom colors
        values =
          c(
            "compliant" = plot_colors[["compliant"]],
            "non_compliant" = plot_colors[["non_compliant"]]
          ),
        # Legend title
        name = "Is compliant?",
        labels = c("Yes", "No")
      ) +
      # manual x axes ticks labels
      scale_x_discrete(labels = c("Yes", "No")) +
      # scale_y_continuous(limits = c(0, 100), labels = scales::percent)
      # Y axes between 0 and 100
      ylim(0, 100)
    # +
    # scale_y_continuous(labels = scales::label_percent(scale = 1))

    label_percent <- map(my_df$perc_c_nc,
                          ~ paste0(round(.x, 1), "%"))
                   
    # Add percent numbers on the bars
    if (default_percen_labels) {
      one_plot <-
        one_plot +
        geom_text(aes(label =
                        paste0(round(!!sym(
                          percent
                        ), 1), "%")),
                  # in the middle of the bar
                  position =
                    position_stack(vjust = percent_label_pos),
                  size = geom_text_size)
      
    } else {
      one_plot <-
        one_plot + annotate("text",
                            x = 1:2,
                            y = 20,
                            label = label_percent)
    }
    
    
    
    # to use with grid arrange multiple plots
    if (no_legend) {
      one_plot <- one_plot +
        theme(legend.position = "none")
    }
    
    return(one_plot)
  }

# percent buckets
get_p_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_n_compl_rank =
        dplyr::case_when(
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) &
            !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) &
            !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>%
    return()
}

# percent buckets by 50%
get_2_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_non_compl_2_buckets =
        dplyr::case_when(
          # nc weeks 
          !!sym(field_name) < 50 ~ '< 50%',
          50 <= !!sym(field_name) ~ '>= 50%'
        )
    ) %>%
    return()
}

