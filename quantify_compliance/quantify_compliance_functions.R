# quantify_compliance_functions

get_non_compl_week_counts_percent <- function(my_df, vessel_id_col_name) {
  # browser()
    my_df %>%
    # how many non_compliant weeks per vessel this month
    count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # nc weeks per month
    count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # sum nc by month to get Total
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%
    # turn to have num of weeks per month in a row
    pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # count percentage
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
      geom_col(fill = "lightblue") +
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

