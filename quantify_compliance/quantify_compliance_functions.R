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
           no_legend = FALSE) {
    # browser()
    one_plot <-
      my_df %>%
      ggplot(aes(x = !!sym(is_compliant),
                 y = !!sym(percent),
                 fill = !!sym(is_compliant))) +
      geom_col() +
      geom_text(aes(label =
                      paste0(round(!!sym(percent), 1), "%")),
                position = position_stack(vjust = 0.5)) +
      labs(title = current_title,
           x = "",
           y = "") +
      scale_fill_manual(
        values =
          c(
            "percent_compl" = "lightgreen",
            "percent_non_compl" = "red"
          ),
        name = "Is compliant?",
        labels = c("Yes", "No")
      ) +
      scale_x_discrete(labels = c("Yes", "No")) +
      # scale_y_continuous(limits = c(0, 100), labels = scales::percent)
      ylim(0, 100)
    # +
    # scale_y_continuous(labels = scales::label_percent(scale = 1))

    if (no_legend) {
      one_plot <- one_plot +
        theme(legend.position = "none")
    }

    return(one_plot)
  }
