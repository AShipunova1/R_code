source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")
## ---- count reports ----
str(compl_clean_sa_vs_gom_plus_dual)
compl_clean_sa_vs_gom_plus_dual_rep_cnt <-
  compl_clean_sa_vs_gom_plus_dual %>%
  mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
str(compl_clean_sa_vs_gom_plus_dual_rep_cnt)

## --- check by week ----
id_reports_one_week <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(week_start == "2022-12-26",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  # glimpse()
  # Rows: 47
  select(report_cnts, vessel_official_number)
# %>%
glimpse(id_reports_one_week)

# vessel_id_order = reorder(vessel_official_number,
# as.integer(factor(report_cnts)), FUN = min)

# mutate(order = fct_reorder(as.factor(week_num), year)) %>%

# id_reports %>%
#   mutate(vessel_id_order = reorder(vessel_official_number,
#                                    as.integer(factor(report_cnts)), FUN = min
#                                    )

# id_reports_one_week <- transform(id_reports_one_week,
#                                  report_cnts = reorder(report_cnts, -value)
#                                  )

density_plot <- function(my_df, time_period) {
  ggplot(my_df, aes(x = report_cnts)) +
    geom_histogram(binwidth = .5,
                   colour = "black",
                   fill = "white") +
    # geom_density() +
    geom_vline(
      aes(xintercept = mean(report_cnts, na.rm = T)),
      # Ignore NA values for mean
      color = "red",
      linetype = "dashed",
      linewidth = 1
    ) +
    labs(
      title = paste0(
        "Report counts for ",
        unique(time_period),
        " for GOM non-compliant vessels"
      ),
      x = "Report counts"
      #      x ="Year_week", y = "Vessel official number"
    )
}

density_plot(id_reports_one_week, "week")

data_overview(id_reports_one_week)
# Mean   : 3.809

## --- check by month ----
id_reports_one_month <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(year_month == "Apr 2022",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  # glimpse()
  # Rows: 47
  select(report_cnts, vessel_official_number)

glimpse(id_reports_one_month)
data_overview(id_reports_one_month)
# Mean   : 3.496

density_plot(id_reports_one_month, "month")


compl_clean_sa_vs_gom_plus_dual_rep_cnt_gom_non_compl <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(permit == "gom",
         tolower(compliant_) == "no") %>%
  select(report_cnts, vessel_official_number, year_month)

monthly_count_density_plots <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt_gom_non_compl %>%
  group_by(year_month) %>%
  group_map(.f = ~ density_plot(.x,
                                .x$year_month),
            .keep = TRUE)



# group_map(
#   .f = ~ ggplot(.x, aes(x = report_cnts)) +
#     geom_histogram(binwidth = .5,
#                    colour = "black",
#                    fill = "white") +
#     # geom_density() +
#     geom_vline(
#       aes(xintercept = mean(report_cnts, na.rm = T)),
#       # Ignore NA values for mean
#       color = "red",
#       linetype = "dashed",
#       linewidth = 1
#     )
#
# )
# ~ head(.x, 2L))
# ~ quantile(.x$report_cnts)
# function(x) {
#   x
#   # m <- x$year_month
#   # density_plot(m)
# }


# iris %>% group_by(Species) %>% do(plots=ggplot(data=.) +
# aes(x=Petal.Width, y=Petal.Length) + geom_point() + ggtitle(unique(.$Species)))
grid.arrange(grobs = monthly_count_density_plots, 
             # top = super_title, 
             ncol = 3)

