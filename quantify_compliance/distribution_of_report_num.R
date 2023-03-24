# !Different amo8nt of vessels per period
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
  # browser()
  # put mean line text to the right of the line
  vline_text_x = mean(my_df$report_cnts, na.rm = T) + 0.7
  # put mean line text on top and a little bit lower
  vline_text_y = (my_df %>% count(report_cnts) %>% max()) * 3/4
    # max(my_df$report_cnts)
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
    annotate(
      "text",
      x = vline_text_x,
      # y = 20,
      y = vline_text_y,
      label = round(mean(my_df$report_cnts, na.rm = T), 2),
      color = "red",
      angle = 90
    )  +
    labs(title =
           # paste0(
           # "Report counts for ",
           unique(time_period),
         # " for GOM non-compliant vessels"
         # )
         # ,
         x = "",
         y = "",
         size = 3
         # x = "Report counts"
         #      x ="Year_week", y = "Vessel official number"
         
         )
}

density_plot(id_reports_one_week, "week")

head(id_reports_one_week)
id_reports_one_week %>% count(report_cnts)
# 0 : 23
# 1: 3
id_reports_one_week %>%
  filter(report_cnts == 0) %>% dim()
# [1] 23  2

# data_overview(id_reports_one_week)
# Mean   : 3.809

## --- check by month ----
id_reports_one_month <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(year_month == "Feb 2022",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  # glimpse()
  # Rows: 47
  select(report_cnts, vessel_official_number)

# glimpse(id_reports_one_month)
# data_overview(id_reports_one_month)
# Mean   : 2.445

id_reports_one_month %>% count(report_cnts)
# 1           0    65
# 2           1     7
# ...
# 16          15     2
# 17          19     1

id_reports_one_month %>%
  filter(report_cnts == 0) %>% dim()
# 65  
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
            # keep year_month (the grouping variable)
            .keep = TRUE)

super_title = "Monthly report counts for GOM non-compliant vessels"

grid.arrange(
  grobs = monthly_count_density_plots,
  top = super_title,
  left = "Amount of report counts",
  bottom = "Report counts",
  ncol = 4
)

##  SA ----