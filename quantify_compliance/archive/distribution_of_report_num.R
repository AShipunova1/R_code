# !Different amount of vessels per period
# Use "expected counts" for SA

# Michelle:
# I like the visualization approach. So this is showing # of vessels with # of missing reports? I'd like to consider more as a proportion of how many they have missing vs how many they have done correctly. So, if we look at a month of non-compliant vessels for the GOM, if 65 vessels are missing 1 report (1) is that a missing declaration or logbook?, (2) how many did they submit correctly (e.g. this month in question maybe they are missing 1 logbook, but they submitted 20. So, 1/20 = 5% missing). That is a vessel level, so then perhaps take the average of all for the month, and then the year, by region, and plot it that way. Does that make sense? The main question is to see how usable the data is, i.e., if they are submitting most of the reports then its still good data. So we need to determine that.
#
# Also, if 65 vessels had 0 reports submitted how many reports should they have submitted? What does the 0 mean for a GOM vessel, when a GOM vessel doesn't need to submit reports?

# 03/24/2023
# > What does the 0 mean for a GOM vessel, when a GOM vessel doesn't need to submit reports?
# Just that, there were 0 reports and the vessel marked as non_compliant
# that seems odd. We should probably look into why this is happening for a Gulf vessel. But low priority since the Gulf program is now set aside.
# >  if 65 vessels had 0 reports submitted how many reports should they have submitted?
#   There is no "expected" amount for GOM, right?
#   correct. In the GOM, they are only required to declare IF they move on water (or they submitted a logbook), and only submit a logbook IF they declared they intended to fish.
# For SA, they need either a no fishing report weekly (due Tuesday following the fishing week, which is Mon-Sun) or they need a logbook for every fishing trip (so at least 1 per week if no fishing report is submitted)
# > So this is showing # of vessels with # of missing reports?
# # of vessels with # of submitted reports for non_compliant vessels only
# that would be for Gulf then? For SA they are non-compliant if no reports were submitted
#
# One more thing to consider, each month will have a slightly different number of vessels, should we take it into account?
#   no, that is expected due to fluctuations in permit statuses. We just need to look at this month by month, and annually - to address the data usability concern.

library(grid)
source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

## ---- count reports ----
str(compl_clean_sa_vs_gom_plus_dual)
compl_clean_sa_vs_gom_plus_dual_rep_cnt <-
  compl_clean_sa_vs_gom_plus_dual %>%
  dplyr::mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
str(compl_clean_sa_vs_gom_plus_dual_rep_cnt)

density_plot <- function(my_df, time_period) {
  # put mean line text to the right of the line
  vline_text_x = mean(my_df$report_cnts, na.rm = T) + 0.7
  # put mean line text on top and a little bit lower
  vline_text_y = (my_df %>% dplyr::count(report_cnts) %>% max()) * 3 / 4
  
  ggplot(my_df, aes(x = report_cnts)) +
    geom_histogram(binwidth = .5,
                   colour = "black",
                   fill = "white") +
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
      y = vline_text_y,
      # mean of reports_cnts, rounded to 2 decimals
      label = round(mean(my_df$report_cnts, na.rm = T), 2),
      color = "red",
      angle = 90
    )  +
    labs(
      title = unique(time_period),
      # no x and y labels
      x = "",
      y = "",
      size = 3
    )
}

## ---- GOM + dual ----

## --- check by week ----
id_reports_one_week <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(week_start == "2022-12-26",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  # dplyr::glimpse()
  # Rows: 47
  select(report_cnts, vessel_official_number)

glimpse(id_reports_one_week)

density_plot(id_reports_one_week, "Week of 2022-12-26")

### check numbers ----
head(id_reports_one_week)
id_reports_one_week %>% dplyr::count(report_cnts) %>% head(2)
# 0 : 23
# 1: 3
id_reports_one_week %>%
  filter(report_cnts == 0) %>% dim()
# [1] 23  2

summary(id_reports_one_week)
# Mean   : 3.809

## --- check by month ----
id_reports_one_month <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(year_month == "Feb 2022",
         permit == "gom",
         tolower(compliant_) == "no") %>%
  select(report_cnts, vessel_official_number)

glimpse(id_reports_one_month)
summary(id_reports_one_month)
# Mean   : 2.445

id_reports_one_month %>% dplyr::count(report_cnts)
# 1           0    65
# 2           1     7
# ...
# 16          15     2
# 17          19     1

id_reports_one_month %>%
  filter(report_cnts == 0) %>% dim()
# 65

density_plot(id_reports_one_month, "Feb 2022")

## ---- GOM: plot all months ====
compl_clean_sa_vs_gom_plus_dual_rep_cnt_gom_non_compl <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(permit == "gom",
         tolower(compliant_) == "no") %>%
  select(report_cnts, vessel_official_number, year_month)

monthly_count_density_plots <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt_gom_non_compl %>%
  dplyr::group_by(year_month) %>%
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
### --- check by month ----
sa_id_reports_one_month <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(year_month == "Feb 2022",
         permit == "sa_only",
         tolower(compliant_) == "no") %>%
  # dplyr::glimpse()
  select(report_cnts, vessel_official_number, week_start)

glimpse(sa_id_reports_one_month)
summary(sa_id_reports_one_month)
# Mean   : 0.9171
# Max.   :15.0000

sa_id_reports_one_month %>% dplyr::count(report_cnts)
# 1           0  2098
# 2           1   148
# 3           2     1
# ...
# 13          14    9
# 14          15    1

sa_id_reports_one_month %>%
  filter(report_cnts == 0) %>% dim()
# 2098

density_plot(sa_id_reports_one_month, "Feb 2022")

### get num of weeks == min of reports expected
num_of_weeks <- function(my_df) {
  my_df %>% select(week_start) %>% unique() %>% dplyr::count() %>% return()
}

### SA plot all months ----
compl_clean_sa_vs_gom_plus_dual_rep_cnt_sa_non_compl <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt %>%
  filter(permit == "sa_only",
         tolower(compliant_) == "no") %>%
  select(report_cnts, vessel_official_number, year_month, week_start)

# compl_clean_sa_vs_gom_plus_dual_rep_cnt_sa_non_compl %>%
# dplyr::group_by(year_month) %>%

my_colors = c('Mean' = 'red',
              'Num of weeks' = 'deepskyblue')

sa_density_plot <- function(my_df) {
  expected_min_amount_of_reports <- num_of_weeks(my_df)[[1]]
  # put mean line text to the right of the line
  vline2_text_x = expected_min_amount_of_reports + 0.7
  # put mean line text on top and a little bit lower
  vline2_text_y = (my_df %>% dplyr::count(report_cnts) %>% max()) * 3 / 4
  new_plot <-
    density_plot(my_df,
                 my_df$year_month) +
    geom_vline(
      aes(xintercept = expected_min_amount_of_reports),
      color = my_colors['Num of weeks'],
      # linetype = "dashed",
      linewidth = 1
    ) +
    annotate(
      "text",
      x = vline2_text_x,
      y = vline2_text_y,
      # mean of reports_cnts, rounded to 2 decimals
      label = round(expected_min_amount_of_reports),
      color = my_colors['Num of weeks'],
      angle = 90
    )
  
  return(new_plot)
}

monthly_count_density_plots <-
  compl_clean_sa_vs_gom_plus_dual_rep_cnt_sa_non_compl %>%
  dplyr::group_by(year_month) %>%
  group_map(.f = ~ sa_density_plot(.x),
            # keep year_month (the grouping variable)
            .keep = TRUE)

super_title = "Monthly report counts for SA non-compliant vessels"

grid.arrange(
  grobs = monthly_count_density_plots,
  top = super_title,
  left = "Amount of report counts",
  bottom = "Report counts",
  ncol = 4
)
# ===


## ---- this part doesn't work ----
### ---- make a legend ----
legend_data = data.frame(
  x1 = rep(0, 2),
  y1 = rep(0, 2),
  ll = c('Mean', 'Num of weeks')
)

legend_plot <-
  ggplot(data = legend_data, aes(x1, y1, colour = ll)) +
  geom_text(dat = legend_data,
            aes(label = ll),
            hjust = 0) +
  scale_color_manual(
    name = 'Lines',
    breaks = c('Mean', 'Num of weeks'),
    values = my_colors
  )

legend_plot

my_legend <-
  cowplot::get_legend(legend_plot)
# + theme(legend.position = "right"))

# +
#   theme(
#     legend.title = "Lines"
#   )
# legend.key.size = unit(15, "pt"),
# legend.title = element_blank(),
# legend.margin = margin(l = 0),
# legend.text = element_text(size = 12)
# ) +
# scale_colour_manual(values = rep("#00000000", 4))

my_legend

# my_legend <-
#   cowplot::get_legend(monthly_count_density_plots[[1]] + theme(legend.position = "bottom"))

# monthly_count_density_plots[[1]]

super_title = "Monthly report counts for SA non-compliant vessels"

# s<- lapply(monthly_count_density_plots, grobTree)
# str(monthly_count_density_plots[[1]])

combined_grob <-
  append(monthly_count_density_plots,
         my_legend)

# str(combined_grob[15])
# g1 <- arrangeGrob(grobs = gs, layout_matrix = t(lay))
# g2 <- arrangeGrob(grobs = gs, layout_matrix = lay)
# grid.arrange(g1, g2, ncol=2)

# combined_grob <-
# gList(lapply(monthly_count_density_plots, grobTree),
# grobTree(my_legend))

# tt <-grid.arrange(arrangeGrob(p6, p7, p8, p9, legend,
#                               nrow = 2, #
#                               left = textGrob("Mammalian species richness", rot = 90, vjust = 1,
#                                               gp = gpar(fontsize = 12))))

# my_layout_matrix = cbind(c(1,1,1), c(2,3,4))
my_layout_matrix = cbind(c(1, 1, 1, 1), c(2, 3, 4, 5))

class(monthly_count_density_plots)
class(my_legend)

lay <- rbind(c(1, 1, 1, 1),
             c(1, 1, 1, 1),
             c(1, 1, 1, 1))

# g1 <- arrangeGrob(grobs = gs, layout_matrix = lay)
g1 <-
  arrangeGrob(grobs = monthly_count_density_plots, layout_matrix = lay)
g2 <- my_legend

# save it
monthly_count_density_plots_orig <-
  monthly_count_density_plots

my_grobs = lapply(monthly_count_density_plots, ggplotGrob)

my_grobs_plus <- addGrob(my_grobs, my_legend)
my_treeGrob <- grobTree(my_grobs, my_legend)

grid.arrange(
  my_grobs,
  my_legend,
  # grobs = monthly_count_density_plots,
  # grobs = arrangeGrob(my_legend, monthly_count_density_plots),
  # grobs = combined_grob,
  # my_legend,
  # right = my_legend,
  top = super_title,
  left = "Amount of report counts",
  bottom = "Report counts",
  ncol = 4
)
# grid.arrange(grobTree(P1), grobTree(P2), grobTree(P3), grobTree(P4), ncol=2)
