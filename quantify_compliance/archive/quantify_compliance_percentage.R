source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

## ---- by vessel ----
compl_clean_sa_vs_gom_plus_dual_short <-
  compl_clean_sa_vs_gom_plus_dual %>% 
  select(permit, compliant_, vessel_official_number, week_start, year_month, year_quarter, year)
  
## by year ----
## ---- total counts ----

compl_clean_sa_vs_gom_plus_dual_all <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  dplyr::group_by(vessel_official_number, permit, year) %>% 
  summarise(total_count = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_all)

compl_clean_sa_vs_gom_plus_dual_y <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  filter(tolower(compliant_) == "yes") %>%
  dplyr::group_by(vessel_official_number, permit, year) %>% 
  summarise(compliant_yes = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_y)

compl_clean_sa_vs_gom_plus_dual_y_t <-
full_join(compl_clean_sa_vs_gom_plus_dual_all,
          compl_clean_sa_vs_gom_plus_dual_y,
          by = c("vessel_official_number", "year", "permit"))

str(compl_clean_sa_vs_gom_plus_dual_y_t)
# 'data.frame':	6472 obs. of  4 variables:

## percent by year ----
percent_by_year <-
  compl_clean_sa_vs_gom_plus_dual_y_t %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(vessel_official_number, permit, year) %>%
  summarise(year_percent_yes = compliant_yes * 100 / total_count,
            year_percent_no = 100 - year_percent_yes)


## by month ----

## ---- total counts ----
compl_clean_sa_vs_gom_plus_dual_all_m <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  dplyr::group_by(vessel_official_number, permit, year_month) %>% 
  summarise(total_count_m = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_all_m)

compl_clean_sa_vs_gom_plus_dual_m <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  filter(tolower(compliant_) == "yes") %>%
  dplyr::group_by(vessel_official_number, permit, year_month) %>% 
  summarise(compliant_yes = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_m)

## percent by month
compl_clean_sa_vs_gom_plus_dual_m_t <-
  full_join(compl_clean_sa_vs_gom_plus_dual_all_m,
            compl_clean_sa_vs_gom_plus_dual_m,
            by = c("vessel_official_number", "year_month", "permit"))

str(compl_clean_sa_vs_gom_plus_dual_m_t)

percent_by_month <-
  compl_clean_sa_vs_gom_plus_dual_m_t %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(vessel_official_number, permit, year_month) %>%
  summarise(month_percent_yes = compliant_yes * 100 / total_count_m,
            month_percent_no = 100 - month_percent_yes)

head(percent_by_month)

## ---- by quarter ----

## ---- total counts ----
compl_clean_sa_vs_gom_plus_dual_all_q <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  dplyr::group_by(vessel_official_number, permit, year_quarter) %>% 
  summarise(total_count_q = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_all_q)

compl_clean_sa_vs_gom_plus_dual_q <-
  compl_clean_sa_vs_gom_plus_dual_short %>% 
  filter(tolower(compliant_) == "yes") %>%
  dplyr::group_by(vessel_official_number, permit, year_quarter) %>% 
  summarise(compliant_yes = n()) %>%
  as.data.frame()

head(compl_clean_sa_vs_gom_plus_dual_q)

## percent by quarter
compl_clean_sa_vs_gom_plus_dual_q_t <-
  full_join(compl_clean_sa_vs_gom_plus_dual_all_q,
            compl_clean_sa_vs_gom_plus_dual_q,
            by = c("vessel_official_number", "year_quarter", "permit"))

str(compl_clean_sa_vs_gom_plus_dual_q_t)

percent_by_quarter <-
  compl_clean_sa_vs_gom_plus_dual_q_t %>%
  replace(is.na(.), 0) %>%
  dplyr::group_by(vessel_official_number, permit, year_quarter) %>%
  summarise(quarter_percent_yes = compliant_yes * 100 / total_count_q,
            quarter_percent_no = 100 - quarter_percent_yes)

head(percent_by_quarter)

## plots bt vessel ----
## gom ----
percent_by_quarter %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("quarter"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  filter(vessel_official_number == "1000042") %>%
  # head(100) %>%
  ggplot(aes(
    x = year_quarter,
    y = compliance_percent,
    fill = yes_or_no
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))
# +
#   geom_text(aes(label = vessel_official_number),
#             position = position_dodge(width = 0.9),
#             vjust = -0.25)
  
#   ylim(0, 100)

head(percent_by_year)
percent_by_year %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("year_percent"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  filter(vessel_official_number == "1000042") %>%
  # head(100) %>%
  ggplot(aes(
    x = year,
    y = compliance_percent,
    fill = yes_or_no
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45))

head(percent_by_month)
percent_by_month %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("month_percent"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  filter(vessel_official_number == "1000042") %>%
  # head(100) %>%
  ggplot(aes(
    x = year_month,
    y = compliance_percent,
    fill = yes_or_no
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Monthly ompliance for '1000042'",
       x ="month", y = "compliance percent")
  
# one_vessel_plot <- function() {
month_p <-
  percent_by_month %>% 
    filter(permit == "gom") %>%
    tidyr::pivot_longer(starts_with("month_percent"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
    filter(vessel_official_number == "1000042") %>%
    # head(100) %>%
    ggplot(aes(
      x = year_month,
      y = compliance_percent,
      fill = yes_or_no
    )) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 45)) +
    labs(title = "Monthly compliance for '1000042'",
         x ="month", y = "compliance percent")
  
# }

# by quarter ----
quarter_p <-
  percent_by_quarter %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("quarter_percent"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  filter(vessel_official_number == "1000042") %>%
  # head(100) %>%
  ggplot(aes(
    x = year_quarter,
    y = compliance_percent,
    fill = yes_or_no
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Quarterly compliance for '1000042'",
       x ="quarter", y = "compliance percent")

year_p <-
  percent_by_year %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("year_percent"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  filter(vessel_official_number == "1000042") %>%
  # head(100) %>%
  ggplot(aes(
    x = year,
    y = compliance_percent,
    fill = yes_or_no
  )) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Annually compliance for '1000042'",
       x ="year", y = "compliance percent")

# arrange ----
legend <-
  cowplot::get_legend(month_p + theme(legend.position = "right"))

grid.arrange(
  month_p + theme(legend.position = 'hidden'),
  quarter_p + theme(legend.position = 'hidden'),
  year_p + theme(legend.position = 'hidden'),
  legend,
  nrow = 2
  # top = "compliance for '1000042'",
  # left = "YES and NO percent",
  # right = legend
)


## plot 2 ----
head(percent_by_quarter)
percent_by_quarter %>% 
  filter(permit == "gom") %>%
  head(100) %>%
  ggplot(aes(x = year_quarter,
             y = reorder(vessel_official_number, 
                         as.integer(factor(quarter_percent_yes)), FUN = min), 
             fill = quarter_percent_yes)
         ) + 
  labs(title = "Compliance by quarter",
       x ="quarter", y = "Vessel official number") +
  theme(axis.text.x = element_text(angle = 45),
        axis.text.y = element_blank()) +
  geom_bar(stat = "identity")


## plot 3 ----
head(percent_by_quarter)

percent_by_quarter %>% 
  filter(permit == "gom") %>%
  tidyr::pivot_longer(starts_with("quarter"), names_to = "yes_or_no", values_to = "compliance_percent") %>%
  head(100) %>%
  ggplot(aes(x = year_quarter,
             y = reorder(vessel_official_number, 
                         as.integer(factor(compliance_percent)),
                         FUN = min), 
             fill = yes_or_no)
         ) + 
  labs(title = "Compliance by quarter",
       x ="quarter", y = "vessel official number") +
  theme(axis.text.x = element_text(angle = 45)
        # ,
        # axis.text.y = element_blank()
        ) +
  geom_bar(stat = "identity")



## ==== old ====



## ---- compliance numbers by permit group and time period ----
by_week <-
  compl_clean_sa_vs_gom_plus_dual %>%
  dplyr::group_by(permit,
           compliant_,
           week) %>%
  summarise(n = n())
str(by_week)
# gropd_df [240 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

# with both
# gropd_df [360 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

# compl_clean_sa_vs_gom_plus_dual$year_month_w_start <-
# floor_date(compl_clean_sa_vs_gom_plus_dual$week_start  # Create year-month column
# )

## ---- by month ----
compl_clean_sa_vs_gom_plus_dual__months <-
  compl_clean_sa_vs_gom_plus_dual %>%
  dplyr::mutate(
    month_w_start = format(week_start, "%m"),
    month_w_end = format(week_end, "%m"),
    year_month_w_start = floor_date(week_start),
    year_month_w_end = floor_date(week_end)
  )

by_month_w_start <-
  compl_clean_sa_vs_gom_plus_dual__months %>%
  dplyr::group_by(permit,
           month_w_start) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))
str(by_month_w_start)

by_month_w_end <-
  compl_clean_sa_vs_gom_plus_dual__months %>%
  dplyr::group_by(permit,
           month_w_end) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

str(by_month_w_end)
setdiff(by_month_w_start$compl_by_not, by_month_w_end$compl_by_not)
# 24
setdiff(by_month_w_end$compl_by_not, by_month_w_start$compl_by_not)

## ---- the proportion by week ----
compl_clean_sa_vs_gom_plus_dual__weeks <-
  compl_clean_sa_vs_gom_plus_dual %>%
  dplyr::group_by(permit,
           week_start) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

## ---- the proportion by quarter ----
compl_clean_sa_vs_gom_plus_dual_q <-
  compl_clean_sa_vs_gom_plus_dual %>%
  dplyr::mutate(year_quarter = as.yearqtr(week_start)) %>%
  dplyr::group_by(permit,
           year_quarter) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

## ---- plots for proportions ----
names(by_month_w_start)
p1 <-
  ggplot(by_month_w_start,
         aes(x = month_w_start,
             y = compl_by_not, color = permit))
p1 + geom_point()

# sa * 10 for visibility
by_month_w_start_10 <-
  by_month_w_start %>%
  dplyr::mutate(sa_multiple_10 = if_else(startsWith(permit,  'sa'), compl_by_not * 10, compl_by_not))

tail(by_month_w_start_10)

p2 <-
  ggplot(by_month_w_start_10,
         aes(x = month_w_start,
             y = sa_multiple_10, color = permit))
p2 + geom_point()

# using year_month, SA * 10
p3 <-
  compl_clean_sa_vs_gom_plus_dual_y_m %>%
  dplyr::mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = year_month,
             y = sa_multiple_10, color = permit))

p3 + geom_point() +
  labs(title = "SUM(YES) / SUM(NO) for 'Compliant' per month",
       y = "Numbers for SA are *10 for comparison",
       x = "Month_Year") +
  theme(axis.text.x = element_text(angle = 45))

# by year

compl_clean_sa_vs_gom_plus_dual_y_by_10 <-
  compl_clean_sa_vs_gom_plus_dual_y %>%
  dplyr::mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not))
min_compl_by_not = min(compl_clean_sa_vs_gom_plus_dual_y_by_10$sa_multiple_10)
max_compl_by_not = max(compl_clean_sa_vs_gom_plus_dual_y_by_10$compl_by_not)

p4 <-
  compl_clean_sa_vs_gom_plus_dual_y %>%
  ggplot(aes(x = year,
             y = compl_by_not, fill = permit))


p4 + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Per year",
       y = "SUM(YES) / SUM(NO) for 'Compliant'",
       x = "Year") +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_text(aes(label = round(compl_by_not, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.25)

# per week
# have to exclude the week starts 2023-02-20 (the last in this data)
# bc 1053/1
tail(compl_clean_sa_vs_gom_plus_dual__weeks)
compl_clean_sa_vs_gom_plus_dual %>%
  filter(week_start > "2023-02-19") %>%
  select(permit, compliant_, week_start, week) %>%
  dplyr::group_by(permit,
           week_start) %>%
  summarise(sum(tolower(compliant_) == "yes"))
# summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
# (sum(tolower(compliant_) == "no"))) %>%

# head()
# compl_clean_sa_vs_gom_plus_dual__weeks %>% head()

p5 <-
  compl_clean_sa_vs_gom_plus_dual__weeks %>%
  filter(week_start < "2023-02-20") %>%
  dplyr::mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = week_start,
             # y = compl_by_not,
             y = sa_multiple_10,
             color = permit))

p5 + geom_point() +
  labs(title = "SUM(YES) / SUM(NO) for 'Compliant' per week",
       y = "Numbers for SA are *10 for comparison",
       x = "week start") +
  theme(axis.text.x = element_text(angle = 45))

# plot by quarter

p6 <-
  compl_clean_sa_vs_gom_plus_dual_q %>%
  dplyr::mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = year_quarter,
             # y = compl_by_not,
             y = sa_multiple_10,
             fill = permit))

p6 + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "SUM(YES) / SUM(NO) for 'Compliant' per quarter",
       y = "Numbers for SA are *10 for comparison",
       x = "quarter") +
  # labs(title = "Per quarter",
  #      y = "SUM(YES) / SUM(NO) for 'Compliant'",
  #      x = "Quarter") +
  # theme(
  #   axis.text.x = element_text(angle = 45)
  # )
  # +
  # compl_by_not
  geom_text(aes(label = round(sa_multiple_10, 2)),
            position = position_dodge2(width = 1.3),
            vjust = -0.25)

## ---- show the proportion compliant and the proportion non compliant? And show Gulf on a separate plot than SA ----

## ==== Gulf ====
gom_w_start_compl <-
  compl_clean_sa_vs_gom_plus_dual %>%
  filter(permit == "gom") %>%
  select(compliant_, week_start, year_month, year_quarter, year) %>%
  # gather(key, val, compliant_)
  tidyr::pivot_longer("compliant_", names_to = "key", values_to = "compliant")
head(gom_w_start_compl)

## ---- week ----
gom_w_start_compl_weekly <-
  dplyr::count(gom_w_start_compl, week_start, compliant)

head(gom_w_start_compl_weekly)
# [1] 120   3
# [1] "week_start" "val"        "n"

#   spread(val, n)
# tibble [60 × 3] (S3: tbl_df/tbl/data.frame)
# "week_start" "NO"         "YES"

## ---- month ----

gom_w_start_compl_monthly <-
  gom_w_start_compl %>%
  dplyr::count(year_month, compliant)

## ---- quarter ----

gom_w_start_compl_quarterly <-
  gom_w_start_compl %>%
  dplyr::count(year_quarter, compliant)

## ---- year ----

gom_w_start_compl_yearly <-
  gom_w_start_compl %>%
  dplyr::count(year, compliant)

## ---- individual plots  ----
## ---- gom_week ----
gom_week <-
  gom_w_start_compl_weekly %>%
  ggplot(aes(x = week_start,
             y = n,
             fill = compliant))

gom_week + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "GOM compliants per week",
       y = "YES and NO counts",
       x = "week") +
  theme(axis.text.x = element_text(angle = 45))

## ---- gom_month ----
gom_month <-
  gom_w_start_compl_monthly %>%
  ggplot(aes(x = year_month,
             y = n,
             fill = compliant))

gom_month + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "GOM compliants per month",
       y = "YES and NO counts",
       x = "year_month") +
  theme(axis.text.x = element_text(angle = 45))

## ---- gom_quarter ----
gom_quarter <-
  gom_w_start_compl_quarterly %>%
  ggplot(aes(x = year_quarter,
             y = n,
             fill = compliant))

gom_quarter + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "GOM compliants per quarter",
       y = "YES and NO counts",
       x = "year_quarter") +
  theme(axis.text.x = element_text(angle = 45))

gom_plot <- function(gom_w_start_compl, time_period) {
  # browser()
  counts_by_period <-
    dplyr::count(gom_w_start_compl, !!sym(time_period), compliant)
  
  gom_p <-
    counts_by_period %>%
    ggplot(aes(
      x = !!sym(time_period),
      y = n,
      fill = compliant
    ))
  
  gom_p + geom_bar(position = "dodge", stat = "identity") +
    labs(
      title = paste0("GOM compliants per ", time_period),
      y = "YES and NO counts",
      x = time_period
    ) +
    theme(axis.text.x = element_text(angle = 45)) %>%
    return()
}
p_gom_per_week <- gom_plot(gom_w_start_compl, "week_start")
p_gom_per_month <- gom_plot(gom_w_start_compl, "year_month")
p_gom_per_quarter <- gom_plot(gom_w_start_compl, "year_quarter")
p_gom_per_year <- gom_plot(gom_w_start_compl, "year")

## ==== SA only ====
sa_only_w_start_compl <-
  compl_clean_sa_vs_gom_plus_dual %>%
  filter(permit == "sa_only") %>%
  select(compliant_, week_start, year_month, year_quarter, year) %>%
  tidyr::pivot_longer("compliant_", names_to = "key", values_to = "compliant")

sa_only_plot <- function(sa_only_w_start_compl, time_period) {
  counts_by_period <-
    dplyr::count(sa_only_w_start_compl, !!sym(time_period), compliant)
  
  sa_only_p <-
    counts_by_period %>%
    ggplot(aes(
      x = !!sym(time_period),
      y = n,
      fill = compliant
    ))
  
  sa_only_p + geom_bar(position = "dodge", stat = "identity") +
    labs(title = paste0("per ", time_period),
         y = "",
         x = time_period) +
    #
    # labs(title = paste0("sa_only compliants per ", time_period),
    #      y = "YES and NO counts",
    #      x = time_period) +
    theme(axis.text.x = element_text(angle = 45)) %>%
    return()
}
p_sa_only_per_week <-
  sa_only_plot(sa_only_w_start_compl, "week_start")
p_sa_only_per_month <-
  sa_only_plot(sa_only_w_start_compl, "year_month")
p_sa_only_per_quarter <-
  sa_only_plot(sa_only_w_start_compl, "year_quarter")
p_sa_only_per_year <- sa_only_plot(sa_only_w_start_compl, "year")

plots_sa_list <- list(
  p_sa_only_per_week,
  p_sa_only_per_month,
  p_sa_only_per_quarter +
    geom_text(
      aes(label = n),
      position = position_dodge2(width = 1.3),
      vjust = -0.25
    ),
  p_sa_only_per_year +
    geom_text(
      aes(label = n),
      position = position_dodge(width = 0.9),
      vjust = -0.25
    )
)

legend <-
  cowplot::get_legend(plots_sa_list[[1]] + theme(legend.position = "right"))

grid.arrange(
  plots_sa_list[[1]] + theme(legend.position = 'hidden'),
  plots_sa_list[[2]] + theme(legend.position = 'hidden'),
  plots_sa_list[[3]] + theme(legend.position = 'hidden'),
  plots_sa_list[[4]] + theme(legend.position = 'hidden'),
  nrow = 2,
  top = "SA_only compliants",
  left = "YES and NO counts",
  right = legend
)

## ---- GOM plots together ----
gom_w_start_compl <-
  compl_clean_sa_vs_gom_plus_dual %>%
  filter(permit == "gom") %>%
  select(compliant_, week_start, year_month, year_quarter, year) %>%
  tidyr::pivot_longer("compliant_", names_to = "key", values_to = "compliant")

gom_plot <- function(gom_w_start_compl, time_period) {
  counts_by_period <-
    dplyr::count(gom_w_start_compl, !!sym(time_period), compliant)
  
  gom_p <-
    counts_by_period %>%
    ggplot(aes(
      x = !!sym(time_period),
      y = n,
      fill = compliant
    ))
  
  gom_p + geom_bar(position = "dodge", stat = "identity") +
    labs(title = paste0("per ", time_period),
         y = "",
         x = time_period) +
    #
    # labs(title = paste0("gom compliants per ", time_period),
    #      y = "YES and NO counts",
    #      x = time_period) +
    theme(axis.text.x = element_text(angle = 45)) %>%
    return()
}
p_gom_per_week <-
  gom_plot(gom_w_start_compl, "week_start")
p_gom_per_month <-
  gom_plot(gom_w_start_compl, "year_month")
p_gom_per_quarter <-
  gom_plot(gom_w_start_compl, "year_quarter")
p_gom_per_year <- gom_plot(gom_w_start_compl, "year")

plots_gom_list <- list(
  p_gom_per_week,
  p_gom_per_month,
  p_gom_per_quarter +
    geom_text(
      aes(label = n),
      position = position_dodge2(width = 1.3),
      vjust = -0.25
    ),
  p_gom_per_year +
    geom_text(
      aes(label = n),
      position = position_dodge(width = 0.9),
      vjust = -0.25
    )
)

legend <-
  cowplot::get_legend(plots_gom_list[[1]] + theme(legend.position = "right"))

grid.arrange(
  plots_gom_list[[1]] + theme(legend.position = 'hidden'),
  plots_gom_list[[2]] + theme(legend.position = 'hidden'),
  plots_gom_list[[3]] + theme(legend.position = 'hidden'),
  plots_gom_list[[4]] + theme(legend.position = 'hidden'),
  nrow = 2,
  top = "gom compliants",
  left = "YES and NO counts",
  right = legend
)

## ---- percentage ----
# It’s unclear what each figure is showing. What does “per week start” or “per year month" mean? Can we make the titles, “weekly compliance Gulf and dual permitted”, “monthly compliance Gulf and dual permitted”, “annual compliance Gulf and dual permitted”, and then the same for SA. So 6 total figures.

# Oh, and show it not as a number but a proportion. So, if we were looking at Gulf weekly compliance and it was 50 compliant vessels and 50 non-compliant, then the figures would be showing bars for 50% compliant and 50% non compliant. (50/100*100= 50%)

# Ok. We don’t want to sum the entries though. We just need to know vessel level compliance, weekly, monthly and annually.  How many vessels by gulf and SA. The file you have lists the compliance by vessel and permit type, so you’ll just need to analyze it that way. We can discuss Thursday, if it’s confusing.

## ==== Prepare compliance info ====
compl_clean_sa_vs_gom_plus_dual_short <-
  compl_clean_sa_vs_gom_plus_dual %>%
  select(
    vessel_official_number,
    compliant_,
    permit,
    week_start,
    year_month,
    year_quarter,
    year
  )

# tidyr::pivot_longer("compliant_", names_to = "key", values_to = "compliant")

gom_compl_clean_sa_vs_gom_plus_dual_short <-
  filter(compl_clean_sa_vs_gom_plus_dual_short, permit == "gom")
sa_compl_clean_sa_vs_gom_plus_dual_short <-
  filter(compl_clean_sa_vs_gom_plus_dual_short, permit == "sa_only")

my_percent <- function(x, y) {
  # y : 100%
  # x : b%
  return(x * 100 / y)
}

## ---- GOM + dual ----
gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  dplyr::group_by(vessel_official_number, compliant_, week_start) %>%
  summarise(n = n()) %>%
  summarise(by_week = sum(n)) %>%
  head()

gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  filter(week_start == "2022-12-26") %>%
  # select(vessel_official_number) %>% unique() %>%
  # Rows: 1,114
  dplyr::count(compliant_) %>%
  # dplyr::count(gom_w_start_compl, week_start, compliant)
  dplyr::glimpse()
# 1067 + 47 : 100
# 1067      : b

# (1067 * 100) / (1067 + 47)
# [1] 95.78097

# gom_compl_clean_sa_vs_gom_plus_dual_short %>%
#   dplyr::add_count(vessel_official_number, name = 'count') %>%
#   dplyr::group_by(vessel_official_number) %>%
#   dplyr::mutate(percent_yes = 100 * mean(compliant_),
#          percent_no = 100 - percent_yes) %>%  head()
# ungroup

gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  summarize(
    count = n(),
    percent_yes = my_percent(sum(compliant_ == "YES"), count),
    percent_no = my_percent(sum(compliant_ == "NO"), count)
  ) %>% head()

gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  dplyr::group_by(week_start) %>%
  summarize(
    count = n(),
    percent_yes = my_percent(sum(compliant_ == "YES"), count),
    percent_no = my_percent(sum(compliant_ == "NO"), count)
  ) %>%
  filter(week_start == "2022-12-26") %>%
  head()
# week_start count percent_yes percent_no
# <date>     <int>       <dbl>      <dbl>
# 2022-12-26  1114        95.8       4.22


gom_per_week <-
  gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  dplyr::group_by(week_start) %>%
  summarize(
    count = n(),
    percent_yes = my_percent(sum(compliant_ == "YES"), count),
    percent_no = my_percent(sum(compliant_ == "NO"), count)
  )

# str(gom_per_week)
# tibble [60 × 4] (S3: tbl_df/tbl/data.frame)

gom_per_month <-
  gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  dplyr::group_by(year_month) %>%
  summarize(
    count = n(),
    percent_yes = my_percent(sum(compliant_ == "YES"), count),
    percent_no = my_percent(sum(compliant_ == "NO"), count)
  )

str(gom_per_month)

gom_per_year <-
  gom_compl_clean_sa_vs_gom_plus_dual_short %>%
  dplyr::group_by(year) %>%
  summarize(
    count = n(),
    percent_yes = my_percent(sum(compliant_ == "YES"), count),
    percent_no = my_percent(sum(compliant_ == "NO"), count)
  )

gom_per_year
# 96.9/3.15 = 30.7619 = (sum(yes) / sum(no))

## ---- SA only ----
percent_by_time_period <- function(my_df, time_period_field_name) {
  my_df %>%
    dplyr::group_by(!!sym(time_period_field_name)) %>%
    summarize(
      count = n(),
      percent_yes = my_percent(sum(compliant_ == "YES"), count),
      percent_no = my_percent(sum(compliant_ == "NO"), count)
    ) %>%
    return()
}

compl_clean_sa_vs_gom_plus_dual %>%
  filter(permit == "sa_only") %>%
  select(vessel_official_number, compliant_, week, year) %>%
  dplyr::arrange(vessel_official_number, year, week) %>%
  tail()
sa_per_year <-
  percent_by_time_period(sa_compl_clean_sa_vs_gom_plus_dual_short, "year")

sa_per_month <-
  percent_by_time_period(sa_compl_clean_sa_vs_gom_plus_dual_short, "year_month")

sa_per_week <-
  percent_by_time_period(sa_compl_clean_sa_vs_gom_plus_dual_short, "week_start")

# test
# sa_per_week %>%
# filter(week_start == "2022-12-26")

# sa_compl_clean_sa_vs_gom_plus_dual_short %>%
# filter(week_start == "2022-12-26") %>%
# dplyr::group_by(compliant_, week_start) %>%
# summarise(n = n())

# my_percent(953, (684+953))
# 58.21625

## ---- plots for percentage ----

# sa_per_year_long <-
#   sa_per_year %>%
#   tidyr::pivot_longer(starts_with("percent"),
#                names_to = "key",
#                values_to = "percent")

# my_df <- sa_per_year_long
# time_period <- "year"
my_region <- c("Gulf and dual", "South Atlantic")

my_colors <- c("red", "blue")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

percent_plot <- function(my_df, time_period, region) {
  percent_p <-
    my_df %>%
    tidyr::pivot_longer(starts_with("percent"),
                 names_to = "key",
                 values_to = "percent") %>%
    ggplot(aes(
      x = !!sym(time_period),
      y = percent,
      fill = key
    ))
  
  my_title <- dplyr::case_when(
    time_period == "year" ~ "Annual",
    time_period == "year_month" ~ "Monthly",
    time_period == "week_start" ~ "Weekly"
  )
  
  my_x_lab <- dplyr::case_when(
    time_period == "year" ~ "year",
    time_period == "year_month" ~ "month",
    time_period == "week_start" ~ "week"
  )
  
  percent_p + geom_bar(position = "dodge", stat = "identity") +
    labs(
      title = paste0(my_title, " compliance"),
      # title = paste0(my_title, " compliance ",  region, " permitted"),
      y = "",
      x = my_x_lab
    ) +
    # scale_fill_manual(values = cbbPalette) +
    scale_fill_manual(values = my_colors,
                      name = "Colors",
                      labels = c("% non-compliant", "% compliant")) +
    theme(axis.text.x = element_text(angle = 45)) +
    ylim(0, 100) %>%
    return()
}
sa_per_year_p <- percent_plot(sa_per_year, "year", my_region[[2]])
sa_per_month_p <- percent_plot(sa_per_month, "year_month", my_region[[2]])
sa_per_week_p <- percent_plot(sa_per_week, "week_start", my_region[[2]])

gom_per_year_p <- percent_plot(gom_per_year, "year", my_region[[1]])
gom_per_month_p <- percent_plot(gom_per_month, "year_month", my_region[[1]])
gom_per_week_p <- percent_plot(gom_per_week, "week_start", my_region[[1]])

legend <-
  cowplot::get_legend(gom_per_week_p + theme(legend.position = "right"))

region <- my_region[[2]]
# sa_all <-
grid.arrange(
  sa_per_week_p + theme(legend.position = 'hidden'),
  sa_per_month_p + theme(legend.position = 'hidden'),
  sa_per_year_p + theme(legend.position = 'hidden'),
  legend,
  nrow = 2,
  top = paste0(region, " permitted"),
  left = "YES and NO percentage"
  # ,
  # right = legend
)

region <- my_region[[1]]
grid.arrange(
  gom_per_week_p + theme(legend.position = 'hidden'),
  gom_per_month_p + theme(legend.position = 'hidden'),
  gom_per_year_p + theme(legend.position = 'hidden') + 
    geom_text(aes(label = round(percent, 2)),
              position = position_dodge(width = 0.9),
              vjust = -0.25),
  legend,
  nrow = 2,
  top = paste0(region, " permitted"),
  left = "YES and NO percentage"
  # ,
  # right = legend
)

# with a function

#### save the legend separately

# ```{r legend}
my_legend <-
  cowplot::get_legend(gom_per_week_p)
# ```

#### combine plots

# ```{r combine plots}
arrange_plots <- function(plot_arr, region, legend) {
  plot_arr_hide_leg <-
    lapply(plot_arr, function(x)
      x + theme(legend.position = 'hidden'))
  
  grid.arrange(
    grobs = plot_arr_hide_leg,
    # add % on top of the bars
    geom_text(
      aes(label =
            paste0(round(percent, 2), "%")),
      position = position_dodge(width = 0.9),
      vjust = -0.25
    ),
    legend,
    nrow = 2,
    top = paste0(region, " permitted"),
    left = "YES and NO percentage"
  )
}
# ```
##### SA
# ```{r sa}

# SA
sa_region <- my_regions[[2]]
sa_plot_arr <- list(sa_per_week_p, sa_per_month_p, sa_per_year_p)

arrange_plots(sa_plot_arr, sa_region, my_legend)

# ```

##### GOM + dual
# ```{r GOM + dual}
gom_region <- my_regions[[1]]
gom_plot_arr <- list(gom_per_week_p, gom_per_month_p, gom_per_year_p)

arrange_plots(gom_plot_arr, gom_region, my_legend)
# ```
