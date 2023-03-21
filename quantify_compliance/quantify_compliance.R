library(zoo)

## ---- set up ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

## ---- looking at the proportion of reports submitted vs flag ----
# glimpse(compl_clean)
# Rows: 167,607
# Columns: 21

## ---- Have only SA permits, exclude those with Gulf permits ----
# get list of all permitgroups
all_permit_groups <- select(compl_clean, permitgroup) %>%
  unique() %>%
  paste(collapes = ",")

all_permit_groups %>%
  str_replace_all('\\"', "") %>%
  cat(sep = ', ',
      file = file.path(my_paths$outputs,
                       "permitgroups.txt"))


# separate into 3 groups
compl_clean_sa_vs_gom <-
  compl_clean %>%
  mutate(permit =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             !grepl("CDW|CHS|SC", permitgroup) ~ "gom_only",
             .default = "both"
           ))

names(compl_clean_sa_vs_gom)
# dim(compl_clean_sa_vs_gom)
# [1] 167607     22

# ---- gulf + dual ----
compl_clean_sa_vs_gom_plus_dual <-
  compl_clean %>%
  mutate(permit =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             # !grepl("CDW|CHS|SC", permitgroup) ~ "gom_only",
             .default = "gom"
           ))

## ---- compliance numbers by permit group and time period ----
by_week <-
  compl_clean_sa_vs_gom_plus_dual %>%
  group_by(permit,
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
  mutate(
    month_w_start = format(week_start, "%m"),
    month_w_end = format(week_end, "%m"),
    year_month_w_start = floor_date(week_start),
    year_month_w_end = floor_date(week_end)
  )


# by_y_month_w_start <-
#   compl_clean_sa_vs_gom_plus_dual__months %>%
#   group_by(permit,
#            compliant_,
#            year_month_w_start) %>%
#   summarise(n = n())
#
# str(by_y_month_w_start)
# gropd_df [240 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)

# by_y_month_w_end <-
#   compl_clean_sa_vs_gom_plus_dual__months %>%
#   group_by(permit,
#            compliant_,
#            year_month_w_end) %>%
#   summarise(n = n())
# str(by_y_month_w_end)

# month_w_start <-
#   compl_clean_sa_vs_gom_plus_dual__months %>%
#   group_by(permit,
#            compliant_,
#            month_w_start)

by_month_w_start <-
  compl_clean_sa_vs_gom_plus_dual__months %>%
  group_by(permit,
           month_w_start) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))
str(by_month_w_start)

by_month_w_end <-
  compl_clean_sa_vs_gom_plus_dual__months %>%
  group_by(permit,
           month_w_end) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

str(by_month_w_end)
setdiff(by_month_w_start$compl_by_not, by_month_w_end$compl_by_not)
# 24
setdiff(by_month_w_end$compl_by_not, by_month_w_start$compl_by_not)

# by_month_w_end <-
#   compl_clean_sa_vs_gom_plus_dual__months %>%
#   group_by(permit,
#            compliant_,
#            month_w_end) %>%
#   summarise(n = n())

# setdiff(by_month_w_start$n, by_month_w_end$n)

## ---- compliance info ----
compl_clean_sa_vs_gom_counts_w_err <-
  compl_clean_sa_vs_gom %>%
  group_by(
    permit,
    captainreports__,
    negativereports__,
    gom_permitteddeclarations__,
    compliant_,
    complianceerrors__
  ) %>%
  summarise(n = n())
# %>%
# summarise(complianceerrors = sum(as.integer(complianceerrors__))) %>%
# mutate(freq = n / sum(n)) %>%
# mutate(freq = (captainreports__ + negativereports__) / sa_only)
# mutate(per =  100 *count/sum(count)) %>%
str()
# gropd_df [831 × 6] (S3: grouped_df/tbl_df/tbl/data.frame)

# xgompermitteddeclarations vs. xcomplianceerrors
# logbook but no logbook
# filter_egregious <- quo(xgompermitteddeclarations == 0 &
#                           xcaptainreports == 0 &
#                           xnegativereports == 0 &
#                           xcomplianceerrors > 0
# )
# compl_clean_sa_non_compl <-
#   compl_clean_sa %>%
#   filter(!!filter_egregious)

# compl_clean_sa_vs_gom_counts %>% head()
# compl_clean_sa_vs_gom_counts %>% tail()

# dim(compl_clean_sa_vs_gom_counts_w_err)
# [1] 1654    7

compl_clean_sa_vs_gom_counts_w_err_not_coml <-
  compl_clean_sa_vs_gom_counts_w_err %>%
  filter(tolower(compliant_) == "no")

write.csv(
  compl_clean_sa_vs_gom_counts_w_err_not_coml,
  file.path(
    my_paths$outputs,
    "compl_clean_sa_vs_gom_counts_w_err_not_coml.csv"
  ),
  row.names = FALSE
)

# dim()
# [1] 378   7


# yes
# dim()
# [1] 1276    7

## ---- using ts() ----
select(compl_clean_sa_vs_gom_plus_dual, year) %>% unique()
# 22, 23
monthly <-
  ts(
    compl_clean_sa_vs_gom_plus_dual$week_start,
    start = c(2022, 1),
    frequency = 12
  )
str(monthly)

quarterly <- aggregate(monthly, nfrequency = 4)
str(quarterly)

compl_clean_sa_vs_gom_plus_dual_ts <-
  compl_clean_sa_vs_gom_plus_dual %>%
  mutate(
    monthly = ts(week_start, start = c(2022, 1), frequency = 12),
    qarterly = ts(week_start, start = c(2022, 1), frequency = 4)
  )

str(compl_clean_sa_vs_gom_plus_dual_ts)

compl_clean_sa_vs_gom_plus_dual_ts %>%
  group_by(permit,
           monthly) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

# using zoo
compl_clean_sa_vs_gom_plus_dual_y_m <-
  compl_clean_sa_vs_gom_plus_dual %>%
  mutate(year_month = as.yearmon(week_start)) %>%
  group_by(permit,
           year_month) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

# A tibble: 28 × 3

# zoo(rnorm(5))

# by year
compl_clean_sa_vs_gom_plus_dual_y <-
  compl_clean_sa_vs_gom_plus_dual %>%
  group_by(permit,
           year) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

# the proportion by week
compl_clean_sa_vs_gom_plus_dual__weeks <-
  compl_clean_sa_vs_gom_plus_dual %>%
  group_by(permit,
           week_start) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

# the proportion by quarter
compl_clean_sa_vs_gom_plus_dual_q <-
  compl_clean_sa_vs_gom_plus_dual %>%
  mutate(year_quarter = as.yearqtr(week_start)) %>%
  group_by(permit,
           year_quarter) %>%
  summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              (sum(tolower(compliant_) == "no")))

## ---- plot ----
names(by_month_w_start)
p1 <-
  ggplot(by_month_w_start,
         aes(x = month_w_start,
             y = compl_by_not, color = permit))
p1 + geom_point()

# sa * 10
by_month_w_start_10 <-
  by_month_w_start %>%
  mutate(sa_multiple_10 = if_else(startsWith(permit,  'sa'), compl_by_not * 10, compl_by_not))

tail(by_month_w_start_10)

p2 <-
  ggplot(by_month_w_start_10,
         aes(x = month_w_start,
             y = sa_multiple_10, color = permit))
p2 + geom_point()

# using year_month, SA * 10
p3 <-
  compl_clean_sa_vs_gom_plus_dual_y_m %>%
  mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = year_month,
             y = sa_multiple_10, color = permit))

p3 + geom_point() +
  labs(title = "SUM(YES) / SUM(NO) for 'Compliant' per month",
       y = "Numbers for SA are *10 for comparison",
       x = "Month_Year") +
  theme(
    axis.text.x = element_text(angle = 45)
  )

# by year

compl_clean_sa_vs_gom_plus_dual_y_by_10 <-
  compl_clean_sa_vs_gom_plus_dual_y %>%
  mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
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
  theme(
    axis.text.x = element_text(angle = 45)
  ) + 
  geom_text(aes(label = round(compl_by_not, 2)),
                position = position_dodge(width = 0.9),
                vjust = -0.25
  )

#per week
# have to exclude the week starts 2023-02-20 (the last in this data)
# bc 1053/1
tail(compl_clean_sa_vs_gom_plus_dual__weeks)
compl_clean_sa_vs_gom_plus_dual %>%
  filter(week_start > "2023-02-19") %>% 
  select(permit, compliant_, week_start, week) %>%
  group_by(permit,
           week_start) %>%
  summarise(sum(tolower(compliant_) == "yes"))
  # summarise(compl_by_not = (sum(tolower(compliant_) == "yes")) /
              # (sum(tolower(compliant_) == "no"))) %>%

  # head()
  # compl_clean_sa_vs_gom_plus_dual__weeks %>% head()
  
p5 <-
  compl_clean_sa_vs_gom_plus_dual__weeks %>%
  filter(week_start < "2023-02-20") %>%
  mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = week_start,
             # y = compl_by_not,
             y = sa_multiple_10,
             color = permit)
         )

p5 + geom_point() +
  labs(title = "SUM(YES) / SUM(NO) for 'Compliant' per week",
       y = "Numbers for SA are *10 for comparison",
       x = "week start") +
  theme(
    axis.text.x = element_text(angle = 45)
  )

# plot by quarter

p6 <-
  compl_clean_sa_vs_gom_plus_dual_q %>%
  mutate(sa_multiple_10 = if_else(startsWith(permit, 'sa'),
                                  compl_by_not * 10,
                                  compl_by_not)) %>%
  ggplot(aes(x = year_quarter,
             # y = compl_by_not,
             y = sa_multiple_10,
             fill = permit)
  )

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
            vjust = -0.25
  )

