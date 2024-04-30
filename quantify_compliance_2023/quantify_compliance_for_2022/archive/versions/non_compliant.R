# month and year:
#   sa how many reports are missing
# gom

source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

# for each vessel:
##  how many weeks total == min reports for SA
##  how many "NO" per each time period (res: 2342 vessel_id per m or y)
##  how many reports per each time period


## ---- get non compliant vessel ids, check if they are non-compliant in every month and year

get_non_compliant_vessels_per_time_period <- function(time_period) {
  sym_time_period <- sym(time_period)
  compl_clean_sa_vs_gom_plus_dual %>%
    select(vessel_official_number, compliant_, !!sym_time_period) %>%
    dplyr::group_by(vessel_official_number, !!sym_time_period) %>%
    summarise(cnt_compl_no = sum(tolower(compliant_) == "no")) %>%
    filter(cnt_compl_no > 0) %>%
    as.data.frame() %>%
    return()
}
non_compliant_vessels_per_year <-
  get_non_compliant_vessels_per_time_period("year")

data_overview(non_compliant_vessels_per_year)
# vessel_official_number 3741
# no
# vessel_official_number 2342


str(non_compliant_vessels_per_year)
# 'data.frame':	3322 obs. of  3 variables:

# 'data.frame':	167607 obs. of  3 variables:

# no
# 'data.frame':	42162 obs. of  3 variables:


non_compliant_vessels_per_month <-
  get_non_compliant_vessels_per_time_period("year_month")

glimpse(non_compliant_vessels_per_month)

non_compliant_vessels_per_month %>%
  select(vessel_official_number) %>% unique() %>% dim()
# [1] 2342    1

## could do just that, if it is non-compliant in a year, it is non-compl. in each month?
# compl_clean_sa_vs_gom_plus_dual %>%
#   filter(tolower(compliant_) == "no") %>%
#   select(vessel_official_number) %>%
#   unique() %>% dim()

non_compliant_vessel_ids <-
  non_compliant_vessels_per_year %>%
  select(vessel_official_number) %>%
  unique()

str(non_compliant_vessel_ids)

## all non compliant ----
compl_clean_sa_vs_gom_plus_dual_non_compl <-
  compl_clean_sa_vs_gom_plus_dual %>%
  filter(vessel_official_number %in% non_compliant_vessel_ids$vessel_official_number)
  #   select(vessel_official_number) %>% unique() %>% dplyr::glimpse()
  # Rows: 2,342
  
  
## ---- count reports ----
compl_clean_sa_vs_gom_plus_dual_non_compl_rep_cnt <-
  compl_clean_sa_vs_gom_plus_dual_non_compl %>%
  dplyr::mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
# %>% select(report_cnts) %>% unique() %>%
#   dplyr::glimpse()
# $ report_cnts <int> 0, 1, 6, 14, 2, 3, 7, 21, 5, 18, 17, 8, 16, 9, 10, 4, 51, 22, 1…

str(compl_clean_sa_vs_gom_plus_dual_non_compl_rep_cnt)
# [1] 107805     25
# 42162
compl_clean_sa_vs_gom_plus_dual %>% select(captainreports__,
                                           negativereports__,
                                           gom_permitteddeclarations__) %>% unique() %>% head()

## ---- gom + dual ----
names(compl_clean_sa_vs_gom_plus_dual_non_compl_rep_cnt)
short_non_compl <-
  compl_clean_sa_vs_gom_plus_dual_non_compl_rep_cnt %>%
  select(
    vessel_official_number,
    permit,
    compliant_,
    week_start,
    year_month,
    year_quarter,
    year,
    report_cnts
  )

gom_non_compl <-
  short_non_compl %>%
  filter(permit == "gom")

dim(gom_non_compl)
# 26193

gom_non_compl %>%
  data_overview()
# vessel_official_number 493
# compliant_               2

# summarise_each(funs(n_distinct(.)))

gom_sum_rep_by_month <-
  gom_non_compl %>%
  # select(vessel_official_number, year_month) %>%
  dplyr::group_by(vessel_official_number, year_month) %>%
  summarise(sum_report_cnts = sum(report_cnts),
            across(compliant_, ~list(sort(unique(.x))))) %>%
  as.data.frame()
# %>%
#   head()

head(gom_sum_rep_by_month)
  # is_compl = list(sort(unique(compliant_)))
  # ) %>%
  # as.data.frame() %>%
  # dplyr::mutate(non_compl = length(is_compl)
  #        dplyr::mutate(non_compl = length(is_compl)
  #               # dplyr::mutate(non_compl = ifelse(length(is_compl) > 1 | tolower(is_compl) == "no",
  #               #                           "non_compl", "yes")
data_overview(gom_sum_rep_by_month)
# 6196            
# vessel_official_number 493
# year_month              14
# sum_report_cnts         96
# is_compl                 3
# ---
# 1000042                    2?
# Groups:   vessel_official_number [493]
# ---
gom_sum_rep_by_month %>%
  filter(vessel_official_number == "1000042") %>%
  head()
  # dim()
# 14 4

compl_clean_sa_vs_gom_plus_dual %>%
  filter(vessel_official_number == "1000042") %>%
  dplyr::glimpse()
#   dim()

gom_sum_rep_by_month_c <-
  gom_sum_rep_by_month %>%
  dplyr::mutate(is_compl = ifelse(
    compliant_ == "YES",
    "compl", "non_compl"
    )
  )

# vessel_official_number year_month sum_report_cnts compliant_  is_compl
# 1                      Jan 2022               0        YES     compl
# 1                      Feb 2022               0        YES     compl
# 1                      Mar 2022              25        YES     compl
# 1                      Apr 2022              39    NO, YES non_compl
gom_non_compl_total_report_cnts_by_month <-
  gom_sum_rep_by_month_c %>%
  filter(is_compl == "non_compl") %>%
  dplyr::group_by(year_month) %>%
  summarise(total_report_cnts = sum(sum_report_cnts)) %>%
  as.data.frame()
# %>%
#   head()
# year_month total_report_cnts
# <yearmon>              <int>
# Jan 2022                 331
# Feb 2022                 795
# Mar 2022                 990
# Apr 2022                1119

gom_total_report_cnts_by_month <-
  gom_sum_rep_by_month_c %>%
  dplyr::group_by(year_month, is_compl) %>%
  summarise(total_report_cnts = sum(sum_report_cnts)) %>%
  as.data.frame()

head(gom_total_report_cnts_by_month)
head(gom_sum_rep_by_month_c)

count_vessel_ids_per_month_compl <-
  gom_sum_rep_by_month_c %>%
  select(year_month, is_compl, vessel_official_number) %>%
  dplyr::group_by(year_month, is_compl) %>%
  summarise(num_vessel = n()
    ) %>%
  as.data.frame()

# 358+47 = 405

gom_sum_rep_by_month_c %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>% dim()
  # 405

head(count_vessel_ids_per_month_compl)
head(gom_sum_rep_by_month_c)

gom_vsl_report_cnts <-
  inner_join(count_vessel_ids_per_month_compl,
           gom_total_report_cnts_by_month)
# Joining with `by = join_by(year_month, is_compl)`

head(gom_vsl_report_cnts)

## percent ----

gom_vsl_report_cnts_total <-
  gom_vsl_report_cnts %>%
  dplyr::group_by(year_month) %>%
  summarise(total_vsl = sum(num_vessel),
            total_rep = sum(total_report_cnts)) %>%
  as.data.frame()

gom_all_cnts <-
  inner_join(gom_vsl_report_cnts,
           gom_vsl_report_cnts_total)
# Joining with `by = join_by(year_month)`

gom_all_cnts_to_plot <-
  gom_all_cnts %>%
  dplyr::group_by(year_month, is_compl) %>%
  summarise(percent_reports = total_report_cnts * 100 / total_rep,
            percent_vsl = num_vessel * 100 / total_vsl
            ) %>% 
  filter(is_compl == "non_compl") %>%
  select(-is_compl)

head(gom_all_cnts_to_plot)
## plot gom ----
## plot non compl
gom_vsl_report_cnts %>%
  filter(is_compl == "non_compl") %>%
  ggplot(aes(
    x = year_month
    )) +
  geom_line(aes(y = num_vessel, color = "num_vessel")) +
  geom_line(aes(y = total_report_cnts, color = "total_report_cnts")) +
  labs(title = "Monthly report counts for non_compliant vessels",
       x ="month", y = "report counts")

gom_vsl_report_cnts %>%
  filter(is_compl == "compl") %>%
  ggplot(aes(
    x = year_month
  )) +
  geom_line(aes(y = num_vessel, color = "num_vessel")) +
  geom_line(aes(y = total_report_cnts, color = "total_report_cnts")) +
  labs(title = "Monthly report counts for compliant vessels",
       x ="month", y = "report counts")

## --- 
gom_all_cnts_to_plot %>%
  ggplot(aes(
    x = year_month
  )) +
  geom_line(aes(y = percent_reports , color = "percent of total reports")) +
  geom_line(aes(y = percent_vsl, color = "percent of reporting vessels")) +
  labs(title = "Monthly report counts for non-compliant vessels, percent of total",
       x ="month", y = "percentage")

gom_all_cnts_to_plot %>%
  ggplot(aes(
    x = year_month
  )) +
  geom_point(aes(y = percent_reports, color = "percent of total reports")) +
  geom_point(aes(y = percent_vsl, color = "percent of reporting vessels")) +
  geom_line(aes(y = percent_reports)) +
  geom_line(aes(y = percent_vsl), color = "red") +
  labs(title = "Monthly report counts for non-compliant vessels, percent of total",
       x ="month", y = "percentage") +
  geom_text(aes(y = percent_reports,
                label =
                  paste0(round(percent_reports, 2), "%")
                ),
  color = "blue",
  position = position_dodge(width = 0.9),
  vjust = -0.25,
  size = 3)


## ---- SA ----
sa_non_compl <-
  short_non_compl %>%
  filter(permit == "sa_only")

data_overview(sa_non_compl)
# vessel_official_number 1849

# better, because there could be more than 1 report per week, hence multiple join
num_of_weeks_per_vessel <-
  sa_non_compl %>%
  select(vessel_official_number, week_start) %>%
  dplyr::group_by(vessel_official_number) %>%
  summarise(num_of_weeks = n())
# %>%
#   dplyr::ungroup() %>% str()

# A tibble: 1,849 × 2

num_of_weeks_per_vessel2 <-
  sa_non_compl %>%
  select(vessel_official_number, week_start) %>%
  dplyr::add_count(vessel_official_number, name = "num_of_weeks")
# %>%
# filter(vessel_official_number == "1024180")
# %>%
#   dplyr::glimpse()

## check ----
sa_non_compl %>%
  filter(vessel_official_number == "1024180")

num_of_weeks_per_vessel %>%
  filter(vessel_official_number == "1000164")

num_of_weeks_per_vessel2 %>%
  filter(vessel_official_number == "1000164")

## expected vs. given
sa_non_comp_week_num <-
  left_join(sa_non_compl,
            num_of_weeks_per_vessel) %>%
  dplyr::arrange(vessel_official_number)
# %>%
# data_overview()
# dplyr::glimpse()
# Joining with `by = join_by(vessel_official_number)`
# Rows: 39,863
# Columns: 8

sa_non_comp_week_num %>%
  select(vessel_official_number, report_cnts, num_of_weeks) %>% head()

sa_non_comp_week_num %>%
  dplyr::group_by(vessel_official_number, year_month) %>%
  summarise(total_reports = sum(report_cnts)) %>%
  as.data.frame() %>%
  filter(vessel_official_number == "1024180")

num_of_weeks_per_vessel %>%
  filter(vessel_official_number == "1024180")


select(vessel_official_number, report_cnts, num_of_weeks) %>% dplyr::glimpse()
