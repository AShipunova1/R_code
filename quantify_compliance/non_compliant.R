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
    group_by(vessel_official_number, !!sym_time_period) %>%
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
  #   select(vessel_official_number) %>% unique() %>% glimpse()
  # Rows: 2,342
  
  
## ---- count reports ----
compl_clean_sa_vs_gom_plus_dual_non_compl_rep_cnt <-
  compl_clean_sa_vs_gom_plus_dual_non_compl %>%
  mutate(
    report_cnts = ifelse(
      permit == "sa_only",
      captainreports__ + negativereports__,
      captainreports__ + gom_permitteddeclarations__
    )
  )
# %>% select(report_cnts) %>% unique() %>%
#   glimpse()
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

gom_non_compl %>%
  # select(vessel_official_number, year_month) %>%
  group_by(vessel_official_number, year_month) %>%
  summarise(sum_report_cnts = sum(report_cnts),
            is_compl = 
            ) %>% head()

# 1000042                    2?
# Groups:   vessel_official_number [493]

compl_clean_sa_vs_gom_plus_dual_non_compl_rep %>%
  filter(vessel_official_number == "1000042") %>%
  glimpse()
#   dim()
# 2299    6

compl_clean_sa_vs_gom_plus_dual %>%
  filter(vessel_official_number == "1000042") %>%
  glimpse()
#   dim()

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
  group_by(vessel_official_number) %>%
  summarise(num_of_weeks = n())
# %>%
#   ungroup() %>% str()

# A tibble: 1,849 × 2

num_of_weeks_per_vessel2 <-
  sa_non_compl %>%
  select(vessel_official_number, week_start) %>%
  add_count(vessel_official_number, name = "num_of_weeks")
# %>%
# filter(vessel_official_number == "1024180")
# %>%
#   glimpse()

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
  arrange(vessel_official_number)
# %>%
# data_overview()
# glimpse()
# Joining with `by = join_by(vessel_official_number)`
# Rows: 39,863
# Columns: 8

sa_non_comp_week_num %>%
  select(vessel_official_number, report_cnts, num_of_weeks) %>% head()

sa_non_comp_week_num %>%
  group_by(vessel_official_number, year_month) %>%
  summarise(total_reports = sum(report_cnts)) %>%
  as.data.frame() %>%
  filter(vessel_official_number == "1024180")

num_of_weeks_per_vessel %>%
  filter(vessel_official_number == "1024180")


select(vessel_official_number, report_cnts, num_of_weeks) %>% glimpse()
