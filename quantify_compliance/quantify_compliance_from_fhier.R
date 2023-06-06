# quantify_compliance

library(zoo)
library(gridExtra)
library(cowplot)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

# ---- separate SA and GOM permits ----
compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)

# View(compl_clean_sa_vs_gom)

# ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_m <-
  compl_clean_sa_vs_gom %>%
  # add month
  mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  mutate(year_quarter = as.yearqtr(week_start))

# ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m %>%
  mutate(
    captainreports__ = as.integer(captainreports__),
    negativereports__ = as.integer(negativereports__),
    gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
  )

dim(compl_clean_sa_vs_gom)
# [1] 208893     22

# SA only ----

# at least 1 logbook or no fish report per week
# "No REPORT" err
# How many non-compliant in each week?
# For a given month:
# 100% - the total # of non-compl. vessels
# x%   - submitted 1 week, 2 weeks etc.
# proportion of weeks rep. are missing

# View(compl_clean_sa_vs_gom_m_int)

## get SA only ----
sa_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(permit_sa_gom == "sa_only")

dim(sa_compl_clean_sa_vs_gom_m_int)
# 123,453

## SA only non compliant ----
sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_)
#     compliant_     n
# 1 NO         38259
# 2 YES        85194

sa_compl_clean_sa_vs_gom_m_int %>% 
  count(compliant_, overridden_)
#   compliant_ overridden_     n
# 1 NO         NO          33322
# TODO: check if the override comment it is somewhere else
# 2 NO         YES          4937 
# 3 YES        NO          85194

# compl_clean_sa_vs_gom_m_int %>% 
#   count(compliant_, overridden_)

sa_compl_clean_sa_vs_gom_m_int_non_c <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

### test one month ----
compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO") %>% 
  filter(year_month == "Dec 2022") %>%
  count(vessel_official_number, name = "id_n") %>%
  # how many non_compliant this month
  count(id_n, name = "non_compl_weeks_in_month")
  # count(week_num, compliant_)
#    id_n non_compl_weeks_in_month
# 1     1                       25
# 2     2                       25
# 3     3                       38
# 4     4                      379
# TODO vessels in "1" not anywhere else

sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  get_non_compl_week_counts_percent(sa_compl_clean_sa_vs_gom_m_int_non_c,
                                    "vessel_official_number")
View(sa_compl_clean_sa_vs_gom_m_int_non_c_perc)



## SA only plots ----
### one plot ----
# gg_22_01 <- 
  # sa_compl_clean_sa_vs_gom_m_int_non_c_perc %>% 
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_non_c_perc$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
      current_year_month
    )
  )

# one plot
# gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc[[12]]

super_title = "SA only from csvs: how many weeks vessels were non_compliant"

grid.arrange(grobs = gg_sa_compl_clean_sa_vs_gom_m_int_non_c_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

## SA non compliant and not overridden ----
sa_compl_clean_sa_vs_gom_m_int_nc_no <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO" & overridden_ == "NO")

sa_compl_clean_sa_vs_gom_m_int_nc_no_perc <-
  get_non_compl_week_counts_percent(sa_compl_clean_sa_vs_gom_m_int_nc_no,                                    "vessel_official_number")
# View(sa_compl_clean_sa_vs_gom_m_int_nc_no_perc)

gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc <-
  unique(sa_compl_clean_sa_vs_gom_m_int_nc_no_perc$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      sa_compl_clean_sa_vs_gom_m_int_nc_no_perc,
      current_year_month
    )
  )

# one plot
gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc[[12]]

super_title = "SA only from csvs: how many weeks vessels were non_compliant and not overridden"

grid.arrange(grobs = gg_sa_compl_clean_sa_vs_gom_m_int_nc_no_perc,
             top = super_title,
             # left = my_legend,
             ncol = 4)

## Same using only report counts without "compliant__" ----

# GOM + dual ----
gom_all_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(!(permit_sa_gom == "sa_only"))

dim(gom_all_compl_clean_sa_vs_gom_m_int)
# [1] 85440    24

gom_all_compl_clean_sa_vs_gom_m_int_even <-
  gom_all_compl_clean_sa_vs_gom_m_int %>%
  mutate(
    logb_n_decl = captainreports__ + gom_permitteddeclarations__,
    even_num_rep = dplyr::if_else(((logb_n_decl %% 2) == 0),
                                  "even", "odd")
  )
# View(gom_all_compl_clean_sa_vs_gom_m_int_even)

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  count(even_num_rep, compliant_)
#   even_num_rep compliant_     n
#   <chr>        <chr>      <int>
# 1 even         NO          3542 ?
# 2 even         YES        76908
# 3 odd          NO           373
# 4 odd          YES         4617 ?

## investigate  odd/YES ----
# gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
#   filter(compliant_ == "YES" & even_num_rep == "odd") %>% View()

gom_all_compl_clean_sa_vs_gom_m_int_even %>% 
  filter(compliant_ == "YES" & even_num_rep == "odd") %>% head(1) %>% glimpse()
# $ vessel_official_number      <chr> "TX3416RA"
# $ name                        <chr> "CONTROLLED CHAOS"
# $ permitgroup                 <chr> "(CHG)885,(RCG)836"
# $ year_month                  <yearmon> Dec 2022
# $ week_num                    <int> 52
# $ week                        <chr> "52: 12/26/2022 - 01/01/2023"
# $ week_start                  <date> 2022-12-26
# $ week_end                    <date> 2023-01-01


## by "compliant?"
gom_all_compl_clean_sa_vs_gom_m_int_non_comp <-
  gom_all_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

# gom_all_compl_clean_sa_vs_gom_m_int_non_comp_perc <-
#   get_non_compl_week_counts(gom_all_compl_clean_sa_vs_gom_m_int_non_comp)
# numbers are too low

# GOM + dual non compl ----

 # e.comp_error_type_cd = 'DECL_NO_TRIP'
 # AND TRUNC(SYSDATE) > TRUNC(tn.trip_start_date)
 # )
 # OR -- Past activity date
 # (
 # e.comp_error_type_cd = 'TRIP_NO_DECL'

 # AND TRUNC(SYSDATE) < (TRUNC(c.comp_week_end_dt) + 14) THEN -- Not past allotted correction time

# SA:
#    TRUNC(SYSDATE) >= (TRUNC(c.comp_week_end_dt) + 24) THEN -- Past allotted correction time

# select * from srh.srfh_for_hire_type@secapxdv_dblk.sfsc.noaa.gov
# 1	HEADBOAT
# 2	CHARTER
# 42	RECREATIONAL
# 81	COMMERCIAL
# 62	UNKNOWN

## get not SA only ----
gom_d_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int %>% 
  filter(!(permit_sa_gom == "sa_only"))

dim(gom_d_compl_clean_sa_vs_gom_m_int)
# [1] 85440    24

## Gom and dual non compliant ----
gom_d_compl_clean_sa_vs_gom_m_int_nc <-
  gom_d_compl_clean_sa_vs_gom_m_int %>% 
  filter(toupper(compliant_) == "NO")

dim(gom_d_compl_clean_sa_vs_gom_m_int_nc)
# [1] 3915   24

## Gom and dual non compliant and not overridden----
gom_d_compl_clean_sa_vs_gom_m_int_nc_no <-
  gom_d_compl_clean_sa_vs_gom_m_int %>% 
  filter(toupper(compliant_) == "NO" &
           toupper(overridden_) == "NO")

dim(gom_d_compl_clean_sa_vs_gom_m_int_nc_no)
# [1] 2935   24

gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  gom_d_compl_clean_sa_vs_gom_m_int_nc_no %>%
  filter(year == '2022')

# TODO 2023 separately for "both" permits

## get percentage of nc weeks ----
perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  get_non_compl_week_counts_percent(gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
                                    "vessel_official_number")

View(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22)

## GOM + dual 22 csv plots ----
### one plot
# gg_22_01_gom <- 
  # perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 %>%
  # filter(year_month == "Jan 2022") %>%
  # ggplot(aes(non_compl_weeks, percent_nc)) +
  # geom_col()

gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22 <-
  unique(perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22$year_month) |>
  map(
    \(current_year_month)
    perc_plots_by_month(
      perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
      current_year_month
    )
  )

# one plot
# gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22[[12]]

super_title = "GOM & dual from csvs: how many weeks vessels were non_compliant"
grid.arrange(grobs =
               gg_perc_gom_d_compl_clean_sa_vs_gom_m_int_nc_no_22,
             top = super_title,
             ncol = 4)
