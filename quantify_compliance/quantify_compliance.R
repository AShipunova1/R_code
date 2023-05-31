# quantify_compliance

library(zoo)
library(gridExtra)
library(cowplot)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

### ---- separate SA and GOM permits ----

separate_permits_into_3_groups <- function(compl_clean) {
  compl_clean %>%
  mutate(permit_sa_gom =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             !grepl("CDW|CHS|SC", permitgroup) ~ "gom_only",
             .default = "both"
           )) %>%
    return()
}

compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)

# View(compl_clean_sa_vs_gom)

## ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_m <-
  compl_clean_sa_vs_gom %>%
  # add month
  mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  mutate(year_quarter = as.yearqtr(week_start))

## ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m %>%
  mutate(
    captainreports__ = as.integer(captainreports__),
    negativereports__ = as.integer(negativereports__),
    captainreports__ = as.integer(gom_permitteddeclarations__)
  )


# View(err_desc_clean_headers_csv_content)

compl_err_db_data_22_23 %<>% 
  mutate(comp_year = as.character(comp_year))

tic("full_join")
compl_clean_sa_vs_gom_m_int_v <-
  full_join(
    compl_err_db_data_22_23,
    compl_clean_sa_vs_gom_m_int,
    by = join_by(supplier_vessel_id == vessel_official_number,
                 comp_year == year,
                 comp_week == week_num
                 ),
    relationship = "many-to-many"
  )
# 
# names(compl_clean_sa_vs_gom_m_int)
toc()
dim(compl_clean_sa_vs_gom_m_int_v)
# [1] 208989     36

# "supplier_vessel_id"
# "coast_guard_nbr"      "state_reg_nbr"

compl_clean_sa_vs_gom_m_int_v %>%
  filter(!is.na(coast_guard_nbr) &
           !is.na(state_reg_nbr) &
           (!(coast_guard_nbr == state_reg_nbr))) %>% dim()
# 1618

# comp_error_type_cd,
# error_type_wo_desc

not_in_compl_join <-
  compl_clean_sa_vs_gom_m_int_v %>%
  filter(is.na(permit_sa_gom))

not_in_compl_join %>%  count(comp_year)
#   comp_year     n
# 2      2022    58
# 3      2023    24

# names(not_in_compl_join)
not_in_compl_join %>% 
  count(supplier_vessel_id, coast_guard_nbr, state_reg_nbr) %>% 
  unique()
#   supplier_vessel_id coast_guard_nbr state_reg_nbr  n
# 1            1000164         1000164      FL2310RW 21
# 2            1198330         1198330      FL9110GE 11
# 3            1299734         1299734      TX6287FJ 18
# 4            1327036         1327036      FL2940RH 11
# 5             991490          991490      FL7825PU 21

compl_clean_sa_vs_gom_m_int %>%
  filter(
    vessel_official_number %in% c("TX6287FJ", "FL7825PU", "FL9110GE", "FL2940RH", "FL2310RW")
  ) %>% dim()
# 218

## join those by state_reg_nbr ----
compl_clean_sa_vs_gom_m_int_j <-
  inner_join(
    compl_err_db_data_22_23,
    compl_clean_sa_vs_gom_m_int_v,
    by = join_by(
      state_reg_nbr == supplier_vessel_id,
      comp_year,
      comp_week,
      activity_dt,
      activity_time,
      coast_guard_nbr,
      comp_error_type_cd,
      error_type_wo_desc,
      for_hire_trip_type,
      is_overridable,
      is_past_grace_period,
      lateness,
      safis_vessel_id,
      srh_vessel_comp_id
    ),
    relationship = "many-to-many"
  ) 

dim(compl_clean_sa_vs_gom_m_int_j)
# [1] 23682    37

dim(compl_clean_sa_vs_gom)
# [1] 208893     22

dim(compl_clean_sa_vs_gom_m_int_v)
# [1] 208989     36

# test
compl_clean_sa_vs_gom_m_int_j %>%
  filter(is.na(permit_sa_gom)) %>% dim()
# 0 37

grep("x", names(compl_clean_sa_vs_gom_m_int_j), value = T) %>% 
  paste0(collapse = ", ")

compl_clean_sa_vs_gom_m_int_j %>%
  select(permit_groupexpiration, permitgroupexpiration) %>%
  unique() %>%
  # change_to_dates(permitgroupexpiration, "%m/%d/%Y"
  mutate(permit_groupexpiration = as.POSIXct(permit_groupexpiration,
                                             format = "%m/%d/%Y")) %>%
  # str()
  
# $ permit_groupexpiration: chr  "04/30/2024" "01/31/2024" "02/29/2024" "12/31/2023" ...
# $ permitgroupexpiration : POSIXct, format: "2024-04-30" "2024-01-31" ...

  filter(
    !(permit_groupexpiration == permitgroupexpiration)
  ) %>% dim()
# 0

identical(names(compl_clean_sa_vs_gom_m_int_j),
          names(compl_clean_sa_vs_gom_m_int_v))
# F

setdiff(names(compl_clean_sa_vs_gom_m_int_j),
          names(compl_clean_sa_vs_gom_m_int_v))
# [1] "state_reg_nbr.y"

grep("\\.y", names(compl_clean_sa_vs_gom_m_int_j), value = T) %>% 
  paste0(collapse = ", ")

compl_clean_sa_vs_gom_m_int_j %>%
  select(state_reg_nbr.y, state_reg_nbr) %>%
  unique() %>%
  filter(!(state_reg_nbr.y == state_reg_nbr)) %>% dim()
# 0

compl_clean_sa_vs_gom_m_int_j_n <-
  select(compl_clean_sa_vs_gom_m_int_j,
         -state_reg_nbr.y)
  
## combine v and j ----

compl_clean_sa_vs_gom_m_int_join <-
  rbind(compl_clean_sa_vs_gom_m_int_v,
        compl_clean_sa_vs_gom_m_int_j_n)

dim(compl_clean_sa_vs_gom_m_int_join)
# [1] 232671     36

# SA only ----
# at least 1 logbook or no fish report per week
# "No REPORT" err
# How many non-compliant in each week?
# For a given month:
# 100% - the total # of non-compl. vessels
# x%   - submitted 1 week, 2 weeks etc.
# proportion of weeks rep. are missing

# View(compl_clean_sa_vs_gom_m_int)

sa_compl_clean_sa_vs_gom_m_int <-
  compl_clean_sa_vs_gom_m_int_join %>% 
  filter(permit_sa_gom == "sa_only")

dim(sa_compl_clean_sa_vs_gom_m_int)
# 123,453
# [1] 145187 w j

View(sa_compl_clean_sa_vs_gom_m_int)

sa_compl_clean_sa_vs_gom_m_int %>% count(error_type_wo_desc)
#   error_type_wo_desc     n
# 1          NO REPORT 61294
# 2               <NA> 83893
# > 

sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(is.na(error_type_wo_desc)) %>% 
  count(compliant_)
#     compliant_     n
# 1         NO    53
# 2        YES 83840

sa_compl_clean_sa_vs_gom_m_int %>% 
   filter(is.na(error_type_wo_desc) &
           compliant_ == "NO" ) %>% 
   count(name, supplier_vessel_id)
#         name supplier_vessel_id  n
# 1   FIRED UP           FL7825PU 21
# 2  SEAS FIRE           FL9110GE 11
# 3 UNDER FIRE           FL2310RW 21

compl_clean_sa_vs_gom_m_int_join %>% 
  filter(state_reg_nbr %in% c("FL2310RW", "FL9110GE", "FL2310RW") ) %>%
  View()
# supplier_vessel_id by coast_guard, not state

sa_compl_clean_sa_vs_gom_m_int_non_c <-
  sa_compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO")

sa_compl_clean_sa_vs_gom_m_int_non_c %>% 
  filter(year_month == "Dec 2022") %>%
  count(supplier_vessel_id, state_reg_nbr, comp_week, overridden_) %>% 
  filter(n > 1)
#     supplier_vessel_id state_reg_nbr comp_week overridden_ n
#               1000164      FL2310RW        49         YES 1
#               1000241          <NA>        49          NO 1
#               1090694       1090694        49          NO 2

# sa_compl_clean_sa_vs_gom_m_int_non_c %>% 
#   filter(year_month == "Dec 2022") %>%
#   count(supplier_vessel_id, state_reg_nbr, comp_week) %>%
#   group_by(comp_week) 
   
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

non_compl_per_week_month <-
  compl_clean_sa_vs_gom_m_int %>%
  filter(compliant_ == "NO") %>%
  count(year_month, vessel_official_number, 
        name = "non_compl_week") %>%
  # how many non_compliant each month
  count(year_month, non_compl_week, name = "non_compl_in_month") 
glimpse(non_compl_per_week_month)

# non_compl_total_nc_per_month <-
#   compl_clean_sa_vs_gom_m_int %>% 
#   filter(compliant_ == "NO") %>% 
#   count(year_month, name = "total_nc_per_month")
#   # %>% glimpse()

compl_clean_sa_vs_gom_m_int %>% 
  filter(compliant_ == "NO") %>% 
  filter(year_month == "Dec 2022") %>%
  select(vessel_official_number) %>%
  unique() %>% 
  dim()
# 467 = 25+25+38+379

  
names(non_compl_per_week_month)

non_compl_per_week_month_wide <-
  non_compl_per_week_month %>%
    pivot_wider(names_from = non_compl_week,
                values_from = non_compl_in_month,
                values_fill = 0)

names(non_compl_per_week_month_wide)

non_compl_per_week_month_wide_w_total <-
  non_compl_per_week_month_wide %>% 
  mutate(total_nc_vsl_per_month = rowSums(.[2:6]))

names(non_compl_per_week_month_wide_w_total)

non_compl_per_week_month_w_total <-
  non_compl_per_week_month_wide_w_total %>% 
    pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_week",
                 values_to = "non_compl_in_month") %>% 
    mutate(percent_nc_weeks = 100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month
             )
View(non_compl_per_week_month_w_total)

gg_22_01 <- 
  non_compl_per_week_month_w_total %>% 
  filter(year_month == "Jan 2022") %>%
  ggplot(aes(non_compl_week, percent_nc_weeks)) +
  geom_col()

non_compl_per_week_month_w_total_short <-
  non_compl_per_week_month_w_total %>%
  select(year_month, non_compl_week, percent_nc_weeks)

gg_non_compl_per_week_month_w_total <-
  non_compl_per_week_month_w_total_short$year_month %>%
  unique() %>% 
  map(function(current_year_month) {
    non_compl_per_week_month_w_total_short %>%
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_week, percent_nc_weeks)) +
      geom_col(fill = "lightblue") +
      labs(title = current_year_month) %>%
      # TODO: add color, a month as a title, axes text
      return()
  })

gg_non_compl_per_week_month_w_total[[1]]

grid.arrange(grobs = gg_non_compl_per_week_month_w_total,
             # top = super_title,
             # left = my_legend,
             ncol = 4)
