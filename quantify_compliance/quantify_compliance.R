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
  mutate(permit =
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
  compl_clean_sa_vs_gom_m_int %>% 
  filter(permit == "sa_only")

str(sa_compl_clean_sa_vs_gom_m_int)
# 123,453

View(err_desc_clean_headers_csv_content)

compl_err_db_data %<>% 
  mutate(comp_year = as.character(comp_year))

tic("full_join")
compl_clean_sa_vs_gom_m_int_v <-
  full_join(
    compl_err_db_data,
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
# [1] 253757     36

# too big
# [1] 4539787      38
# aa %>% 
#   filter(is.na(vessel_official_number)) %>% dim()
# 0

aa %>%
  filter(is.na(COAST_GUARD_NBR)) %>%
  glimpse()
  # dim()
# [1] 2341422      38

# "supplier_vessel_id"
# "coast_guard_nbr"      "state_reg_nbr"

compl_clean_sa_vs_gom_m_int_v <-
  compl_clean_sa_vs_gom_m_int %>%
  filter((
    vessel_official_number == compl_err_db_data$supplier_vessel_id
  ) |
    (vessel_official_number == compl_err_db_data$coast_guard_nbr) |
    (vessel_official_number == compl_err_db_data$state_reg_nbr)
  )


  mutate(comp_year = compl_err_db_data$comp_year,
comp_week = compl_err_db_data$comp_week,
comp_error_type_cd = compl_err_db_data$comp_error_type_cd,
error_type_wo_desc = compl_err_db_data$error_type_wo_desc
)
