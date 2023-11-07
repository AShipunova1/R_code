# usage: source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

library(zoo)
library(gridExtra)
library(cowplot)

# ---- set up ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

# ---- looking at the proportion of reports submitted vs flag ----
# dplyr::glimpse(compl_clean)
# Rows: 167,607
# Columns: 21

## ---- Separate SA and GOM permits ----
### ---- get list of all permitgroups ----
# https://www.fisheries.noaa.gov/southeast/recreational-fishing/frequently-asked-questions-southeast-hire-integrated-electronic#general-program-requirements

# GOM: RCG, HRCG, CHG, HCHG
# SA:  CDW, CHS, SC

### ---- separate into 3 groups ----
separate_permits_into_3_groups <- function(compl_clean) {
  compl_clean %>%
  dplyr::mutate(permit =
           dplyr::case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             !grepl("CDW|CHS|SC", permitgroup) ~ "gom_only",
             .default = "dual"
           )) %>%
    return()
}
compl_clean_sa_vs_gom <- separate_permits_into_3_groups(compl_clean)
    
# names(compl_clean_sa_vs_gom)
# dim(compl_clean_sa_vs_gom)
# [1] 167607     22

### ---- 2 groups: sa_only vs. gulf + dual ----
compl_clean_sa_vs_gom_plus_dual_0 <-
  compl_clean %>%
  dplyr::mutate(permit =
           dplyr::case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "gom"
           ))

## ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_plus_dual <-
  compl_clean_sa_vs_gom_plus_dual_0 %>%
  # add month
  dplyr::mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  dplyr::mutate(year_quarter = as.yearqtr(week_start))

## ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_plus_dual$captainreports__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$captainreports__)
compl_clean_sa_vs_gom_plus_dual$negativereports__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$negativereports__)
compl_clean_sa_vs_gom_plus_dual$gom_permitteddeclarations__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$gom_permitteddeclarations__)

## ---- aux functions ----
my_title <- function(time_period) {
  dplyr::case_when(
    time_period == "year" ~ "Annual",
    time_period == "year_month" ~ "Monthly",
    time_period == "year_quarter" ~ "Quarterly",
    time_period == "week_start" ~ "Weekly"
  ) %>%
    return()
}

# rename column names for x labels
my_x_lab <- function(time_period) {
  dplyr::case_when(
    time_period == "year" ~ "year",
    time_period == "year_month" ~ "month",
    time_period == "year_quarter" ~ "quarter",
    time_period == "week_start" ~ "week"
  ) %>%
    return()
}
# cat(names(compl_clean_sa_vs_gom_plus_dual_q_t))

time_period_fields <- c("year", "year_quarter", "year_month", "year")

get_time_period_col_name <- function(my_df) {
  time_period_field <- intersect(colnames(my_df), time_period_fields)
  return(time_period_field)
}

