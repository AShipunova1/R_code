# usage: source(r"(~\R_code_github\quantify_compliance\quantify_compliance_start.R)")

library(zoo)
library(gridExtra)
library(cowplot)

## ---- set up ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

## ---- looking at the proportion of reports submitted vs flag ----
# glimpse(compl_clean)
# Rows: 167,607
# Columns: 21

## ---- Separate SA and GOM permits ----
## ---- get list of all permitgroups ----
# https://www.fisheries.noaa.gov/southeast/recreational-fishing/frequently-asked-questions-southeast-hire-integrated-electronic#general-program-requirements

# GOM: RCG, HRCG, CHG, HCHG
# SA:  CDW, CHS, SC

## ---- separate into 3 groups ----
compl_clean_sa_vs_gom <-
  compl_clean %>%
  mutate(permit =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             !grepl("CDW|CHS|SC", permitgroup) ~ "gom_only",
             .default = "both"
           ))

# names(compl_clean_sa_vs_gom)
# dim(compl_clean_sa_vs_gom)
# [1] 167607     22

## ---- gulf + dual ----
compl_clean_sa_vs_gom_plus_dual_0 <-
  compl_clean %>%
  mutate(permit =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "sa_only",
             .default = "gom"
           ))

## ---- add columns for month and quarter ----
compl_clean_sa_vs_gom_plus_dual <-
  compl_clean_sa_vs_gom_plus_dual_0 %>%
  # add month
  mutate(year_month = as.yearmon(week_start)) %>%
  # add quarter
  mutate(year_quarter = as.yearqtr(week_start))

## ---- convert report numbers to numeric ----
compl_clean_sa_vs_gom_plus_dual$captainreports__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$captainreports__)
compl_clean_sa_vs_gom_plus_dual$negativereports__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$negativereports__)
compl_clean_sa_vs_gom_plus_dual$gom_permitteddeclarations__ <-
  as.integer(compl_clean_sa_vs_gom_plus_dual$gom_permitteddeclarations__)

