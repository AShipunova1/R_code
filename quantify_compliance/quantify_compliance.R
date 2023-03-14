## ---- set up ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/quantify_compliance/get_data.R")

## ---- looking at the proportion of reports submitted vs flag ----
glimpse(compl_clean)
# Rows: 167,607
# Columns: 21

## ---- Have only SA permits, exclude those with Gulf permits ----
compl_clean_sa_vs_gom <-
  compl_clean %>%
  mutate(sa_only = 
           case_when(!grepl("RCG|HRCG|CHG|HCHG", permitgroup) ~ "yes",
                     .default = "no"
         )
  )

names(compl_clean_sa_vs_gom)
compl_clean_sa_vs_gom %>%
  group_by(sa_only, captainreports__, negativereports__) %>%
  summarise(n = n()) %>%
  # summarise(complianceerrors = sum(as.integer(complianceerrors__))) %>%
  # mutate(freq = n / sum(n)) %>%
  # mutate(freq = (captainreports__ + negativereports__) / sa_only)
  # mutate(per =  100 *count/sum(count)) %>% 
  str()

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