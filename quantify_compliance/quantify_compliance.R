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

compl_clean_sa_vs_gom_plus_dual$year_month_w_start <-
  floor_date(compl_clean_sa_vs_gom_plus_dual$week_start  # Create year-month column
             )
             
compl_clean_sa_vs_gom_plus_dual %<>%
  mutate(year_month_w_end = floor_date(week_end)
         )

str(compl_clean_sa_vs_gom_plus_dual)             
             
             by_4_weeks <-
               compl_clean_sa_vs_gom_plus_dual %>%
               group_by(permit,
                        compliant_,
                        week) %>%
               summarise(n = n())
             
             str(by_week)
             # gropd_df [240 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)
             
             # with both
             # gropd_df [360 × 4] (S3: grouped_df/tbl_df/tbl/data.frame)
             
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
             