# Find vessels that have more than one vessel ID associated with them over time

# Get common functions
source("~/R_code_github/useful_functions_module.r")

## ---- set up ----
my_paths <- set_work_dir()

## ---- get safis data ----
csv_names_list = list("all_vessels_safis.csv")
# read csv file
csv_contents <- load_csv_names(my_paths, csv_names_list)
csvs_clean <- lapply(csv_contents, clean_headers)
dim(csvs_clean[[1]])

date_fields <- c("entrydate", "updatedate", "de", "dc")
date_format = "%m/%d/%Y"
safis_clean <- 
  change_fields_arr_to_dates(csvs_clean[[1]], date_fields, date_format)
  
data_overview(safis_clean)

names(safis_clean)

safis_clean %>%
  filter(coastguard != statereg) %>% 
  select(coastguard, statereg) %>%
  unique() %>% dim()
# 16 ==
# 130649 !=

