# Find vessels that have more than one vessel ID associated with them over time

# Get common functions
source("~/R_code_github/useful_functions_module.r")

## ---- set up ----
my_paths <- set_work_dir()

## ----- get csv data into variables -----
# temp_var <- get_compl_and_corresp_data(my_paths)
# compl_clean <- temp_var[[1]]
# corresp_clean <- temp_var[[2]]
## ---- get safis data ----
# safis_clean
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\all_vessels_safis.csv"
csv_names_list = list("all_vessels_safis.csv")
# read all csv files
csv_contents <- load_csv_names(my_paths, csv_names_list)
csvs_clean <- lapply(csv_contents, clean_headers)
dim(csvs_clean[[1]])

date_fields <- c("entrydate", "updatedate", "de", "dc")
date_format = "%m/%d/%Y"
safis_clean <- 
  change_fields_arr_to_dates(csvs_clean[[1]], date_fields, date_format)
  
data_overview(safis_clean)
# vesselid         130666


# df1 %>%
#   group_by_(.dots = names(df1)[3:6]) %>%
#   filter(n_distinct(c1) > 1)
compl_clean %>%
  select(vesselofficialnumber, name) %>%
  unique() %>%
  # group_by(name) %>%
  add_count(name, vesselofficialnumber, name = "id_freq") %>% 
  filter(id_freq > 1) %>% str()
    # filter(n_distinct(vesselofficialnumber) > 1)
  # 0


