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
# csvs_clean %>% 
#   for (i in seq_along(csvs_clean)){
#     csvs_clean[[i]]$vesselofficialnumber <-
#       trimws(csvs_clean[[i]]$vesselofficialnumber)
#   }

# data_overview(csvs_clean[[1]])
date_format = "%m/%d/%Y"

# csvs_clean[[1]] %>%
#   # change_to_dates(date_fields[1], date_format) %>%
#   # change_to_dates("updatedate", date_format) ->
#   change_to_dates("entrydate", date_format) %>%
#   change_to_dates("updatedate", date_format) %>%
#   change_to_dates("de", date_format) %>%
#   change_to_dates("dc", date_format) ->
#   safis_clean

# data_overview(safis_clean)
# gdf %>% mutate(across(v1:v2, ~ .x + n))


change_fields_arr_to_dates(csvs_clean[[1]], date_fields, date_format) %>%
  str()

fun1 <- function(x, date_format) {
  out <- as.POSIXct(x,
             format = date_format)
  # as.POSIXct(pull(csvs_clean[[1]][x]),
  #   format = date_format) %>% str()
  out
}

csvs_clean[[1]] %>%
  mutate(across(date_fields, fun1, date_format)) %>% str()
# as.POSIXct(pull(my_df[field_name]),
# format = date_format))
  

csvs_clean[[1]] %>%
  change_to_dates(date_fields, date_format) %>% str()

data_overview(safis_clean)
# vesselid         130666
data_overview(safis_clean)

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


