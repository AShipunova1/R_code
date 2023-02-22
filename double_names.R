# Find vessels that have more than one vessel ID associated with them over time

# Get common functions
source("~/R_code_github/useful_functions_module.r")

## ---- set up ----
my_paths <- set_work_dir()

## ---- get safis data ----
csv_names_list = list("all_vessels_safis.csv")

# read csv file with EOF within quoted strings
read_csv_w_eofs <- function(my_paths, csv_names_list) {
    my_inputs <- my_paths$inputs
    # add input directory path in front of each file name.
    myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, x))
    
    # read all csv files
    contents <- sapply(myfiles, fread, header = TRUE)
    # convert the first one into a data frame
    # TODO change this function to deal with multiple files
    contents[, 1] %>% 
      as.data.frame() %>% 
      return()
}

csv_contents <- read_csv_w_eofs(my_paths, csv_names_list)
csvs_clean <- clean_headers(csv_contents)
# data_overview(csvs_clean)

## ---- convert dates ----
date_fields <- c("entrydate", "updatedate", "de", "dc")
date_format = "%m/%d/%Y"
safis_clean <- 
  change_fields_arr_to_dates(csvs_clean, date_fields, date_format)
  
# data_overview(safis_clean)

double_names_pairs <- 
  safis_clean %>%
    filter(coastguard != statereg) %>% 
    select(coastguard, statereg) %>% 
    unique()
# > dim(double_names_pairs)
# [1] 141670      2
# > dim(unique(double_names_pairs))
# [1] 138507      2

# %>%
  # unique() %>% dim()
# 16 ==
# 130649 !=

## ---- find FHIER correspondence and compliance data using both ----
## ----- get csv data into variables -----
temp_var <- get_compl_and_corresp_data(my_paths)
compl_clean <- temp_var[[1]]
corresp_clean <- temp_var[[2]]

