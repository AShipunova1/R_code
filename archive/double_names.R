# Find vessels that have more than one vessel ID associated with them over time
# extensive list of pairs from safis/safis management system
# find pairs which are different in compliance vs. correspondence
# download compliance for each year from FHIER/reports/FHIER COMPLIANCE REPORT
# download correspondence from FHIER/Correspondence

# Get common functions
source("~/R_code_github/useful_functions_module.r")
library(data.table)

## ---- set up ----
my_paths <- set_work_dir()

## ---- get safis data ----
add_csv_path = "other"
csv_names_list = list("all_vessels_safis.csv")

# read csv file with EOF within quoted strings
read_csv_w_eofs <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, add_csv_path, x))
  
  # read csv files
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
  dplyr::filter(coastguard != statereg) %>%
  dplyr::select(coastguard, statereg) %>%
  unique()
# dim(double_names_pairs)
# [1] 138507      2

## ---- find FHIER correspondence and compliance data using both ----
## ----- get compliance and correspondence csv data into variables -----
csv_names_list_21_23 = c("Correspondence__2_24_23.csv",
                         "FHIER_Compliance_22__02_24_23.csv",
                         "FHIER_Compliance_23__02_24_23.csv")
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_21_23)
compl_clean <- temp_var[[1]]
corresp_clean <- temp_var[[2]]

used_doube_pairs <- double_names_pairs %>%
  dplyr::filter(coastguard %in% compl_clean$vesselofficialnumber &
           statereg != "-")
# 97

used_doube_pairs <- double_names_pairs %>%
  dplyr::filter(statereg %in% compl_clean$vesselofficialnumber &
           coastguard != "-") %>% add_row(used_doube_pairs)
# 103

used_doube_pairs <- double_names_pairs %>%
  dplyr::filter(coastguard %in% corresp_clean$vesselofficialnumber &
           statereg != "-") %>% add_row(used_doube_pairs)
# 185
used_doube_pairs <- double_names_pairs %>%
  dplyr::filter(statereg %in% corresp_clean$vesselofficialnumber &
           coastguard != "-") %>% add_row(used_doube_pairs)
# 205

used_double_pairs_u <- unique(used_doube_pairs)
# 102

# write.csv(used_double_pairs_u, "used_double_pairs_u.csv")

# ---- check double pairs in compliance vs. correspondence
# Defining an empty dataframe
df_out <- tibble(pair = character(),
                 correspondence = character(),
                 compliance = character())

# head(used_double_pairs_u)
# loop over pairs
for (i in 1:nrow(used_double_pairs_u)) {
  # browser()
  # combine each pair in a string for search
  pair <- paste(used_double_pairs_u[i, ][1],
                used_double_pairs_u[i, ][2], sep = "|")
  
  # look up in correspondence
  in_corr <- corresp_clean %>%
    dplyr::filter(grepl(pair, vesselofficialnumber)) %>%
    dplyr::select(vesselofficialnumber) %>%
    # dplyr::select(vesselofficialnumber, contact_freq) %>%
    unique()
  
  # look up in compliance
  in_compl <- compl_clean %>%
    dplyr::filter(grepl(pair, vesselofficialnumber)) %>%
    dplyr::select(vesselofficialnumber) %>%
    unique()
  
  # some combinations are too general
  if (nrow(in_corr) > 2 | nrow(in_compl) > 2) {
    print(pair)
  }
  else if (!identical(in_corr, in_compl)) {
    # flatten found to a vector
    correspondence <- paste(in_corr)
    compliance <- paste(in_compl)
    # browser()
    output <- c(pair, correspondence, compliance)
    # Using rbind() to append the output of one iteration to the dataframe
    df_out <- rbind(df_out, output)
  }
}

# naming the columns
names(df_out) <- c("pair", "correspondence", "compliance")

# dplyr::glimpse(df_out)

# write.csv(df_out, file = "double_ids_check.csv", row.names = F)

