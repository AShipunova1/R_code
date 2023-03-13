filenames = c("FHIER_Compliance_22__02_24_23.csv",
                         "FHIER_Compliance_23__02_24_23.csv")

## ---- get csv data into variables ----
csv_names_list <- prepare_csv_names(filenames)
# read all csv files
csv_contents <- load_csv_names(my_paths, csv_names_list)
# browser()
# unify headers, trim vesselofficialnumber, just in case
csvs_clean1 <- clean_all_csvs(csv_contents)
str(csvs_clean1)
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)
compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]
