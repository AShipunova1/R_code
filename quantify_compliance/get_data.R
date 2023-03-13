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
# browser()
compl_clean <- compliance_cleaning(csvs_clean1)

