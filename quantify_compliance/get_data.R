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

# View(compl_clean)

## ---- get compliance error definitions ----

err_desc_filenames = c("Compliance_Error_Types_03_29_2023.csv")

err_desc_csv_contents <-
  load_csv_names(my_paths, err_desc_filenames)

err_desc_clean_headers_csv_content <-
  clean_headers(err_desc_csv_contents[[1]])
err_desc <-
  change_to_dates(err_desc_clean_headers_csv_content,
                  "last_updated",
                  "%m/%d/%Y %I:%M:%S %p")
# 03/22/2023 04:56:11 PM
# "%m/%d/%Y %I:%M %p"
as.POSIXct("03/22/2023 04:56:11 PM", format = "%m/%d/%Y %I:%M:%S %p")

View(err_desc_clean_headers_csv_content)
