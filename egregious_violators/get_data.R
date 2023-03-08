# get data for egregious violators
csv_names_list_22_23 = c("Correspondence__2_24_23.csv",
                         "FHIER_Compliance_22__02_24_23.csv",
                         "FHIER_Compliance_23__02_24_23.csv")

## ---- get csv data into variables ----
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)
compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]
