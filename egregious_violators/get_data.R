# get data for egregious violators
# Download from FHIER first
csv_names_list_22_23 = c("Correspondence_22_23__06_22_2023.csv",
                         r"(FHIER_Compliance_2022__06_22_2023.csv)",
                         r"(FHIER_Compliance_2023__06_22_2023.csv)")

## ---- get csv data into variables ----
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]
