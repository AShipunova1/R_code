# get data for egregious violators
# Download from FHIER first
csv_names_list_22_23 = c("Correspondence__08_01_2023.csv",
                         r"(FHIER_Compliance_2022__08_01_2023.csv)",
                         r"(FHIER_Compliance_2023__08_01_2023.csv)")

data_file_date <- today()
  # lubridate::mdy("06_22_2023") 

## ---- get csv data into variables ----
my_paths$inputs <- file.path(my_paths$inputs, "from_Fhier")
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_inputs"

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\from_Fhier\Correspondence\Correspondence_22_23__06_22_2023.csv"
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]
