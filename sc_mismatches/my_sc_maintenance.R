source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

csv_names_list = list("sc_mismatches/sc_report_Aug_2023.csv")
# csv_names_list = list("sc_mismatches/sc_report_08_2023_after.csv")

xsl_names_list = list("sc_mismatches/scdnrFedVessels_08302023.xlsx")

SC_vessels_FHIERData_0 <- load_csv_names(my_paths, csv_names_list)[[1]]

SC_vessels_FHIERData <- clean_headers(SC_vessels_FHIERData_0)

glimpse(SC_vessels_FHIERData)
str(SC_vessels_FHIERData)

# get enabled only
SC_vessels_FHIERData_enabled <- 
  SC_vessels_FHIERData %>%
  filter(tolower(enabled) == "yes") 
dim(SC_vessels_FHIERData_enabled)
# 199 8

# SC_vessels_FHIERData_enabled %>% names()
#   filter(vessel_official_number   == "1225219")

#create new dataframe with just enabled vessel official # for analysis
FHIER_vessel_officialnumber <-
  data.frame(Official_number = tolower(SC_vessels_FHIERData_enabled$vessel_official_number)) 

dim(FHIER_vessel_officialnumber)
# 199
# 88

#---
SC_permittedVessels  <- load_xls_names(my_paths, xsl_names_list, 1)

glimpse(SC_permittedVessels)
# 213

# SC_permittedVessels1 <- read_excel(paste(my_paths$inputs, xsl_names_list[[1]], sep = "/"), 1)

#---
#create new dataframe with just official # codes for analysis
SC_vessel_officalnumber <- data.frame(Official_number = tolower(SC_permittedVessels[[1]])) 

#check for mistmatching fields using dplyr packages anti_join function
mismatched_officialnumbers_FHIERvsSC <-
  anti_join(FHIER_vessel_officialnumber,
            SC_vessel_officalnumber,
            by = "Official_number") #to ID rows that exist in FHIER but not in SC file

#NOTE - now do in reverse to ensure no vessels are in SC list that are not in FHIER list
mismatched_officialnumbers_SCvsFHIER <-
  anti_join(SC_vessel_officalnumber, FHIER_vessel_officialnumber, by = "Official_number") #to ID rows that exist in SC but not in FHIER

#---
# identical?
fh_to_sc_diff <- setdiff(FHIER_vessel_officialnumber$Official_number, SC_vessel_officalnumber$Official_number)

identical(sort(mismatched_officialnumbers_FHIERvsSC$Official_number),
          sort(fh_to_sc_diff))
# T

sc_to_fh_diff <-
  setdiff(
    SC_vessel_officalnumber$Official_number,
    FHIER_vessel_officialnumber$Official_number
  )
identical(sort(mismatched_officialnumbers_SCvsFHIER$Official_number),
          sort(sc_to_fh_diff))
# T

# check if not in sc report at all ----
not_it_fhier_sc_report <-
  setdiff(
    tolower(SC_vessel_officalnumber$Official_number),
    tolower(SC_vessels_FHIERData$vessel_official_number)
  )

# length(SC_vessels_FHIERData$vessel_official_number)
# 243
# length(FHIER_vessel_officialnumber$Official_number)
# 209 enabled
length(not_it_fhier_sc_report)
# 4

# write to files ----
project_dir_name <- "mismatched fields SC"

output_file_path <- 
  file.path(
    my_paths$outputs,
    project_dir_name,
    str_glue(
    "{output_file_name}_{today()}.csv"
  ))

#create output file - use these to update FHIER maintenance list
output_file_name = "###"

write_csv(
  mismatched_officialnumbers_FHIERvsSC,
) #remove from FHIER, bc no longer permitted

write_csv(
  mismatched_officialnumbers_SCvsFHIER,
  file.path(
    my_paths$outputs,
    project_dir_name,
    paste0("enable_in_FHIER_",
    today(),
    ".csv")
  )
) #enable in FHIER, as newly permitted

write_csv(
  not_it_fhier_sc_report,
  file.path(
    my_paths$outputs,
    project_dir_name,
    "add_to_FHIER_",
    today(),
    ".csv"
  )
) #add to FHIER, as newly permitted


