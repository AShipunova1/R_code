source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# Download Maintenance / SC Vessels Reporting via VESL
# from FHIER
# https://grunt.sefsc.noaa.gov/apex/f?p=162...

csv_names_list = list(r"(sc_mismatches\2024_05\fhier_report_05_06_2024.csv)")

xsl_names_list = list(r"(sc_mismatches\2024_04\scdnrFedVessels_04302024.xlsx)")

SC_vessels_FHIERData_0 <- load_csv_names(my_paths$inputs, csv_names_list)[[1]]

SC_vessels_FHIERData <- clean_headers(SC_vessels_FHIERData_0)

glimpse(SC_vessels_FHIERData)
str(SC_vessels_FHIERData)

# get enabled only
SC_vessels_FHIERData_enabled <-
  SC_vessels_FHIERData %>%
  filter(tolower(enabled) == "yes")
dim(SC_vessels_FHIERData_enabled)
# 188
# 214

#create new dataframe with just enabled vessel official # for analysis
FHIER_vessel_officialnumber <-
  data.frame(Official_number = tolower(SC_vessels_FHIERData_enabled$vessel_official_number))

dim(FHIER_vessel_officialnumber)
# [1] 189   1
# 188
# 214

#---
SC_permittedVessels  <- load_xls_names(my_paths, xsl_names_list, 1)

dim(SC_permittedVessels)
# 213
# 188
# 200
# 219

glimpse(SC_permittedVessels)

#---
names(SC_permittedVessels)[1]
# vessel_reg_uscg_
#create new dataframe with just official # codes for analysis
SC_vessel_officalnumber <-
  data.frame(Official_number = tolower(SC_permittedVessels$vessel_reg_uscg_))

#check for mistmatching fields using dplyr packages anti_join function
mismatched_officialnumbers_FHIERvsSC <-
  anti_join(FHIER_vessel_officialnumber,
            SC_vessel_officalnumber,
            by = "Official_number") #to ID rows that exist in FHIER but not in SC file

# NOTE - now do in reverse to ensure no vessels are in SC list that are not in FHIER list
mismatched_officialnumbers_SCvsFHIER <-
  anti_join(SC_vessel_officalnumber,
            FHIER_vessel_officialnumber,
            by = "Official_number") #to ID rows that exist in SC but not in FHIER

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
# 254
# 256

# length(FHIER_vessel_officialnumber$Official_number)
# 209 enabled
# 187
# 189

length(not_it_fhier_sc_report)
# 4
# 1
# 0
# 11
# 14 "2024-04-06"

#create output files - use these to update FHIER maintenance list ----
## make output file names ----
project_dir_name <- "mismatched fields SC"
output_file_names <- c("remove_from_FHIER",
                       "enable_in_FHIER",
                       "add_to_FHIER")

output_file_path_list <-
  output_file_names |>
  purrr::map(~ file.path(
    my_paths$outputs,
    project_dir_name,
    str_glue("{.x}_{today()}.csv")
  ))

output_file_path_list <-
  setNames(output_file_path_list, output_file_names)

## write files ----
write_csv(
  mismatched_officialnumbers_FHIERvsSC,
  output_file_path_list$remove_from_FHIER
) #remove from FHIER, bc no longer permitted

write_csv(
  mismatched_officialnumbers_SCvsFHIER,
  output_file_path_list$enable_in_FHIER
) #enable in FHIER, as newly permitted

write_csv(
  as.data.frame(not_it_fhier_sc_report),
  output_file_path_list$add_to_FHIER
) #add to FHIER, as newly permitted


