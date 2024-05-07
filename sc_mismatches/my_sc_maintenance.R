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
# str(SC_vessels_FHIERData)

# get enabled only
SC_vessels_FHIERData_enabled <-
  SC_vessels_FHIERData %>%
  filter(tolower(enabled) == "yes")

dim(SC_vessels_FHIERData_enabled)
# 188
# 214

# create new dataframe with just enabled vessel official # for analysis
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

vessel_id_field_name <- names(SC_permittedVessels)[1]
# vessel_reg_uscg_

# create new dataframe with just official # codes for analysis
SC_vessel_officalnumber <-
  data.frame(Official_number = tolower(SC_permittedVessels[[vessel_id_field_name]]))

# check for mismatching fields
mismatched_officialnumbers_FHIERvsSC <-
  setdiff(
    FHIER_vessel_officialnumber$Official_number,
    SC_vessel_officalnumber$Official_number
  )

# Now do in reverse to ensure no vessels are in SC list that are not in FHIER list
mismatched_officialnumbers_SCvsFHIER <-
  setdiff(
    SC_vessel_officalnumber$Official_number,
    FHIER_vessel_officialnumber$Official_number
  )

# create output files - use these to update FHIER maintenance list ----
## make output file names ----
project_dir_name <- "mismatched_fields_sc"
output_file_names <- c("remove_from_FHIER",
                       "add_or_enable_in_FHIER")

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


