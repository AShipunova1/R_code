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

# check for mismatching fields (in FHIER, but not in SC)
mismatched_officialnumbers_inFHIER_notSC <-
  setdiff(
    FHIER_vessel_officialnumber$Official_number,
    SC_vessel_officalnumber$Official_number
  )

length(mismatched_officialnumbers_inFHIER_notSC)
# 0

# Now do in reverse to ensure no vessels are in SC list that are not in FHIER list
mismatched_officialnumbers_inSC_notFHIER <-
  setdiff(
    SC_vessel_officalnumber$Official_number,
    FHIER_vessel_officialnumber$Official_number
  )

length(mismatched_officialnumbers_inSC_notFHIER)
# 5

# create output files - use these to update FHIER maintenance list ----
## make output file names ----
my_outputs <- list(mismatched_officialnumbers_inFHIER_notSC,
                mismatched_officialnumbers_inSC_notFHIER)

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

# Map over my_outputs and output_file_path_list

# Explanation:
# - This code snippet uses `map2` to iterate over two lists simultaneously: `my_outputs` and `output_file_path_list`.
# - For each pair of elements `(x, y)` in the lists:
#   - It checks if the length of `x` is greater than 0 to ensure there is data to write.
#   - If there is data:
#     - It converts `x` to a data frame.
#     - Sets the column names of the data frame based on the file name without extension using `tools::file_path_sans_ext`.
#     - Writes the data frame to a CSV file specified by the path `y` using `write_csv`.
# - This process is repeated for each pair of elements in the lists.

map2(my_outputs, output_file_path_list, \(x, y) {
  if (length(x) > 0) {
    output_df <- as.data.frame(x)
    names(output_df) <-
      tools::file_path_sans_ext(basename(y))
    write_csv(output_df, y)
  }
})
