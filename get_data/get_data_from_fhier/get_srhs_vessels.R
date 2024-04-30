# get SRHS vessels to exclude ----
# The file is provided by Kenneth Brennan

# files to have:
# from "Logbook Processing (Do this before all Logbook Analyses)" google drive
# https://drive.google.com/drive/folders/18ociLUchXpLxrhb3-gJRuIV_0PaQGUFy?usp=sharing
# "2023SRHSvessels.csv"

file_name <- "2023SRHSvessels.csv"
srhs_vessels_2022_path <-
  file.path(my_paths$inputs,
            "from_Fhier",
            file_name)

# file.exists(srhs_vessels_2022)

srhs_vessels_2022_info <-
  readr::read_csv(
    srhs_vessels_2022_path,
    # all columns are read as characters.
    col_types = cols(.default = 'c'),
    # Automatically repair column names to be syntactically valid.
    name_repair = fix_names
  )

# View(srhs_vessels_2022_info)
