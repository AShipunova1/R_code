# get SRHS vessels to exclude ----
# The file is provided by Kenneth Brennan

# files to have:
# from "Logbook Processing (Do this before all Logbook Analyses)" google drive
# https://drive.google.com/drive/folders/18ociLUchXpLxrhb3-gJRuIV_0PaQGUFy?usp=sharing
# "2023SRHSvessels.csv"

file_name <- "2023SRHSvessels.csv"
srhs_vessels_2022 <-
  file.path(my_paths$inputs,
            "from_Fhier",
            file_name)


srhs_vessels_2022_info <-
  read_excel(
    srhs_vessels_2022,
    # add the sheet name if needed and uncomment the next line
    # sheet = sheet_n,
    # use my fix_names function for col names
    .name_repair = fix_names,
    # if omitted, the algorithm uses only the few first lines and sometimes guesses it wrong
    guess_max = 21474836,
    # read all columns as text
    col_types = "text"
  )
