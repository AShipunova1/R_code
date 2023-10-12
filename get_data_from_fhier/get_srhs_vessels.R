# get SRHS vessels to exclude ----
# The file is provided by Kenneth Brennan

srhs_vessels_2022 <-
  r"(~\Official documents\srhs_boats\2022_SRHS_Vessels_08_18_2023.xlsx)"

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
