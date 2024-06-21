# get SRHS vessels to exclude ----
srhs_vessels_2022 <-
  r"(~\Official documents\srhs_boats\2022_SRHS_Vessels_08_18_2023.xlsx)"
# "C:\Users\anna.shipunova\Documents\Official documents\srhs_boats\2022_SRHS_Vessels_08_18_2023.xlsx"

srhs_vessels_2022_info <-
  read_excel(
  srhs_vessels_2022,
  # sheet = sheet_n,
  # use my fix_names function for col names
  .name_repair = fix_names,
  guess_max = 21474836,
  # read all columns as text
  col_types = "text"
)
