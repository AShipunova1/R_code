# read Jeannette's file ----
# Load readxl package for reading Excel files
library(readxl)

# the file is from Jeannette Oct 17 2023
# Set file path and name of the Excel file to read in
v_list_file_name <-
  r"(.\R_files_local\vessels_permits\SA.Permitted.Vessels.Among_revised.Lists.xlsx)"

# Create vector of sheet numbers to read
sheets <- seq(1:4)

# Loop through sheet numbers
# Use map() to apply read_excel() to each sheet
# Returns a list of data frames
all_sheets_l <- map(sheets, function(sheet_num) {

  # Read in Excel sheet by sheet number
  read_excel(
    v_list_file_name,
    sheet = sheet_num,

    # Read all columns as text
    col_types = "text",

    guess_max = 21474836,

    # Use universal name repair
    .name_repair = "universal"
  )
})

# map(all_sheets_l, dim)
# [[1]]
# [1] 2216    1
#
# [[2]]
# [1] 55  2
#
# [[3]]
# [1] 126   1
#
# [[4]]
# [1] 131   4

# Filter sheet 4 to only group 1 and 3 rows
vessels_22_sa <- all_sheets_l[[4]] |>
  filter(group %in% c(1, 3)) |>

  # Select only permit_vessel_id column
  select(permit_vessel_id) |>

  # Bind rows with sheet 1 data
  rbind(all_sheets_l[[1]])

dim(vessels_22_sa)
# [1] 2321    1

