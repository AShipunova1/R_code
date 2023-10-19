source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# read Jeannette's file ----
library(readxl)  # reading in .xlsx
# the file is from Jeannette Oct 17 2023
v_list_file_name <-
  file.path(
    my_paths$inputs,
    r"(vessels_permits\SA.Permitted.Vessels.Among_revised.Lists.xlsx)"
  )

# Create a sequence of sheet numbers from 1 to 4. These sheet numbers will be used to specify which sheets to read from the Excel file.
sheets <- seq(1:4)

# Use 'purrr::map' to read data from multiple sheets of an Excel file.
# For each sheet, read the data and store it in a list.
all_sheets_l <-
  purrr::map(sheets,
             function(sheet_num) {
               readxl::read_excel(
                 # The name of the Excel file.
                 v_list_file_name,
                 # Specify the sheet number to read.
                 sheet = sheet_num,
                 # Maximum number of rows to guess data types
                 guess_max = 21474836,
                 # Read all columns as text to preserve data integrity.
                 col_types = "text",
                 # Use my function for name repair for column names.
                 .name_repair = fix_names,
                 # Do not use the first row as column names
                 col_names = FALSE
               )
             })

# Check what's inside ----
map(all_sheets_l, dim)
# [[1]]
# [1] 2215    1
#
# [[2]]
# [1] 55  2
#
# [[3]]
# [1] 126   1
#
# [[4]]
# [1] 130   2

map(all_sheets_l, glimpse)

# Clean up dfs ----

## add sheet names to the df list ----
# Extract the sheet names from an Excel file specified by 'v_list_file_name'.
# - 'excel_sheets' function is used to get the sheet names.
# - 'str_replace_all' replaces any periods with underscores in sheet names.
# - 'tolower' converts all sheet names to lowercase.
all_sheets_l_names <-
  readxl::excel_sheets(v_list_file_name) |>
  str_replace_all("\\.", "_") |>
  tolower()

# Rename the elements of the 'all_sheets_l' object to match the modified sheet names.
# - 'names' function is used to access and modify the names of the object.
# - 'all_sheets_l_names' is a character vector containing the modified sheet names.
# - Use the sheet names for all dataframes in the 'all_sheets_l'  list.
#   The names of 'all_sheets_l' are updated to match the corresponding sheet names.
names(all_sheets_l) <- all_sheets_l_names[1:length(all_sheets_l)]

## Remove old column names ----
# like "Vessel id"
# Define a function called 'remove_first_row_if_vessel'.
# This function takes a data frame 'my_df' as its input parameter.
remove_first_row_if_vessel <- function(my_df) {

  # Check if the first cell (1,1) of 'my_df' contains the word "vessel"
  # (case-insensitive search).
  if (grepl("vessel", my_df[1, 1], ignore.case = TRUE)) {
    # If the word "vessel" is found, remove the first row (-1) from 'my_df'.
    my_df <- my_df[-1,]
  }
  # Return the modified 'my_df'. If no changes were made, the original
  # 'my_df' is returned unchanged.
  return(my_df)
}

# Use the 'map' function to apply the 'remove_first_row_if_vessel' function
# to each data frame in the 'all_sheets_l' list.
# The result is a modified 'all_sheets_l' list with the undesired header rows removed from each data frame.

all_sheets_l <-
  purrr::map(all_sheets_l, remove_first_row_if_vessel)

# View(all_sheets_l)

## add names to the columns ----

### 1) ----
# Set the names of the column in the first data frame in the list to "vessel_official_number".
names(all_sheets_l[[1]]) <- c("vessel_official_number")

### 2) ----
# Set the names of the column in the second data frame in the list.
names(all_sheets_l[[2]]) <- c("vessel_official_number",
                              "comments")

### 3) ----
# Set the names of the column in the third data frame in the list.
names(all_sheets_l[[3]]) <- c("vessel_official_number")

### 4) ----
# Set the temporary names of the column in the forth data frame in the list.
names(all_sheets_l[[4]]) <- c("num",
                              "vessel_official_number-comments")

#### Split comments ----
# Create a new data frame 'temp1' by splitting the 'vessel_official_number-comments'
# column of the fourth data frame in 'all_sheets_l' using the 'separate_wider_delim'
# function.
temp1 <- all_sheets_l[[4]] |>
  separate_wider_delim(
    "vessel_official_number-comments",  # Column to split
    delim = " - ",                     # Delimiter used for splitting
    names = c("vessel_official_number", "comments"),  # Names of the new columns
    too_few = "align_end"               # How to handle if fewer columns are created
  )


View(temp1)
# vessels_22_sa ----
# Create a new data frame 'vessels_22_sa' by combining data from two sheets.
vessels_22_sa <-

  # Extract data from the fourth sheet of 'all_sheets_l' (sheet number 4).
  all_sheets_l[[4]] |>

  # Filter rows where the 'group' column contains values 1 or 3.
  filter(group %in% c(1, 3)) |>

  # Select only the 'permit_vessel_id' column.
  select(permit_vessel_id) |>

  # Combine the filtered data with data from the first sheet (sheet number 1) of 'all_sheets_l'.
  rbind(all_sheets_l[[1]])


dim(vessels_22_sa)
# [1] 2321    1

# remove from FHIER results ----
names(all_sheets_l[[2]])
