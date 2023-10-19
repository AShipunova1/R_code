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
# Create a new data frame 'sheet_4_temp_1' by splitting the 'vessel_official_number-comments'
# column of the fourth data frame in 'all_sheets_l' using the 'separate_wider_delim'
# function.
sheet_4_temp_1 <- all_sheets_l[[4]] |>
  separate_wider_delim(
    "vessel_official_number-comments",  # Column to split
    delim = " - ",                     # Delimiter used for splitting
    names = c("vessel_official_number", "comments"),  # Names of the new columns
    too_few = "align_end"               # How to handle if fewer columns are created
  )


# Create a new data frame 'sheet_4_temp_2' by performing a series of operations on the
# data frame 'sheet_4_temp_1':

# 1. Use 'group_split' to split 'sheet_4_temp_1' into multiple groups.
#    - 'grp' parameter creates groups based on cumulative sums of NA values
#      in each row. A new group starts when a row contains all NA values.
#    - '.keep = TRUE' means that the grouping column is retained in each group.

# 2. Use 'purrr::map_at' to apply a function to specific columns in each group.
#    - '.at = -1' specifies the position of the columns to be operated on
#      (all columns except the last one).
#    - 'tail' function is applied to each specified column within each group.
#    - '-1' indicates that the last element (row) of each column should be extracted.

# View(sheet_4_temp_1)
sheet_4_temp_2 <-
  sheet_4_temp_1 %>%
  group_split(grp = cumsum(rowSums(is.na(.)) == ncol(.)), .keep = TRUE) %>%
  purrr::map_at(.at = -1, tail, -1)

# change the last group columns
# sheet_4_temp_2[[3]] |> head()
# num vessel_official_number comments grp
# <chr> <chr> <chr> <int>
# 1 NA NA 99 vessels did not have the isLatest flag … 2
# 2 1.0 NA 555341.0 2
# 3 2.0 NA 556601.0 2

# Create a new data frame 'sheet_4_temp_3' by applying mutations to the third group
# in 'sheet_4_temp_2' using the 'dplyr::mutate' function.

sheet_4_temp_2[[3]] <- sheet_4_temp_2[[3]] |>
  dplyr::mutate(vessel_official_number =
                  dplyr::case_when(
                    # If 'comments' contains a space, it is Janette's comment, set 'vessel_official_number' to NA.
                    grepl(" ", comments) ~ NA,
                    # For all other cases, remove '.0' from 'comments' and assign it to 'vessel_official_number'.
                    .default = stringr::str_replace(comments, "\\.0", "")
                  ))

# Create a new data frame 'sheet_4' by binding rows from a list of data frames 'sheet_4_temp_2'.
# The 'dplyr::bind_rows' function combines the data frames in the list into a single data frame.
sheet_4 <- dplyr::bind_rows(sheet_4_temp_2)

# put sheet_4 back to the common list
all_sheets_l[[4]] <- sheet_4
# View(all_sheets_l[[4]])

# vessels_22_sa ----
# Create a new data frame 'vessels_22_sa' by combining data from two sheets.
vessels_22_sa <-

  # Extract data from the fourth sheet of 'all_sheets_l' (sheet number 4).
  all_sheets_l[[4]] |>

  # Filter rows where the 'grp' column contains values 0 or 2.
  dplyr::filter(grp %in% c(0, 2)) |>

  # Select only the 'vessel_official_number' column.
  dplyr::select(vessel_official_number) |>

  # Combine the filtered data with data from the first sheet (sheet number 1) of 'all_sheets_l'.
  rbind(all_sheets_l[[1]]) |>
  # na.omit returns the object with incomplete cases removed.
  stats::na.omit()

vessels_22_sa |>
  dim()
# [1] 2321    1

# remove wrong ids from FHIER results ----
vessels_to_remove_from_ours <-
  all_sheets_l$in_ours_not_jeannettes$vessel_official_number

length(vessels_to_remove_from_ours)
# 55
