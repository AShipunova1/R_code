load_csv_names <-
function(csv_files_paths, csv_names_list) {
 # Use 'lapply' to add the 'my_inputs' directory path in front of each file name in 'csv_names_list'
  # This creates a list of full file paths for the CSV files
  myfiles <- lapply(csv_names_list, function(x) file.path(csv_files_paths, x))

  # browser()
  print(myfiles)

  # Use 'lapply' again to read all CSV files listed in 'myfiles'
  # The 'read_csv' function from the 'readr' package is used, specifying default column types as 'c' ('character')
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  # Return the contents of the CSV files as a list
  return(contents)
}
