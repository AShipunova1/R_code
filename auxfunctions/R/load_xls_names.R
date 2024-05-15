load_xls_names <-
function(my_paths, xls_names_list, sheet_n = 1) {

  # Extract the 'inputs' directory path from 'my_paths' and store it in 'my_inputs'
  my_inputs <- my_paths$inputs

  # Use 'lapply' to prepend 'my_inputs' directory path to each Excel file name in 'xls_names_list'
  myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))

  # Read Excel files listed in 'myfiles' into one data frame using 'map_df'
  contents <-
    purrr::map_df(myfiles,
           ~ my_read_xlsx(.x, # File path
                          sheet_n, # Sheet number to read)
    ))

  # Return the concatenated data frame containing data from all Excel files
  return(contents)
}
