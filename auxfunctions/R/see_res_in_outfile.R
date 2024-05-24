#' Show the Long Text in an Outfile
#'
#' @param var_to_output 
#' @param extension 
#'
see_res_in_outfile <- function(var_to_output, extension = ".txt") {
  
  # Creates a temporary file with the specified extension and assigns its path to `outfile`.
  outfile <- tempfile(fileext = extension)
  
  # Checks if the specified extension is ".csv".
  if (extension == ".csv") {
    # Writes the content of `var_to_output` to the temporary CSV file using `write_csv` from the `readr` package.
    readr::write_csv(var_to_output, file = outfile)
  }
  else {
    # Redirects R output to the file specified by `outfile` if the extension is not ".csv".
    sink(file = outfile)
    
    # Writes the content of `var_to_output` to the temporary file specified by `outfile`.
    print(var_to_output,
          na.print = "",  # Specifies that NA values should be printed as an empty string.
          n = nrow(var_to_output))  # Ensures all rows of `var_to_output` are printed.
    
    # Restores the normal R output to the console.
    sink()
  }
  
  # Opens the temporary file in the default text editor for viewing.
  file.show(outfile)
}
