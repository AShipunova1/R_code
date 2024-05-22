see_res_in_outfile <- function(var_to_output) {
  # Creates a temporary file with the extension ".txt" and assigns its path to the variable `outfile`.
  outfile <- tempfile(fileext = ".txt")
  sink(file = outfile)
  # Writes the content of `text_to_output` to the temporary file specified by `outfile`.
  print(var_to_output, 
        na.print = "",
        n = nrow(var_to_output))
  # Opens the temporary file in the default text editor for viewing.
  file.show(outfile)
  sink()
}
