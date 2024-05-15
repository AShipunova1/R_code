see_res_in_outfile <- function(text_to_output) {
  # Creates a temporary file with the extension ".txt" and assigns its path to the variable `outfile`.
  outfile <- tempfile(fileext = ".txt")
  # Writes the content of `text_to_output` to the temporary file specified by `outfile`.
  cat(text_to_output, file = outfile)
  # Opens the temporary file in the default text editor for viewing.
  file.show(outfile)
}
