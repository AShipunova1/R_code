#' Show the Long Text in an Outfile
#'
#' @param var_to_output 
#' @param extension 
#'
see_res_in_outfile <- function(var_to_output, extension = ".txt") {
  # Creates a temporary file and assigns its path to the variable `outfile`.
  outfile <- tempfile(fileext = extension)
  sink(file = outfile)
  # Writes the content of `text_to_output` to the temporary file specified by `outfile`.
  print(var_to_output, 
        na.print = "",
        n = nrow(var_to_output))
  # Opens the temporary file in the default text editor for viewing.
  file.show(outfile)
  sink()
}
