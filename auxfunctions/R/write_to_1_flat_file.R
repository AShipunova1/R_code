write_to_1_flat_file <-
function(flat_file_name, file_name_to_write) {
  # Redirect the output to the specified 'flat_file_name' and append content.
  sink(flat_file_name, append = TRUE)

  # Read the contents of the current file.
  current_file_text <- readr::read_lines(file_name_to_write)

  # Print a header indicating the current file being processed.
  cat("\n\n#### Current file:", basename(file_name_to_write), "----\n\n")

  # Print the contents of the current file, separating lines with newline characters.
  cat(current_file_text, sep = "\n")

  # # Restore the default output behavior.
  sink()
}
