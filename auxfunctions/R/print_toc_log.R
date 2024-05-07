print_toc_log <-
function(variables) {
  # Retrieve the log from the tic package with formatting enabled and store it in 'log.txt'
  log.txt <- tic.log(format = TRUE)

  # Write the lines of 'log.txt' to the console
  writeLines(unlist(log.txt))

  # Clear the log, removing its contents
  tic.clearlog()
}
