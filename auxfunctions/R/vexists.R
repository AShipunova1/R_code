vexists <-
function(...) {
  # Use 'substitute' to capture the variable names from the arguments and convert them to character vectors.
  vars <- as.character(substitute(...()))

  # Use 'sapply' to iterate over the variable names and check if each variable exists in the current environment.
  exists_check <- sapply(vars, exists)

  # Return a logical vector indicating the existence of each variable.
  return(exists_check)
}
