create_dir_if_not <-
function(curr_dir_name) {
  # Check if the directory does not exist
  if (!dir.exists(curr_dir_name)) {
    dir.create(curr_dir_name)  # Create the directory if it doesn't exist
  }
}
