create_google_sheet <-
  function(output_path,
           new_google_ss_name) {
    
  # Create a new empty spreadsheet in the Google Drive output folder
  # And save its properties into current_result_google_ss_name_info
  # In case of an error print the message and keep going.
  # If there is a file with this name this code will create another one with the same name.
  
  tryCatch({
    message("Try to create a new file")
    
    current_result_google_ss_name_info <-
      googledrive::drive_create(
        name = current_result_google_ss_name,
        path = googledrive::as_id(output_path),
        type = "spreadsheet"
      )
    
  }, error = function(cond) {
    message(paste(
      "Failed to create a new file: ",
      current_result_google_ss_name
    ))
    
    message("Here's the original error message:")
    message(conditionMessage(cond))
    # Choose a return value in case of error
  }, warning = function(cond) {
    
  }, finally = {
    # message("Some other message at the end")
  })
  
  # The function returns the created spreadsheet info
  return(current_result_google_ss_name_info)
  }
