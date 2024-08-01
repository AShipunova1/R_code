write_to_google_sheets <- function(variables) {
  
}
    # Define the current result Google Sheets name
    current_result_google_ss_name <- "Egregious Violators Current"
    
    # my_current_ss contains information about the existing 'Egregious Violators Current' file
    my_current_ss <-
      googledrive::drive_ls(
        path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
        pattern = current_result_google_ss_name,
        type = "spreadsheet",
        n_max = 1
      )
    
    # An example of my_current_ss:
    #   name                        id                                           drive_resource
    # 1 Egregious Violators Current ...--o6BpLWpb4-... <named list [36]>
    
    # Next:
    # 1) load it to R;
    # 2) create a new spreadsheet with the date of the loaded worksheet and dump the content into it;
    # 3) create a new worksheet in the current spreadsheet with today's date;
    # 4) write the code output into it;
    # 5) check in browser.
    
    # 1) load it to R
    previous_current_content <- googlesheets4::read_sheet(my_current_ss)
    
    # 2) create a new spread sheet with the date of loaded worksheet and dump the content into it
    # a) get the previous spreadsheet name
    
    # ss_info contains detailed information about the current spreadsheet, including sheet names
    ss_info <- googlesheets4::gs4_get(my_current_ss)
    
    # grep for the pattern in case there are additional tabs
    previous_current_spread_sheet_name <-
      grep(
        "egregious_violators_to_investigate_20\\d\\d-\\d\\d-\\d\\d",
        ss_info$sheets$name,
        value = T
      )
    # E.g. "egregious_violators_to_investigate_2024-06-18"
    
    # Rename the file from "current" to the previous_current_spread_sheet_name with the previous date.
    # In case of an error print the message and keep going.
    # If there is a file with this name this code will create another one with the same name.

    tryCatch({
      message("Try to rename the file")
      
      googledrive::drive_mv(
        my_current_ss,
        path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
        name = previous_current_spread_sheet_name
      )
      # E.g.
      # Original file:
      # • Egregious Violators Current
      # Has been renamed:
      # • output/egregious_violators_to_investigate_2024-06-18
      
    }, error = function(cond) {
      message(
        paste(
          "Failed to rename this file: ",
          previous_current_spread_sheet_name
        )
      )
      
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
    }, warning = function(cond) {
      
    }, finally = {
      # message("Some other message at the end")
    })
    
    # Create a new empty spreadsheet in the Google Drive output folder to replace the renamed one
    # And save its properties into current_result_google_ss_name_info
    # In case of an error print the message and keep going.
    # If there is a file with this name this code will create another one with the same name.
    
    tryCatch({
      message("Try to create a new file")
      
      current_result_google_ss_name_info <-
        googledrive::drive_create(
          name = current_result_google_ss_name,
          path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
          type = "spreadsheet"
        )
      
    }, error = function(cond) {
      message(
        paste(
          "Failed to create a new file: ",
          current_result_google_ss_name
        )
      )
      
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
    }, warning = function(cond) {
      
    }, finally = {
      # message("Some other message at the end")
    })
    
    # Write our results into the newly created spreadsheet "Egregious Violators Current"
    # into a sheet/tab with a name defined in out_file_basename
    googlesheets4::write_sheet(
      compl_corr_to_investigation_short_dup_marked__permit_region__add_columns,
      ss = current_result_google_ss_name_info,
      sheet = out_file_basename
    )
    
    # See sheets/tabs to check
    googlesheets4::sheet_properties(ss = current_result_google_ss_name_info)
    
    # Remove the empty Sheet1 created automatically by googledrive::drive_create()
    googlesheets4::sheet_delete(ss = current_result_google_ss_name_info, "Sheet1")
    
    # Check the existing tabs again
    googlesheets4::sheet_properties(ss = current_result_google_ss_name_info)$name
    # Should be only one name now, like
    # [1] "egregious_violators_to_investigate_2024-07-15"
    
    # See in browser to check
    googledrive::drive_browse(current_result_google_ss_name_info)
    
    # Generate a shareable link for the new spreadsheet
    current_output_file_link <- googledrive::drive_link(current_result_google_ss_name_info)
    
    auxfunctions::pretty_print(current_output_file_link, "Link to the new spreadsheet:")
    
    # The function returns the current output file link
    return(current_output_file_link)
    
  