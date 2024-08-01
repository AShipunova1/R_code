add_tab_to_google_sheet <-
  function(my_df, ss_info, tab_name) {
    # Write "my_df" into the spreadsheet "ss_info"
    # into a sheet/tab with a name "tab_name"
    googlesheets4::write_sheet(my_df, ss = ss_info, sheet = tab_name)
    
    # See sheets/tabs to check
    googlesheets4::sheet_properties(ss = ss_info)
    
    # Remove the empty Sheet1 created automatically by googledrive::drive_create()
    googlesheets4::sheet_delete(ss = ss_info, "Sheet1")
    
    # Check the existing tabs again
    googlesheets4::sheet_properties(ss = ss_info)$name
    # Should be only one name now, like
    # [1] "egregious_violators_to_investigate_2024-07-15"
    
    # See in browser to check
    googledrive::drive_browse(ss_info)
    
    # Generate a shareable link for the new spreadsheet
    current_output_file_link <- googledrive::drive_link(ss_info)
    
    auxfunctions::pretty_print(current_output_file_link, "Link to the new spreadsheet:")
    
    # The function returns the current output file link
    return(current_output_file_link)
  }
