add_tab_to_google_sheet <-
  function(my_df, ss_info, tab_name) {
    # Write "my_df" into the spreadsheet "ss_info"
    # into a sheet/tab with a name "tab_name"
    googlesheets4::write_sheet(my_df, ss = ss_info, sheet = tab_name)
    
    # See sheets/tabs to check
    tab_info <- 
      googlesheets4::sheet_properties(ss = ss_info)

    # The function returns the current output file link
    return(tab_info)
  }
