rm_tab_from_google_sheet <-
  function(ss_info, tab_name = "Sheet1") {
    # See sheets/tabs to check
    tab_info_0 <-
      googlesheets4::sheet_properties(ss = ss_info)
    
    # Remove the empty Sheet1 created automatically by googledrive::drive_create()
    try(googlesheets4::sheet_delete(ss = ss_info, sheet = tab_name))
    
    # Check the existing tabs again
    tab_info_1 <-
      googlesheets4::sheet_properties(ss = ss_info)$name
    # Should be only one name now, like
    # [1] "egregious_violators_to_investigate_2024-07-15"
    
    # See in browser to check
    # googledrive::drive_browse(ss_info)
    
  }
