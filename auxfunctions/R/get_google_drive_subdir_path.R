# Find Google Drive subdir paths
get_google_drive_subdir_path <-
  function(google_drive_project_name = "Egregious violators") {
    google_drive_project_path_info <-
      googledrive::drive_find(pattern =
                                google_drive_project_name,
                              type = "folder",
                              n_max = 1)
    
    return(google_drive_project_path_info)
  }
