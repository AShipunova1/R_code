# Find Google Drive subdir paths
get_google_drive_subdir_path <-
  function(google_drive_path_info,
           google_drive_subdir_name = "output") {
    google_drive_subdir_info <-
      googledrive::drive_ls(
        path = googledrive::as_id(google_drive_path_info),
        pattern = google_drive_subdir_name,
        type = "folder",
        n_max = 1
      )
    
    return(google_drive_subdir_info)
  }
