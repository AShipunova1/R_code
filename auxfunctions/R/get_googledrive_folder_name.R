# Set up Google Drive paths
get_google_drive_folder_name <-
  function(google_drive_project_name = "Egregious violators",
           google_drive_output_name = "output") {
    google_drive_project_info <-
      googledrive::drive_find(pattern =
                                google_drive_project_name,
                              type = "folder",
                              n_max = 1)
    
    google_drive_output_info <-
      googledrive::drive_ls(
        path = googledrive::as_id(google_drive_project_info),
        pattern = google_drive_output_name,
        type = "folder",
        n_max = 1
      )
    
  }
