prepare_csv_full_path <-
  function(filenames_list,
           add_path,
           input_dir_part = NA) {
    
    #' my_paths is a default
    if (is.na(input_dir_part)) {
      my_paths <- auxfunctions::set_work_dir()
      input_dir_part <- my_paths$inputs
    }
    
    #' add_path
    #' Use subdirectory names for correspondence and compliance files.
    #' add_path <- "from_Fhier/Correspondence"
    #' or
    #' add_path <- "from_Fhier/FHIER Compliance"
    
    # Use 'sapply' to add paths in front of each filename in the 'filenames_list' vector.
    my_list <- sapply(filenames_list, function(x) {
      file.path(input_dir_part, add_path, x)
    })
    
    # Convert the resulting list into a character vector and return it.
    return(paste(my_list) %>% as.list())
  }
