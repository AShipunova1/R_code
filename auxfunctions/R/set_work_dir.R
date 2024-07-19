set_work_dir <-
  function() {
    # Set the working directory to the user's home directory (~)
    setwd("~/")
    base_dir <- getwd()
    
    main_r_dir <- "R_files_local"
    
    in_dir <- "my_inputs"
    
    full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
    
    out_dir <- "my_outputs"
    
    full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)
    
    git_r_dir <- "R_code_github"
    
    full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)
    
    # Change the working directory
    setwd(file.path(base_dir, main_r_dir))
    
    # Create a list of directory paths for 'inputs,' 'outputs,' and 'git_r'
    my_paths <- list("inputs" = full_path_to_in_dir,
                     "outputs" = full_path_to_out_dir,
                     "git_r" = full_path_to_r_git_dir)
    
    # Return the list of directory paths
    return(my_paths)
  }
