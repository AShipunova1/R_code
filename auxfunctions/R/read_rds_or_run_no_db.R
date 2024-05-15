read_rds_or_run_no_db <-
function(my_file_path,
           my_data_list_of_dfs,
           my_function) {

    if (file.exists(my_file_path)) {
      # read a binary file saved previously
      my_df <-
        readr::read_rds(my_file_path)
    } else {
      tictoc::tic("run the function")
      my_df <-
        my_function(my_data_list_of_dfs[[1]],
                    my_data_list_of_dfs[[2]])
      tictoc::toc()

      # write all as binary
      readr::write_rds(my_df,
                       my_file_path)
    }

    return(my_df)
  }
