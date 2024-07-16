get_my_used_function_texts <-
  function(my_used_function_names) {
    my_used_function_texts <-
      my_used_function_names |>
      map(\(one_f_name) {
        # browser()
        function_list <- getAnywhere(one_f_name)
        
        function_as_text <-
          function_list$objs[[1]] |>
          function_obj_as_text() |>
          str_replace_all("\\\\", my_slash_replacement)
        
        with_first_line <-
          paste(one_f_name, " <- ", function_as_text)
        
        return(with_first_line)
      }) |>
      set_names(my_used_function_names)
    
    return(my_used_function_texts)
  }

