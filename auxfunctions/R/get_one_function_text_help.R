get_one_function_text_help <- 
  function(function_name,
           my_split_newline_char = "@@@") {
    
    
    my_used_function_names <- c(function_name)
    # E.g. my_used_function_names <- c("trim_all_vessel_ids_simple")
    
    # get all my_used_function_texts ----
    my_used_function_texts <-
      auxfunctions::get_my_used_function_texts(my_used_function_names)
    
    # get all my used function helps ----
    my_used_function_helps <-
      auxfunctions::get_my_used_function_helps(my_used_function_names)
    
    idx <- length(my_used_function_names)
    # 1
    
    my_fun_name_to_search <-
      str_glue("(",
               my_split_newline_char,
               ".+{my_used_function_names[[idx]]})")
    
    my_fun_name_w_txt <-
      purrr::reduce(seq_len(length(my_used_function_names)),
                    \(acc, nxt) auxfunctions::replace_function_with_def(acc, nxt),
                    .init = my_fun_name_to_search)
    
    my_fun_name_w_txt_split <-
      auxfunctions::split_one_line_text_back(my_fun_name_w_txt)
    
    my_fun_name_w_txt_split_rmd_text <-
      knitr::spin(text = my_fun_name_w_txt_split,
                  knit = FALSE,
                  format = 'qmd')
    
    # Change back to \s and \b in functions
    my_fun_name_w_txt_split_rmd_text <-
      my_fun_name_w_txt_split_rmd_text |>
      str_replace_all(my_slash_replacement, "\\\\")
    
    cat(my_fun_name_w_txt_split_rmd_text, sep = "\n")
    
    return(my_fun_name_w_txt_split_rmd_text)
  }