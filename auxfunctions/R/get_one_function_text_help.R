get_one_function_text_help <- 
  function(function_name,
           my_split_newline_char = "@@@") {
    
    # Create a vector containing the function name
    my_used_function_names <- c(function_name)
    # Example: my_used_function_names <- c("trim_all_vessel_ids_simple")
    
    # Get the text for all the functions used
    my_used_function_texts <-
      auxfunctions::get_my_used_function_texts(my_used_function_names)
    
    # Get the help documentation for all the functions used
    my_used_function_helps <-
      auxfunctions::get_my_used_function_helps(my_used_function_names)
    
    # Get the length of the function names vector
    idx <- length(my_used_function_names)
    # Example: 1
    
    # Create a pattern to search for the function name in the text
    my_function_name_to_search <-
      stringr::str_glue("(",
               my_split_newline_char,
               ".+{my_used_function_names[[idx]]})")
    
    # Replace function names with their definitions in the text
    my_function_name_w_txt <-
      purrr::reduce(seq_len(length(my_used_function_names)),
                    \(acc, nxt) auxfunctions::replace_function_with_def(acc, nxt),
                    .init = my_function_name_to_search)
    
    # Split the single line text back into multiple lines
    my_function_name_w_txt_split <-
      auxfunctions::split_one_line_text_back(my_function_name_w_txt)
    
    # Convert the split text into R Markdown format using knitr::spin
    my_function_name_w_txt_split_rmd_text <-
      knitr::spin(text = my_function_name_w_txt_split,
                  knit = FALSE,
                  format = 'qmd')
    
    # Change back to \s and \b in functions
    my_function_name_w_txt_split_rmd_text <-
      my_function_name_w_txt_split_rmd_text |>
      stringr::str_replace_all(my_slash_replacement, "\\\\")
    
    # Print the final R Markdown formatted text
    cat(my_function_name_w_txt_split_rmd_text, sep = "\n")
    
    # Return the final R Markdown formatted text
    return(my_function_name_w_txt_split_rmd_text)
  }
