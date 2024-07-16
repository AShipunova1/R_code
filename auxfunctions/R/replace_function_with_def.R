replace_function_with_def <- 
  function(one_line_text, idx) {
    # browser()
    to_find <- stringr::str_glue("(",
                        my_split_newline_char,
                        ".+{my_used_function_names[[idx]]})")
    to_replace_with <-
      paste(
        "\n# <<<<",
        my_used_function_texts[[idx]],
        "\n# Explanations:",
        my_used_function_helps[[idx]],
        "# >>>>",
        "\\1", #to keep in place what's found
        sep = "\n"
      )
    
    one_line_text_replaced <-
      stringr::str_replace(one_line_text, to_find, to_replace_with)
    
    return(one_line_text_replaced)
  }