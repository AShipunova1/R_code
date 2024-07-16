function_obj_as_text <- function(function_name) {
  # remove environment descriptions
  fun_body <- paste(capture.output(function_name), collapse = "\n") |>
    stringr::str_replace_all("\\n<.+", "")
  
  return(fun_body)
}
