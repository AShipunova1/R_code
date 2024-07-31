fix_names <-
  function(x) {
    x |>
      
      stringr::str_replace_all("\\.", "") |>
      
      stringr::str_replace_all("[^A-z0-9]", "_") |>
      
      stringr::str_replace_all("^(_*)(.+)", "\\2\\1") |>
      
      # Convert column names to lowercase using 'my_headers_case_function'
      auxfunctions::my_headers_case_function()
  }
