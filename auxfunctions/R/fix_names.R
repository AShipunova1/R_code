fix_names <-
function(x) {
  # Use the pipe operator %>%
  x %>%

    # Remove dots from column names
    stringr::str_replace_all("\\.", "") %>%

    # Replace all characters that are not letters or numbers with underscores
    stringr::str_replace_all("[^A-z0-9]", "_") %>%

    # Ensure that letters are only in the beginning of the column name
    stringr::str_replace_all("^(_*)(.+)", "\\2\\1") %>%

    # Convert column names to lowercase using 'my_headers_case_function'
    my_headers_case_function()
}
