split_one_line_text_back <-
  function(my_text_with_at, split_by = my_split_newline_char) {
    my_text_with_at |>
      stringr::str_split(split_by) |>
      unlist()
  }
