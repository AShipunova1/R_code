pretty_print <-
function(my_text, my_title,
                         the_end = "---") {
  # Print out to console
  auxfunctions::title_message_print(my_title)
  cat(c(my_text, the_end),
      sep = "\n")
}
