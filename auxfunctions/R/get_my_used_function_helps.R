## a function to get a function help as a text ----
get_my_used_function_helps <- function(function_name) {
  # browser()
  used_tags <- c("description", "details")
  help_text <-
    help(function_name, "auxfunctions") |>
    utils:::.getHelpFile()
  
  used_tags_help_list <-
    map(used_tags, \(one_tag) {
      help_text |>
        purrr::keep( ~ attr(.x, "Rd_tag") == paste0("\\", one_tag)) |>
        purrr::map(as.character) %>%
        purrr::flatten_chr() %>%
        paste0(., collapse = "")
    }) |>
    setNames(used_tags)
  
  used_tags_help <- 
    paste(used_tags_help_list[[1]],
          "\n",
          used_tags_help_list[[2]])
  
  used_tags_help_commented <- 
    used_tags_help |> 
    str_replace_all("\n", "\n# ")
  
  return(used_tags_help_commented)
}