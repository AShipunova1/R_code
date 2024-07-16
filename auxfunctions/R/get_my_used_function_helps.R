get_my_used_function_helps <- function(function_name) {
  # Un-comment to enable browser() for debugging
  # browser()
  
  # Define the tags to look for in the help file
  used_tags <- c("description", "details")
  
  # Get the help file for the given function from the "auxfunctions" package
  help_text <- 
    help(function_name, "auxfunctions") |>
    utils:::.getHelpFile()
  
  # Extract the relevant sections of the help file based on the defined tags
  used_tags_help_list <- 
    map(used_tags, \(one_tag) {
      help_text |>
        purrr::keep(~ attr(.x, "Rd_tag") == paste0("\\", one_tag)) |>
        purrr::map(as.character) |>
        purrr::flatten_chr() |>
        paste0(collapse = "")
        # paste0(., collapse = "")

    }) |>
    setNames(used_tags)
  
  # Combine the extracted sections into a single string
  used_tags_help <- 
    paste(used_tags_help_list[[1]], 
          "\n", 
          used_tags_help_list[[2]])
  
  # Add comment symbols to each line of the help text
  used_tags_help_commented <- 
    used_tags_help |> 
    str_replace_all("\n", "\n# ")
  
  # Return the commented help text
  return(used_tags_help_commented)
}
