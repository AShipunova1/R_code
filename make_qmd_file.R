# This script is designed to create a Quarto Markdown (.qmd) file for R projects.
library(tidyverse)
library(tools)

# How to use the result qmd:
# To see the report .html - render the .qmd
# to run - "run all" in the .qmd tab (Ctrl-Alt-R)

# Manual steps to adjust the generated .qmd file:
# 1) Change in the result qmd
# ```{r setup ...
# with provided
# ```{r setup current project, results='hide', message=FALSE, warning=FALSE}
# 2) Change the title
# 3) Move the Setup title up
# 4) For egregious violators:
# a) mv trim_all_vessel_ids_simple close to use and remove the previous one (near setup) <<<<>>>>
# b) Do the same for clean_headers (at Results) 

# Change the dir name
# Comment out the "answers" for the next two readline() to use interactively
curent_project_name <- readline(prompt = "Print your project name: ")
egregious_violators
# validation_survey

curent_file_name_no_ext <- readline(prompt = "Print your file name: ")
egregious_violators_start
# validation_survey_main

# In the input .R script:
# add "#' " in front of comments to be shown as text
# and #' as the last line of the visible comments
# add #+, file = prep_addresses_path, instead of a source, to be converted in a chunk

# In the output .qmd:
# *) add |>
#   knitr::kable(caption = "My Caption")
# for pretty tables

# setup ----
# test_function
check_str <- function(my_string, my_pattern) {
  my_string |>
    stringr::str_extract(my_pattern) |>
    as.data.frame() |>
    setNames(nm = "found") |>
    filter(!is.na(found))
}

my_split_newline_char <- "@@@"

to_one_line <-
  function(my_text_with_newline,
           glue_by = my_split_newline_char) {
    my_text_with_newline |>
      str_flatten(collapse = glue_by)
    # paste(collapse = glue_by)
  }

split_one_line_text_back <-
  function(my_text_with_at, split_by = my_split_newline_char) {
    my_text_with_at |>
      str_split(split_by) |>
      unlist()
  }

# source("~/R_code_github/useful_functions_module.r")

# install.packages("devtools")
# library(devtools)
# devtools::install_github("AShipunova1/R_code/auxfunctions")
library(auxfunctions)

my_paths <- auxfunctions::set_work_dir()

dir_to_comb <-
  file.path(my_paths$git_r,
            curent_project_name)

dir.exists(dir_to_comb)

file_name <- curent_file_name_no_ext
  # curent_project_name
file_ext <- c("R", "Rmd", "qmd")

# Create a list of file paths for each file extension.
file_paths <-
  purrr::map(file_ext,
      ~ file.path(dir_to_comb,
                  paste0(file_name, ".", .x)))

# Set the names of the list elements to 'file_ext'.
names(file_paths) <- file_ext

# To debug
# Open the text in VSC
see_res_in_outfile <- function(text_to_output) {
  outfile <- tempfile(fileext = ".txt")
  cat(text_to_output, file = outfile)
  file.show(outfile)
}

# prepare all pieces ----
## read the main file content ----
flat_file_r_text <-
  readLines(file_paths$R)

head(flat_file_r_text)

# main_file_one_line

## read all sourced files ----
# grep("source", flat_file_r_text, value = T)

find_source_paths <- function(my_text = flat_file_r_text) {
  source_path_all_list <-
    my_text |>
    str_extract_all("^\\s*[^#]*source\\((.+)\\)") |>
    unique()

  source_path_list <-
    purrr::discard(source_path_all_list, ~ length(.x) == 0)

  return(source_path_list)
}

source_paths_matches <- find_source_paths()

find_source_path_vars <-
  function(my_text = flat_file_r_text,
           source_paths_matches = source_paths_matches) {
    source_path_vars <-
      source_paths_matches |>
      map(\(one_var) {
        stringr::str_replace(one_var, "\\w+\\((.+)_path\\)", "\\1")
      })

    return(source_path_vars)
  }

source_path_var_names <-
  find_source_path_vars(my_text = flat_file_r_text,
           source_paths_matches = source_paths_matches)

# assuming files are named by convention:
# FILE_NAME_PART_path
# {curent_project_name}_{FILE_NAME_PART} in the current project directory dir_to_comb
# i.e.
# prepare_data_path <-
#   file.path(current_project_dir_name,
#             paste0(current_project_name, "_", "prepare_data.R"))
#
# file.exists(prepare_data_file_path)
#
# source(prepare_data_file_path)

make_source_path <- function(dir_to_comb, source_path_var_names) {
  source_path_var_names |>
    map(\(source_path_var_name) {
      file.path(dir_to_comb,
                paste0(curent_project_name, "_", source_path_var_name, ".R"))
    })
}

source_paths <- make_source_path(dir_to_comb, source_path_var_names)

read_source_files <- function(source_paths) {
  file_content <-
    source_paths |>
    map(\(one_path) {
      if (file.exists(one_path)) {
        one_text <-
          readLines(one_path)
      }
    })
  return(file_content)
}

purrr::map(source_paths, file.exists)
# "C:\Users\anna.shipunova\Documents\R_code_github\validation_survey\validation_survey_get_data.R"
# [1] "C:/Users/anna.shipunova/Documents/R_code_github/validation_survey/validation_survey_get_data_file.R"

source_files_content <- read_source_files(source_paths) |>
  set_names(source_paths_matches)

# View(source_files_content)

### sorce_files_content to one line ----
source_files_content_one_line <-
  source_files_content |>
  map(to_one_line)

# to_one_line(flat_file_r_text, my_split_newline_char)
# str(source_files_content_one_line)

## combine all files ----

## find sourced files ----
# check
# grep("file =", flat_file_r_text, value = T)

# replace sourced files with the file content

to_find_source <-
  str_c(str_escape(unlist(source_paths_matches)),
        collapse = "|")
# str(to_find)

to_replace_source <-
  function(source_text) {
    return(source_files_content_one_line[[source_text]])
  }

flat_file_r_text <-
  stringr::str_replace_all(flat_file_r_text,
                           to_find_source,
                           to_replace_source)

# check
# grep(my_split_newline_char, flat_file_r_text, value = T)
# grep("source", flat_file_r_text, value = T)

### remove 'if (!exists("con"))', gives an error when rendering ----
to_find_con <-
  'if *\\(\\!exists\\("con"\\)[^}]+\\}'
to_replace_con <- "try(con <- auxfunctions::connect_to_secpr())"

flat_file_r_text <-
  stringr::str_replace_all(flat_file_r_text,
                           to_find_con,
                           to_replace_con
                           )

### split one line text back ----
# Otherwise it is too long and breaks the RStudio
flat_file_r_text <-
  split_one_line_text_back(flat_file_r_text)

# check
# grep("get data for egregious violators", flat_file_r_text, value = T)
# tail(flat_file_r_text)
# length(flat_file_r_text)

## Add headers to the flat file to be converted by knitr ----
# In this code, 'flat_file_r_text' is generated by modifying the text content read from the file specified by 'r_file_name'. The 'gsub' function is used to replace specific patterns in the text:
#
# It searches for lines that start with one or more '#' symbols followed by a space, captures the content after that, and then captures a line with "----" at the end.
# It replaces this pattern with a modified format, adding "#'" at the beginning of the first line for headers,
# and copying the pattern on the second line to keep comments in place.
# This transformation is used to adapt R script file to an R Markdown format by converting header lines to Roxygen-style comments.
#

# In this code, 'flat_file_r_text' is modified using the 'gsub' function to replace specific patterns in the text. The pattern being searched for is defined using a regular expression.
# It automatically makes chunk titles

unify_comments <-
  function(flat_file_r_text) {
    flat_file_r_text <-
      gsub(" ====",
           " ----",
           flat_file_r_text)
    return(flat_file_r_text)
  }

make_chunk_titles_from_comments <-
  function(flat_file_r_text) {

    pattern_to_add_md_headers <- "#' \\1\\2"
    pattern_to_add_md_chunk_labels <- "#+ \\2"
    pattern_to_repeat_the_original <- "\\1\\2\\3"

    flat_file_r_text <-
      unify_comments(flat_file_r_text)

    flat_file_r_text <-
      gsub(
        "^(#+ )(.+)(----)",
        paste(
          pattern_to_add_md_headers,
          pattern_to_add_md_chunk_labels,
          pattern_to_repeat_the_original,
          sep = "\\\n"
        ),
        flat_file_r_text
      )

    return(flat_file_r_text)
  }

flat_file_r_text <-
  make_chunk_titles_from_comments(flat_file_r_text)

# It searches for lines starting with "#+" followed by a space and captures the content after that.
# It captures a single quote or a slash.
# It captures more content.
# It captures a newline character.
# Remove all "odd" characters from chunk titles for knitr to work with.
# repeat twice
#
clean_chunk_titles <-
  function(flat_file_r_text) {

    while (any(stringr::str_detect(flat_file_r_text,
                          "#\\+[^#]*[^A-z0-9#+ ]+[^#]+"))) {
      flat_file_r_text <-
        flat_file_r_text |>
        stringr::str_replace_all("(#\\+[^#]*)[^A-z0-9#+ ]+([^#]+)", "\\1\\2")

    }

    return(flat_file_r_text)
  }

flat_file_r_text <-
  clean_chunk_titles(flat_file_r_text)

# check
# grep("how many are duals", flat_file_r_text, value = T)

## Change all sections to a level lower ----
# works with the next step, convert %%%%% to the level 1
lower_section_level <-
  function(flat_file_r_text) {
    flat_file_r_text <-
      gsub("(#+) (.+)(----)",
           "\\1# \\2\\3",
           flat_file_r_text)
    return(flat_file_r_text)
  }

flat_file_r_text <-
  lower_section_level(flat_file_r_text)

## add 2 top sections ----
# E.g. "Prepare data" and "Plots", marked in the R script with #' %%%%%
# like #' %%%%% Prepare data
# not used in the auxiliary files

add_topmost_sections <- function(flat_file_r_text) {
  flat_file_r_text <-
    gsub("(%%%%%+) ", # was defined in the original .R
         "# ",
         flat_file_r_text)

  return(flat_file_r_text)
}

flat_file_r_text <-
  add_topmost_sections(flat_file_r_text)

## add layouts
add_layouts <- function(flat_file_r_text) {
  flat_file_r_text <-
    gsub(
      "(^#\\|)", # was defined in the original .R
      "#|",
      flat_file_r_text
    )
  return(flat_file_r_text)
}

flat_file_r_text <- add_layouts(flat_file_r_text)

# add "pretty" table output (add kable to glimpse)
add_pretty_table <-
  function(flat_file_r_text) {
    flat_file_r_text <-
      gsub(
      "(^ *[^#] +)(dplyr::glimpse)(\\(\\S*\\))",
      # was in the original .R
      '\\1\\2\\3 |>
      str_replace_all("\\n", "\\\\n") |> 
      htmltools::htmlEscape() |> 
\\1knitr::kable(caption = "")',
flat_file_r_text
    )
    return(flat_file_r_text)
  }

# knitr can't deal with "\n" in contactcomments
# flat_file_r_text <-
  # add_pretty_table(flat_file_r_text)

# Add auxfunctions' descriptions ----

## Get all function names ----

# see_res_in_outfile(flat_file_r_text)

all_auxfunction_names <- 
  lsf.str("package:auxfunctions") |> str_replace("(.+) :.+", "\\1")

## a function to get function help as a text ----
get_help_text <- function(function_name) {
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
    stats::setNames(used_tags)

  used_tags_help <-
    paste(used_tags_help_list[[1]],
          "\n",
          used_tags_help_list[[2]])

  used_tags_help_commented <-
    used_tags_help |>
    stringr::str_replace_all("\n", "\n# ")

  return(used_tags_help_commented)
}

## a function to get function obj as a text ----
function_obj_as_text <- function(function_name) {
  # remove environment descriptions
  fun_body <- paste(utils::capture.output(function_name), collapse = "\n") |>
    stringr::str_replace_all("\\n<.+", "")

  return(fun_body)
}

# for \s and \b in the cited functions
my_slash_replacement <- "QQQ"

## get all all_auxfunction_texts ----
get_all_auxfunction_texts <-
  function(all_auxfunction_names) {
    all_auxfunction_texts <-
      all_auxfunction_names |>
      purrr::map(\(one_f_name) {
        # browser()
        function_list <- utils::getAnywhere(one_f_name)

        function_as_text <-
          function_list$objs[[1]] |>
          function_obj_as_text() |>
          stringr::str_replace_all("\\\\", my_slash_replacement)

        with_first_line <-
          paste("\n", one_f_name, " <- ", function_as_text)

        return(with_first_line)
      }) |>
      rlang::set_names(all_auxfunction_names)

    # not used for now
    # all_auxfunction_texts_commented <-
    #   all_auxfunction_texts |>
    #   stringr::str_replace_all("\n", "\n# ")

    return(all_auxfunction_texts)
  }

all_auxfunction_texts <-
  get_all_auxfunction_texts(all_auxfunction_names)
# View(all_auxfunction_texts)

## get all my used function helps ----
get_all_auxfunction_helps <-
  function(all_auxfunction_names) {
    all_auxfunction_helps <-
      all_auxfunction_names |>
      purrr::map(\(one_f_name) {
        get_help_text(one_f_name)
      }) |>
      rlang::set_names(all_auxfunction_names)
    return(all_auxfunction_helps)
  }

all_auxfunction_helps <-
  get_all_auxfunction_helps(all_auxfunction_names)

# glimpse(all_auxfunction_helps)

## Paste function code and description before it is used ----

# check
# grep("@@", flat_file_r_text, value = T)
# 0

# all_auxfunction_names[[3]]

# print("HERE: all_auxfunction_names")
# print(sort(all_auxfunction_names))

used_auxfunction_names <- c()
used_auxfunction_texts <- list()
new_short_fun_names_vector <- all_auxfunction_names
temp_text_w_auxfun <- list(flat_file_r_text)
old_fun_names <- c()

# Have to add the found function texts and search for the list of fun names again amongst them
repeat ({
  old_fun_names <- new_short_fun_names_vector
  
 # get function names 
  for (fun_name in new_short_fun_names_vector) {
    fun_found <-
      stringr::str_detect(unlist(temp_text_w_auxfun), fun_name)
    if (any(fun_found)) {
      used_auxfunction_names <- 
        c(used_auxfunction_names, fun_name) |> 
        unique()
    }
  }
  
  # browser()

  # new iteration
  new_short_fun_names_vector <-
    setdiff(new_short_fun_names_vector, used_auxfunction_names) |> 
    unique()

  if (length(new_short_fun_names_vector) == length(old_fun_names))
    break()
  # the end was reached...
  
   # get function texts
  used_auxfunction_texts <-
    get_all_auxfunction_texts(used_auxfunction_names)
  
  temp_text_w_auxfun <- 
    c(temp_text_w_auxfun, used_auxfunction_texts) |> 
    unique()
  
  temp_text_w_auxfun
})

temp_text_w_auxfun |> length() ==
temp_text_w_auxfun |> unique() |> length()

my_used_function_texts <- 
  temp_text_w_auxfun[2:length(temp_text_w_auxfun)]

names(my_used_function_texts) <- used_auxfunction_names

my_used_function_helps <- 
  get_all_auxfunction_helps(used_auxfunction_names)

#' Put texts and helps into the code
#' 

# Choose a return value in case of error
add_in_front <-
  function(current_function_name,
           to_replace_with,
           my_split_newline_char,
           one_line_text) {
    text_added_in_front <-
      str_glue(
        "MOVE it: _START_ {current_function_name} {to_replace_with} _END_ #'{my_split_newline_char} {one_line_text}{my_split_newline_char}\n"
      )
    
    return(text_added_in_front)
  }

replace_function_with_def <-
  function(one_line_text,
           current_function_name) {
    
    # browser()
    # idx <- 10
    # current_function_name <- auxfunction_names[[idx]]
    
    print(str_glue("{current_function_name}"))
    
    # add parenthesis for back reference
    to_find_function_name <- str_glue("(",
                        my_split_newline_char,
                        ".+{current_function_name}\\b)")
    
    to_replace_with <-
      paste(
        "\n# <<<<",
        "\n# Explanations for the following code:",
        all_auxfunction_helps[[current_function_name]],
        all_auxfunction_texts[[current_function_name]],
        "# >>>>",
        # to keep in place what's found
        "\\1",
        sep = "\n"
      )
    
    tryCatch({
      # message("This is the 'try' part")
      
      one_line_text_replaced <-
        str_replace(one_line_text, to_find_function_name, to_replace_with)
      
      if (one_line_text_replaced == one_line_text) {
        # browser()
        one_line_text_replaced <- 
          add_in_front(current_function_name,
                     to_replace_with,
                     my_split_newline_char,
                     one_line_text)
        simpleError(str_glue("The function {current_function_name} as not added"))
      }
      
      return(one_line_text_replaced)
      
    }, error = function(cond) {
      
      message(paste("current_function_name:", current_function_name))
      message(paste("to_find:", to_find_function_name))
      message(paste("to_replace_with:", to_replace_with))
      
      message("Here's the original error message:")
      message(conditionMessage(cond))
      # Choose a return value in case of error
      text_added_in_front <-
          add_in_front(current_function_name,
                     to_replace_with,
                     my_split_newline_char,
                     one_line_text)
      
      return(text_added_in_front)
    }, warning = function(cond) {}, 
    finally = {
      # message("Some other message at the end")
    })
  }

one_line_text <-
  to_one_line(flat_file_r_text, my_split_newline_char)

# see_res_in_outfile(one_line_text)

length(one_line_text) == 1
# T

one_line_text_replaced <-
  purrr::reduce(used_auxfunction_names, \(acc, nxt)
                {
                  # browser()
                  replace_function_with_def(acc, nxt)
  }, .init = one_line_text)


text_replaced <-
  split_one_line_text_back(one_line_text_replaced)

length(text_replaced)
# 1218
# 2404 with auxf
# 2510 from all

# see_res_in_outfile(text_replaced)

# grep("fix_names", text_replaced, value = T)

# check
# grep(all_auxfunction_names[[23]],
#      text_replaced, value = T)

# To debug
# see_res_in_outfile(text_replaced)

# 

# Remove or comment out all "auxfunctions::" ----
text_replaced_1 <-
  text_replaced |>
  str_replace_all(
    "(install_helper_functions\\(\\))",
    str_glue("# Turn off commenting if you want to take advantage of this R package, for example, to see a function definition and help documentation the standard way (?function_name, F1, or F2).\n# \\1"
  ))
# 
length(text_replaced_1)
# 2531

# For now (rm when the code below works):
text_replaced_no_aux <-
  text_replaced_1 |>
  str_replace_all("auxfunctions::", "")

# rm auxfunctions:: in comments
# Check if the username is not "anna.shipunova"   if (!auxfunctions::get_username()

# Works:
# text_replaced_2 <-
#   gsub(
#     "^([^#]*)auxfunctions::(.+)(\\()",
#     "# Use function \\2 defined above.\n\\1\\2\\3",
#     text_replaced_1
#   )

text_replaced_2 <-
  gsub(
    "^([^#]*)auxfunctions::(.+?)\\b",
    "# Use function \\2 defined above.\n\\1\\2",
    text_replaced_1
  )

# see_res_in_outfile(text_replaced_2)

# gsub("^", "# Use function \\2 defined above.\n", test_text)

# test_text1 <-
#   grep("prepare_csv_full_path", text_replaced_2, value = T)

# text_replaced_2 <-
  # gsub(
  #   "(\n.*)auxfunctions::(.+)(\\()",
  #   "# Use function \\2 defined above.\n\\1\\2\\3",
  #   test_text1
  # )


# see_res_in_outfile(text_replaced_no_aux)

# convert to Rmd ----
# The 'knitr::spin' function is used to create an R Markdown (Rmd) file, but the 'knit' argument is set to 'FALSE', indicating that the document should not be fully knitted. Instead, this function generates an Rmd file from the R script without executing the code chunks.

# check
# identical(length(text_replaced_1), length(flat_file_r_text))

flat_file_r_text <- text_replaced_2
# flat_file_r_text <- text_replaced_no_aux

tictoc::tic("rmd_text")
rmd_text <-
  knitr::spin(text = flat_file_r_text,
              knit = FALSE,
              format = 'qmd')
tictoc::toc()
# rmd_text: 0.11 sec elapsed

# rmd_text |>
#   stringr::str_extract("````") |>
#   as.data.frame() |>
#   setNames(nm = "found") |>
#   filter(!is.na(found)) |> dim()
# # 86

# Change back to \s and \b in functions
# TODO: move it to where the text is the one line
rmd_text <-
  rmd_text |>
  # stringr::str_extract_all(".+QQQ.+") |>
  str_replace_all(my_slash_replacement, "\\\\")
  # unique()

# Don't use in the auxiliary file
maked_title <- 
  curent_project_name |> 
  stringr::str_replace_all("_", " ") |> 
  stringr::str_to_title()

pre_text <- stringr::str_glue("---
title: {maked_title}
---
")

# Don't use in the auxiliary file
# Setup
setup_text <- "

```{r no cache setup, results='hide', message=FALSE, warning=FALSE, cache=FALSE, include=FALSE}

## Quarto Setup

# Quarto enables you to weave together content and executable code into a finished document.
 
# Running Code
 
# The **Run** button allows you to run individual or bunch of chunks as a regular R script.
 
# When you click the **Render** button a document will be generated that includes both content and the output of embedded code.

# Load libraries required for Quatro
# A general-purpose tool for dynamic report generation in R
library(knitr)

# Adds features to a kable output
library(kableExtra)

# Format R code automatically
library(styler)
```

```{r df format setup}
#| include: false

# Customize the appearance of dataframes in HTML

# Uncomment if using tabs
# kable <- function(data) {
#   knitr::kable(data, booktabs = true, digits = 2) %>%
#     kable_styling('striped', full_width = FALSE)
# }

# Define a custom print function for data frames in knitr
knit_print.data.frame = function(x, ...) {
  res = paste(c(
    '',
    '',
    knitr::kable(x, digits = 2) |>
      kableExtra::kable_styling('striped', full_width = FALSE)
  ),
  collapse = '\n')
  knitr::asis_output(res)
}

# Register the custom print function for data frames in the knitr namespace
registerS3method(
  'knit_print', 'data.frame', knit_print.data.frame,
  envir = asNamespace('knitr')
)

# Set global chunk options in knitr if needed
# knitr::opts_chunk$set(echo = TRUE)

# Set the table format for knitr to HTML if needed
# options(knitr.table.format = 'HTML')

# End of Quarto setup

```

# save setup chunk options to use later
```{r setup current project, results='hide', message=FALSE, warning=FALSE}
```
"

# combine pieces into a Quarto file ----

# Don't use in the auxiliary file
cat(
  pre_text,
  file = file_paths$qmd,
  # append = TRUE,
  sep = "\n"
)

# ---
# add in front

# # tabset doesn't work with TOC
# cat(
#   '::: {.panel-tabset}',
#   file = file_paths$qmd,
#   append = TRUE,
#   sep = "\n"
# )

# Don't use in the auxiliary file
cat(
  setup_text,
  file = file_paths$qmd,
  append = TRUE,
  sep = "\n"
)

cat(
  rmd_text,
  file = file_paths$qmd,
  append = TRUE,
  sep = "\n"
)

# # for tabset only
# cat(
#   ':::',
#   file = file_paths$qmd,
#   append = TRUE,
#   sep = "\n"
# )
