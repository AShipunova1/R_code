# setup ----
source("~/R_code_github/useful_functions_module.r")
# my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/fishing_effort_location"

r_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")

rmd_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.Rmd")

qmd_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.qmd")

# Add headers to the flat file to be converted by knitr ----
flat_file_r_with_headers_text <-
  gsub("^(#+ )(.+)(----)",
       "#' \\1\\2\\n\\1\\2\\3",
       readLines(r_file_name))

# glimpse(flat_file_r_with_headers)
 # chr [1:11952] "#' ## Current file: useful_functions_module.r" "" "" ...

# convert to Rmd ----
# The 'knitr::spin' function is used to create an R Markdown (Rmd) file, but the 'knit' argument is set to 'FALSE', indicating that the document should not be fully knitted. Instead, this function generates an Rmd file from the R script without executing the code chunks.
knitr::spin(r_file_name, knit = FALSE)

# prepare all pieces ----

## read the new file ----
# Read the lines of text from the specified file into a character vector called 'rmd_contents'.
rmd_contents <- readLines(rmd_file_name)

pre_text <- '
---
title: "Fishing effort locations heatmap"
date: today
# project:
#   lib-dir: ..
output:
  html_document
execute:
  warning: false
  cache: true
format:
  html:
    toc: true
    toc-depth: 2
    css: style.css
    code-overflow: wrap
    code-fold: true
    code-summary: "Show the code"
    code-line-numbers: true
---
'

# Setup
setup_text <- "
```{r no cache setup, results='hide', message=FALSE, warning=FALSE, cache=FALSE}
# setup ----
library(mapview)
library(knitr)
```
"

# combine pieces into a Quarto file ----

cat(
  pre_text,
  file = qmd_file_name,
  # append = TRUE,
  sep = "\n"
)

cat(
  setup_text,
  file = qmd_file_name,
  append = TRUE,
  sep = "\n"
)

cat(
  rmd_contents,
  file = qmd_file_name,
  append = TRUE,
  sep = "\n"
)
