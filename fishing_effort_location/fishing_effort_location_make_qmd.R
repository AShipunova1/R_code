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

tic("rmd_text")
rmd_text <-
  knitr::spin(text = flat_file_r_with_headers_text,
              knit = FALSE)
toc()

# In this code, the 'knitr::stitch_rmd' function is used to create an R Markdown (Rmd) document from the provided text content ('flat_file_r_with_headers'). The resulting Rmd document is then saved to the file path specified by 'rmd_file_name'.

# too slow
# tic("stitch_rmd")
# knitr::stitch_rmd(text = flat_file_r_with_headers_text,
                  # output = rmd_file_name)

# toc()

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
