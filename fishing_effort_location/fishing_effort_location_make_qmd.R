source("~/R_code_github/useful_functions_module.r")
# my_paths <- set_work_dir()

dir_to_comb <- "~/R_code_github/fishing_effort_location"

rmd_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.R")

knitr::spin(rmd_file_name, knit = FALSE)

qmd_file_name <-
  file.path(dir_to_comb, "flat_file_heatmap.Rmd")

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

cat(
  pre_text,
  file = qmd_file_name,
  append = TRUE,
  sep = "\n"
)

# Setup
setup_text <- "
```{r no cache setup, results='hide', message=FALSE, warning=FALSE, cache=FALSE}
# setup ----
library(mapview)
library(knitr)
```
"

cat(
  pre_text,
  file = qmd_file_name,
  append = TRUE,
  sep = "\n"
)
