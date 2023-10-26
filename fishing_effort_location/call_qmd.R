# call_qmd
library(knitr)
library(quarto)
options(knitr.duplicate.label = 'allow')
# options()
file_path <- "flat_file_heatmap.qmd"
file.exists(file_path)
# knitr::knit(file_path)
# works:
# rmarkdown::render(file_path, output_yaml = "../my_qmd_options.yml")
# err:
knitr::opts_knit$set(knitr.duplicate.label = 'allow')
knitr::opts_knit$set(knitr.progress.linenums = TRUE)

knitr::opts_knit$set(global.par = TRUE)

quarto_render(file_path) # all formats

