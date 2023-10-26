# call_qmd
library(knitr)
options(knitr.duplicate.label = 'allow')
options()
file_path <- "flat_file_heatmap.qmd"
file.exists(file_path)
# knitr::knit(file_path)
rmarkdown::render(file_path, output_yaml = "../my_qmd_options.yml")
