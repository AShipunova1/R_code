library(openxlsx)

getwd()
template_file_full_name <- "~/examples_and_manuals/results_template.xlsx"
template_file_name <- basename(template_file_full_name)

# create new:
# wb <- createWorkbook() 

# load existing:
wb <- loadWorkbook(template_file_full_name)
addWorksheet(wb, sheetName = "test1") 

writeData(wb, sheet = "test1", x = trees) 

saveWorkbook(wb, template_file_name, overwrite = T)


