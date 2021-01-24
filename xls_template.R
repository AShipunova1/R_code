library(openxlsx)

getwd()
template_file_full_name <- "~/examples_and_manuals/results_template.xlsx"
template_file_name <- basename(template_file_full_name)

wb <- loadWorkbook(template_file_full_name)
addWorksheet(wb, sheetName = "test1") 
# write.xlsx(df$sheet1, file = template_file_name, sheetName="sh1", append=TRUE)
write.xlsx(trees, template_file_name, sheetName="New_Sheet", append=TRUE)

writeData(wb, sheet = "test1", x = trees) 

# writeData(wb, sheet = "test1", Rev_4, colNames = F)
saveWorkbook(wb, template_file_name, overwrite = T) #, overwrite = T

# wb <- createWorkbook() 
# addWorksheet(wb, sheetName = "test1") 
# writeData(wb, sheet = "test1", x = trees) 
# addWorksheet(wb, sheetName = "test2") 
# writeData(wb, sheet = "test2", x = trees)
# saveWorkbook(wb, "test.xlsx")
