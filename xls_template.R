library(openxlsx)

create_new_workbook <- function() {
  wb <- createWorkbook()
  wb
}

load_exist_workbook <- function(template_file_full_name) {
  wb <- loadWorkbook(template_file_full_name)
  wb
}

add_data_to_new_sheet <- function(workbook_name, new_sheetName, my_data) {
  addWorksheet(workbook_name, sheetName = new_sheetName) 
  
  writeData(workbook_name, sheet = new_sheetName, x = my_data) 
}

save_workbook <- function(workbook_name, template_file_full_name) {
  template_file_name <- basename(template_file_full_name)
  saveWorkbook(workbook_name, template_file_name, overwrite = TRUE)
}

# __main__
getwd()

