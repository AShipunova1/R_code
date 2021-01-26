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
template_file_full_name <- "~/examples_and_manuals/results_template.xlsx"
wb <- load_exist_workbook(template_file_full_name)
q_mass_r <- "select 
YEAR,
MONTH,
NEGEAR,
target_comnames TARGSPEC1_2,
COMNAME,
anmlcond,
GIS_LATHBEG,
GIS_LONHBEG,
GIS_LATHEND,
GIS_LONHEND
FROM
    REQUEST_INC_ALL
    JOIN LAT_LON_DATA_RESULT_MASS_R ON ( ROUND(gis_lathbeg,6) = ROUND(lat, 6)
                                      AND ROUND(gis_lonhbeg, 6) = ROUND(lon, 6) )
                                    OR ( ROUND(gis_lathend, 6) = ROUND(lat, 6)
                                         AND ROUND(gis_lonhend, 6) = ROUND(lon, 6) );"
mass_r_res <- dbGetQuery(con_nova, q_mass_r)
new_sheetName <- "mass_restr1"
add_data_to_new_sheet(wb, new_sheetName, mass_r_res)

q_great_s_ch_r <- "select 
YEAR,
MONTH,
NEGEAR,
target_comnames TARGSPEC1_2,
COMNAME,
anmlcond,
GIS_LATHBEG,
GIS_LONHBEG,
GIS_LATHEND,
GIS_LONHEND
FROM
    REQUEST_INC_ALL
    JOIN lat_lon_data_result_great_s_ch ON ( ROUND(gis_lathbeg,6) = ROUND(lat, 6)
                                      AND ROUND(gis_lonhbeg, 6) = ROUND(lon, 6) )
                                    OR ( ROUND(gis_lathend, 6) = ROUND(lat, 6)
                                         AND ROUND(gis_lonhend, 6) = ROUND(lon, 6) );"

great_s_ch_r_res <- dbGetQuery(con_nova, q_great_s_ch_r)
new_sheetName <- "great_s_ch_r"
add_data_to_new_sheet(wb, new_sheetName, great_s_ch_r_res)
save_workbook(wb, template_file_full_name)