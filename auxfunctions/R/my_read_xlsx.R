my_read_xlsx <-
function(file_path, sheet_n, start_row = 1) {
  res_df <-
    openxlsx::read.xlsx(
      file_path,
      sheet_n,
      startRow = start_row,
      detectDates = TRUE,
      colNames = TRUE,
      sep.names = "_"
    ) |>
    clean_headers()

  return(res_df)
}
