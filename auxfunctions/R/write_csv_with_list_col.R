#' Write a df with List Columns to a csv
#'
#' @param my_df 
#' @param csv_out_file_path
#'
write_csv_with_list_col <- function(my_df, csv_out_file_path) {
  my_df |>
    rowwise() |>
    dplyr::mutate_if(is.list, ~ paste(unlist(.), collapse = ', ')) |>
    readr::write_csv(csv_out_file_path)
}
