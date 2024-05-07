change_to_dates <-
function(my_df, field_name, date_format = "") {
  # Convert the specified column ('field_name') in 'my_df' to POSIXct date format using 'as.POSIXct'
  # Within the mutate function, it uses pull to extract the column specified by 'field_name' and then applies as.POSIXct to convert the values in that column to POSIXct date format using the provided 'date_format'.

  # browser()
  if (date_format == "") {
    my_tryFormats = c(
      "%m/%d/%Y %I:%M%p",
      "%m/%d/%Y %I:%M %p",
      "%m/%d/%Y %R%OS",
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS",
      "%Y-%m-%d %H:%M",
      "%Y/%m/%d %H:%M",
      "%Y-%m-%d",
      "%Y/%m/%d"
    )
  }

  new_field_name <- str_glue("{field_name}_dttm")

  result_df <-
    my_df |>
    mutate(!!new_field_name := as.POSIXct(!!field_name,
                                      tryFormats = my_tryFormats,
                                      format = date_format))

  # Return the data frame with the specified column converted to dates
  return(result_df)
}
