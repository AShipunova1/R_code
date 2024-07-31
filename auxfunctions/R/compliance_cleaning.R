compliance_cleaning <-
function(compl_arr) {
  # Initialize 'compl' as the input 'compl_arr'.
  # if it is just one df already, do nothing
  compl <- compl_arr

  # Clean the 'week' column by splitting it into three columns with proper classes: 'week_num' (week order number), 'week_start', and 'week_end'.
  compl_clean <-
    purrr::map(compl, clean_weeks)

  # Find a column name containing 'permit', 'group', and 'expiration' (permitgroupexpiration).
  permitgroupexpirations <-
    purrr::map(compl,
        \(x) {
          grep("permit.*group.*expiration",
               tolower(names(x)),
               value = TRUE)
        })

  # Change the classes of dates in the 'permitgroupexpiration' columns from character to POSIXct.
  compl_dates <-
    compl_clean |>
    purrr::imap(\(x, idx) {
      field_name <- permitgroupexpirations[[idx]]
      x |>
        dplyr::mutate({{field_name}} := as.POSIXct(dplyr::pull(x[field_name]),
                                            format = "%m/%d/%Y"))
      # auxfunctions::change_to_dates(x, permitgroupexpirations[[idx]], "%m/%d/%Y")
    })

  # Return the cleaned and processed compliance data.
  return(compl_dates)
}
