vessels_permits_id_clean <-
function(my_df) {
    # Create a new dataframe 'vessels_permits' by renaming two specific columns.
    vessels_permits <- my_df |>
        dplyr::rename("PERMIT_VESSEL_ID" = "QCSJ_C000000000300000") |>
        dplyr::rename("VESSEL_VESSEL_ID" = "QCSJ_C000000000300001")

    # Return the cleaned dataframe.
    return(vessels_permits)
}
