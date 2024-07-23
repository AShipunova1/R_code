clean_all_csvs <-
function(csvs, vessel_id_field_name = NA) {
  # Clean headers of all CSVs using the 'clean_headers' function
  csvs_clean0 <- lapply(csvs, auxfunctions::clean_headers)

  # Trim 'vesselofficialnumber' column (if specified) in all cleaned CSVs
  csvs_clean1 <-
    auxfunctions::trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)

  # Return the list of cleaned CSVs
  return(csvs_clean1)
}
