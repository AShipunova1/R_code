# Define a function to add contact frequency count to a dataframe
add_count_contacts <- function(all_data_df_clean) {
  # Find the column names for contact date and vessel number
  contactdate_field_name <- 
    auxfunctions::find_col_name(all_data_df_clean, "contact", "date")[1]
  vessel_id_field_name <- 
    auxfunctions::find_col_name(all_data_df_clean, "vessel", "number")[1]
  
  # Mutate a new column 'was_contacted' based on the presence of contact date
  # Convert the result to a factor with levels 'yes' and 'no'
  result_df <- all_data_df_clean %>%
    dplyr::mutate(was_contacted = dplyr::if_else(is.na(!!rlang::sym(contactdate_field_name)), "no", "yes")) %>%
  
    # Add count of contacts per vessel, considering whether the vessel was contacted or not
    dplyr::add_count(!!rlang::sym(vessel_id_field_name), 
              was_contacted, 
              name = "contact_freq")
  
  return(result_df)
}
