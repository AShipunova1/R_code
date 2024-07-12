corresp_cleaning <-
function(corresp_df) {

  # Add a new column 'contact_freq' with "yes" if there is a 'contactdate' (and "no" if not).
  # Group the data by 'vesselofficialnumber' and count how many "contacts" there are for each. Save in the "contact_freq" column.
  corresp_df_contact_cnts <- auxfunctions::add_count_contacts(corresp_df)

  # Find the column names for 'createdon' and 'contactdate'.
  createdon_field_name <-
    auxfunctions::find_col_name(corresp_df, "created", "on")[1]
  contactdate_field_name <-
    auxfunctions::find_col_name(corresp_df, "contact", "date")[1]

  # Change the data types of 'createdon' and 'contactdate' columns to POSIXct.
  corresp_df_contact_cnts <-
    auxfunctions::change_to_dates(corresp_df_contact_cnts,
                    createdon_field_name)
  corresp_df_contact_cnts <-
    auxfunctions::change_to_dates(corresp_df_contact_cnts,
                    contactdate_field_name)

  # Return the cleaned and processed correspondence data.
  return(corresp_df_contact_cnts)
}
