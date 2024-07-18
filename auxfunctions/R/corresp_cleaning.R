corresp_cleaning <-
  function(corresp_df) {
    corresp_df_contact_cnts <- auxfunctions::add_count_contacts(corresp_df)
    
    createdon_field_name <-
      auxfunctions::find_col_name(corresp_df, "created", "on")[1]
    contactdate_field_name <-
      auxfunctions::find_col_name(corresp_df, "contact", "date")[1]
    
    corresp_df_contact_cnts <-
      auxfunctions::change_to_dates(corresp_df_contact_cnts, createdon_field_name)
    corresp_df_contact_cnts <-
      auxfunctions::change_to_dates(corresp_df_contact_cnts, contactdate_field_name)
    
    return(corresp_df_contact_cnts)
  }
