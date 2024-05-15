separate_permits_into_3_groups <-
function(my_df, permit_group_field_name = "permitgroup") {
    my_df %>%
      # Use 'mutate' to create a new column 'permit_sa_gom' with categories based on permit group
      dplyr::mutate(permit_sa_gom =
               dplyr::case_when(
                 # Check if 'permit_group_field_name' doesn't contain 'RCG', 'HRCG', 'CHG', or 'HCHG'; assign "sa_only" if true
                 !grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only",
                 # Check if 'permit_group_field_name' doesn't contain 'CDW', 'CHS', or 'SC'; assign "gom_only" if true
                 !grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only",
                 # For all other cases, assign "dual"
                 .default = "dual"
               )) %>%
      # Return the modified data frame
      return()
  }
