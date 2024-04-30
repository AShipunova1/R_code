## ---- find if not compliant and no correspondence ----
not_compliant_51_plus_weeks_and_no_correspondence <-
  setdiff(id_52_plus_weeks$vessel_official_number, corresp_contact_cnts_clean$vessel_official_number)

str(not_compliant_51_plus_weeks_and_no_correspondence)
# 15

# write.csv(not_compliant_51_plus_weeks_and_no_correspondence, file.path(my_paths$outputs, "not_compliant__no_calls", "not_compliant_51_plus_weeks_and_no_correspondence.csv"), row.names = FALSE)

# To use as a filter in FHIER/Correspondence
cat(not_compliant_51_plus_weeks_and_no_correspondence, 
    sep = ', ', 
    file = file.path(my_paths$outputs, 
                     "not_compliant__no_calls",
                     "not_compliant_51_plus_weeks_and_no_correspondence.txt"))

