filenames = c("FHIER_Compliance_22__02_24_23.csv",
              "FHIER_Compliance_23__02_24_23.csv")

## ---- get csv data into variables ----
csv_names_list <- prepare_csv_names(filenames)
# read all csv files
csv_contents <- load_csv_names(my_paths, csv_names_list)
# browser()
# unify headers, trim vesselofficialnumber, just in case
csvs_clean1 <- clean_all_csvs(csv_contents)
str(csvs_clean1)
# browser()
compl_clean <- compliance_cleaning(csvs_clean1)

# View(compl_clean)

## ---- get compliance error definitions ----

err_desc_filenames = c("Compliance_Error_Types_03_29_2023.csv")

err_desc_csv_contents <-
  load_csv_names(my_paths, err_desc_filenames)

err_desc_clean_headers_csv_content <-
  clean_headers(err_desc_csv_contents[[1]])
err_desc <-
  change_to_dates(err_desc_clean_headers_csv_content,
                  "last_updated",
                  "%m/%d/%Y %I:%M:%S %p")

## ---- get permit data from PIMS ----
permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

active_permits_from_pims_raw <-
  load_csv_names(my_paths, permit_names_list)
# View(active_permits_from_pims[[1]])
head(active_permits_from_pims_raw[[1]])

# clean_headers
active_permits_from_pims_temp1 <-
  active_permits_from_pims_raw[[1]] %>%
  clean_headers

# separate columns
active_permits_from_pims_temp2 <-
  active_permits_from_pims_temp1 %>%
  separate_wider_delim(permit__, "-",
                       names = c("permit_code", "permit_num"),
                       too_many = "merge"
  ) %>%
  separate_wider_regex(
    cols = vessel_or_dealer,
    patterns = c(
      vessel_official_number = "[A-Za-z0-9]+",
      " */* ",
      vessel_name = "[A-Za-z0-9]+"
    ),
    too_few = "align_start"
  )

active_permits_from_pims_temp2 %>%
  select(status_date) %>%
  arrange(desc(status_date)) %>% unique() %>% head()
# correct dates

# active_permits_from_pims <-
#   active_permits_from_pims_temp2 %>%
#   mutate(across(ends_with("_date"),
#                 ~ as.POSIXct(.,
#                              format = "%m/%d/%y")))

active_permits_from_pims_temp2 %>%
  select(status_date) %>%
  arrange(desc(status_date)) %>% unique() %>% tail()

ends_with_date_fields <-
  grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

# change_fields_arr_to_dates(active_permits_from_pims_temp2, ends_with_date_fields, "%m/%d/%y")

active_permits_from_pims_temp3a <-
  active_permits_from_pims_temp2 %>%
  change_to_dates("status_date", "%m/%d/%Y")
# correct

active_permits_from_pims_temp3b <-
  map(ends_with_date_fields,
         function(x) {
           active_permits_from_pims_temp2[[x]] <-
             as.Date(active_permits_from_pims_temp2[[x]], "%m/%d/%Y")
         })
# wrong format

active_permits_from_pims <-
  # my_df %>%
  # mutate(across(all_of(field_names_arr), aux_fun_for_dates, date_format)) %>%
change_fields_arr_to_dates(active_permits_from_pims_temp2, ends_with_date_fields, "%m/%d/%Y")

active_permits_from_pims %>%
  select(status_date) %>%
  arrange(desc(status_date)) %>% unique() %>% head()
# correct
View(active_permits_from_pims)
