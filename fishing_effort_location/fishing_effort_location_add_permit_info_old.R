# add permit info ----
## prepare permit info ----
get_permit_data_from_PIMS_csv <- function() {

  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9 ]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

permits_from_pims <- get_permit_data_from_PIMS_csv()
dim(permits_from_pims)
# [1] 23900    13

### keep only permits not expired before 2022 - FILTER ----
permits_from_pims_active <-
  permits_from_pims |>
  filter(expiration_date > '2022-01-01' |
           end_date > '2022-01-01')

dim(permits_from_pims_active)
# [1] 17141    13

## add permits to coordinates ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits <-
  left_join(
    safis_efforts_extended_2022_short_good_sf_crop_big_df_in_metricks,
    permits_from_pims_active,
    join_by(VESSEL_OFFICIAL_NBR == vessel_official_number),
    relationship = "many-to-many"
  )

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits)
# [1] 284785     29
# [1] 282522     29

# check status ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  select(status) |>
  distinct()
# # A tibble: 6 × 1
#   status
#   <chr>
# 1 Mailed
# 2 NA
# 3 Vessel Leased
# 4 Created
# 5 Renewed

# add permit region ----
safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom <-
  safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits |>
  separate_permits_into_3_groups(permit_group_field_name = "permit_code")

dim(safis_efforts_extended_2022_short_good_sf_crop_big_short_df_permits_sa_gom)
# [1] 284785     30
# [1] 282522     30
