source("~/R_code_github/useful_functions_module.r")

# get csvs ----

base_path <- getwd()

corrected_path <-
  file.path(
    base_path,
    r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation__2023-01-24_to_2023-08-01_c.csv)"
  )

file.exists(corrected_path)

corrected_csv <-
  readr::read_csv(
    corrected_path,
    name_repair = fix_names,
    col_types = cols(.default = 'c'),
    skip = 1
  ) |>
  dplyr::distinct()

# "R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv"

aug_9_res_path <-
  file.path(base_path,
            r"(../../R_files_local\my_outputs\egregious_violators\egregious_violators_for_investigation_from_2023-02-07_to_2023-08-15_addr.csv)")

file.exists(aug_9_res_path)
# T

aug_9_csv <-
  readr::read_csv(aug_9_res_path,
                  name_repair = fix_names,
                  col_types = cols(.default = 'c'))

# FHIER info from Reports \ SERO Permits
""

fhier_info_path <-
  file.path(base_path,
            r"(../../\R_files_local\my_inputs\from_Fhier\SERO Permits.csv)")

file.exists(fhier_info_path)
# T

fhier_info <-
  readr::read_csv(fhier_info_path,
                  name_repair = fix_names,
                  col_types = cols(.default = 'c'))

# View(fhier_info)
# [1] 138557     19

# shorten ----
fields_to_use <-
  c(
    "vessel_official_number",
    "name",
    "contactrecipientname",
    "contactphone_number",
    "contactemailaddress",
    "sero_home_port",
    "state",
    "division",
    "full_name",
    "full_address"
  )


corrected_csv1 <-
  corrected_csv |>
  dplyr::select(all_of(fields_to_use)) |>
  dplyr::distinct()

short_dfs <-
  list(aug_9_csv,
       corrected_csv) |>
  purrr::map( ~ .x |>
         dplyr::select(any_of(fields_to_use)) |>
         dplyr::distinct())

map(short_dfs,
       dim)
# [1] 97 10
#
# [[2]]
# [1] 117   8

# compare files ----

csvs_joined <-
  full_join(
    short_dfs[[1]],
    short_dfs[[2]],
    join_by(vessel_official_number,
            name),
    # Override the default suffixes, c(".x", ".y") in not merged cols
    suffix = c(".aug", ".cor")
  )

csvs_joined_diff <-
  csvs_joined |>
  dplyr::filter(
    !sero_home_port.aug == sero_home_port.cor |
      !full_name.aug == full_name.cor |
      !full_address.aug == full_address.cor
  )

dim(csvs_joined_diff)
# [1] 57 16

# View(csvs_joined_diff)

# compare fhier_info with corrected
# print_df_names(fhier_info)
corrected_n_fhier <-
  left_join(short_dfs[[2]],
            fhier_info,
            join_by(vessel_official_number == vessel_id))

# print_df_names(corrected_n_fhier)
# [1] 1659   26


to_rm <- c(
  "entity",
  "accspparticipant",
  "expiration_date",
  "fishery",
  "permit__",
  "effective_date",
  "end_date",
  "permit_status"
)

# grep("email", names(corrected_n_fhier), value = T)

corrected_n_fhier_short <-
  corrected_n_fhier |>
  dplyr::select(-all_of(to_rm)) |>
  dplyr::distinct()

out_path <-
    file.path(base_path,
            r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\compare_corrected_n_fhier_info.csv)")

readr::write_csv(corrected_n_fhier_short,
                 out_path)
# [1] 153  18

# check sero_home_port ----
corrected_n_fhier_short |>
  dplyr::mutate(fhier_home_port = paste(sero_home_port_city,
                                 sero_home_port_county,
                                 sero_home_port_state)) |>
  dplyr::filter(!sero_home_port == fhier_home_port) |>
  dim()
# 0
# all home ports are correct

# check names ----
# corrected_n_fhier_short |>
#   dplyr::mutate(first_name = replace_na(first_name, ""),
#                 last_name = replace_na(last_name, ""),
#                 business_name = replace_na(business_name, "")) |>

# print_df_names(corrected_n_fhier_short)

corrected_n_fhier_short_w_n <-
  corrected_n_fhier_short |>
  dplyr::mutate(
    first_name = replace_na(first_name, ""),
    last_name = replace_na(last_name, ""),
    business_name = replace_na(business_name, "")
  )

corrected_n_fhier_short_w_n_comb <-
  corrected_n_fhier_short_w_n |>
  # for each vessel
  dplyr::group_by(vessel_official_number) |>
  dplyr::mutate(fhier_comb_name =
                  list(unique(paste(
                    first_name,
                    last_name
                  ))),
  fhier_comb_name_2 = list(unique(
    paste(fhier_comb_name,
          business_name)
  ))) |>
  dplyr::ungroup() |>
  dplyr::select(
    vessel_official_number,
    contactrecipientname,
    full_name,
    fhier_comb_name,
    fhier_comb_name_2
  ) |>
  # dplyr::select(-c(first_name,
  #         last_name,
  #         business_name)) |>
  dplyr::distinct()

dim(corrected_n_fhier_short_w_n_comb)
# [1] 153  18
# [1] 115   4

# convert lists in comma separated strings
corrected_n_fhier_short_w_n_comb_flat <-
  corrected_n_fhier_short_w_n_comb |>
  dplyr::rowwise() |>
  dplyr::mutate_if(is.list,
                   ~ paste(unlist(.),
                           collapse = ', ')) %>%
  # back to colwise
  dplyr::ungroup()

dim(corrected_n_fhier_short_w_n_comb_flat)
# 115

# clean up weird comma and space combinations
corrected_n_fhier_short_w_n_comb_flat_clean <-
  corrected_n_fhier_short_w_n_comb_flat |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove whitespace at the start and end, and replaces all internal whitespace with a single space.
    ~ stringr::str_squish(.x)
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove space characters before commas
    ~ stringr::str_replace_all(.x, "\\s+,", ",")
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # replace 2+ commas with one
    ~ stringr::str_replace_all(.x, ",,+", ",")
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove whitespace at the start and end, and replaces all internal whitespace with a single space.
    ~ stringr::str_squish(.x)
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove commas at the end
    ~ stringr::str_replace_all(.x, ",$", "")
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove commas in front
    ~ stringr::str_replace_all(.x, "^,", "")
  )) |>
  dplyr::mutate(dplyr::across(
    c(
      contactrecipientname,
      full_name,
      fhier_comb_name,
      fhier_comb_name_2
    ),
    # remove whitespace at the start and end, and replaces all internal whitespace with a single space.
    ~ stringr::str_squish(.x)
  ))


# View(corrected_n_fhier_short_w_n_clean)
  # dplyr::filter(full_name == fhier_name) |>
# 26
  # dplyr::filter(full_name == fhier_name_2) |>
  # dim()
# 0

# View(corrected_n_fhier_short_w_n_clean)

readr::write_csv(corrected_n_fhier_short_w_n_clean,
                 out_path)

# compare
corrected_n_fhier_short_w_n_clean |>
  dplyr::filter(full_name == fhier_name) |>
  dim()
# 26

corrected_n_fhier_short_w_n_clean |>
  dplyr::filter(!full_name == fhier_name) |>
  # dplyr::filter(full_name == fhier_name_2) |>
  dim()
# 0
# 75

corrected_n_fhier_short_w_n_clean_short <-
  corrected_n_fhier_short_w_n_clean |>
  dplyr::filter(!full_name == fhier_name) |>
  # dplyr::filter(!fhier_name_2 == fhier_name) |>
  dplyr::select(vessel_official_number,
         # contactrecipientname,
         full_name,
         fhier_name,
         fhier_name_2) |>
  dplyr::distinct()
# |>

# View(corrected_n_fhier_short_w_n_clean_short)

# read PIMS csv ----
# https://www.fisheries.noaa.gov/southeast/resources-fishing/frequent-freedom-information-act-requests-southeast-region

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\from_Fhier\FOIA+Vessels+All.csv"

all_permits_url <- "https://noaa-sero.s3.amazonaws.com/drop-files/pims/FOIA+Vessels+All.csv"
  
all_permits_from_web <- read_csv(all_permits_url)

glimpse(all_permits_from_web)

# short all_permits_from_web ----
# print_df_names(all_permits_from_web)
all_permits_from_web_short <-
  all_permits_from_web |>
  dplyr::select(
    -c(
      REQMIT_ID,
      SER_ID,
      PERMIT_STATUS_LOOKUP_ID,
      TRANSACTION_TYPE_LOOKUP_ID,
      # OFFICIAL_NUMBER,
      # VESSEL_NAME,
      # HAILING_PORT_CITY,
      HAILING_PORT_STATE_LOOKUP_ID,
      # VESSEL_STATE,
      ENTITY_ID,
      # ENTITY_NAME,
      ADDRESS_TYPE_LOOKUP_ID,
      # ADDRESS,
      # CITY,
      # POSTAL_CODE,
      STATE_LOOKUP_ID,
      # ADDRESS_STATE,
      FISHERY_TYPE_ID,
      PERMIT_EFFECTIVE_DATE,
      PERMIT_EXPIRATION_DATE,
      PERMIT_TERMINATION_DATE,
      PERMIT_END_DATE,
      FISHERY_NAME_ABBR
    )
  ) |>
  dplyr::distinct()
dim(all_permits_from_web)
# [1] 1000   23

dim(all_permits_from_web_short)
# [1] 463   9

# compare pims and corrected_csv1 ----
glimpse(corrected_csv1)

pims_n_corrected <-
  left_join(corrected_csv1,
             all_permits_from_web_short,
             join_by(vessel_official_number == OFFICIAL_NUMBER))

pims_n_corrected |> 
  dim()
# [1] 97 18

pims_n_corrected |> 
 dplyr::filter(!is.na(HAILING_PORT_CITY)) |> 
 dim()
# [1] 13 18
# Not all vsls are in the pims csv

## check if the port is the same ----
pims_n_corrected |>
  dplyr::filter(!is.na(HAILING_PORT_CITY)) |>
  rowwise() |> 
  dplyr::filter(grepl(HAILING_PORT_CITY,
               sero_home_port,
               ignore.case = TRUE)) |>
  dplyr::ungroup() |> View()
  dim()
# 13
# all 13 ports are the same
  
## check if the address is the same ----
pims_n_corrected_addr <-
  pims_n_corrected |>
  dplyr::filter(!is.na(HAILING_PORT_CITY)) |>
  rowwise() |> 
  dplyr::filter(grepl(ADDRESS,
               full_address,
               ignore.case = TRUE)) |>
  dplyr::ungroup()
  
dim(pims_n_corrected_addr)
# 12 (one is the full address is.na)
# out of 13
  
pims_n_corrected_addr |>
  dplyr::select(vessel_official_number,
         full_address,
         ADDRESS, 
         ADDRESS_STATE) |> 
  dplyr::distinct() |> 
  View()

## check if the name is the same ----
include_name_filter <-
  rlang::quo(grepl(ENTITY_NAME,
               full_name,
               ignore.case = TRUE))
  
pims_n_corrected_name <-
  pims_n_corrected |>
  rowwise() |>
  dplyr::filter(!!include_name_filter) |>
  dplyr::ungroup()

dim(pims_n_corrected_name)
# 7
# out of 13
  
pims_n_corrected_name |>
  dplyr::select(vessel_official_number,
         contactrecipientname,
         full_name,
         ENTITY_NAME) |> 
  View()

pims_n_corrected |>
  dplyr::select(vessel_official_number,
         contactrecipientname,
         full_name,
         ENTITY_NAME) |>
  rowwise() |>
  dplyr::filter(!(!!include_name_filter)) |>
  dplyr::ungroup() |>
  View()
  dim()
# 6

# agrepl(
#   pattern,
#   x,
#   max.distance = 0.1,
#   costs = NULL,
#   ignore.case = FALSE,
#   fixed = TRUE,
#   useBytes = FALSE
# )

pims_n_corrected |>
  dplyr::select(vessel_official_number,
         contactrecipientname,
         full_name,
         ENTITY_NAME) |>
  dplyr::filter(!is.na(ENTITY_NAME)) |> 
  rowwise() |>
  dplyr::filter(agrepl(ENTITY_NAME,
                full_name,
                max.distance = 0.3,
                ignore.case = TRUE)) |>
  dplyr::ungroup() |>
  dim()
# [1] 13  4
# all the same
# ?adist


out_path1 <-
    file.path(base_path,
            r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\compare_corrected_and_pims_info.csv)")

readr::write_csv(pims_n_corrected,
                 out_path1)
