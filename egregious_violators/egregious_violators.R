# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
current_project_name <- "egregious_violators"
current_project_path <-
  file.path(my_paths$git_r, current_project_name)

source(file.path(current_project_path, "get_data_egregious_violators.R"))

## check ----
check_new_vessels <-
  function(my_df) {
    list_to_check <-
      c("FL1848EY",
        "FL4232JY",
        "1246468",
        "FL7549EJ")
    my_df |>
      dplyr::filter(vessel_official_number %in% list_to_check) |>
      dplyr::select(vessel_official_number) |>
      dplyr::distinct() |>
      dim() %>%
      return()
  }


# ---- Preparing compliance info ----

## ---- add permit_expired column ----
check_new_vessels(compl_clean)
# 4

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is more than a month from data_file_date than "no"
  dplyr::mutate(permit_expired = dplyr::case_when(permitgroupexpiration > (data_file_date + 30) ~ "no",
                                    .default = "yes"))

## ---- add year_month column ----

number_of_weeks_for_non_compliancy = 27
days_in_27_weeks <- number_of_weeks_for_non_compliancy*7

half_year_ago <-
  data_file_date - days_in_27_weeks

compl_clean_w_permit_exp_last_27w <-
  compl_clean_w_permit_exp |>
  dplyr::mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last 28 weeks
  dplyr::filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)
# today()
# [1] "2023-08-10"
# [1] 235509     22

check_new_vessels(compl_clean_w_permit_exp)
# 4

# 185538
# [1] 217772     22
dim(compl_clean_w_permit_exp_last_27w)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d
# [1] 87826    23
# [1] 74169    23

check_new_vessels(compl_clean_w_permit_exp_last_27w)
# 4

## ---- Have only SA permits, exclude those with Gulf permits ----

compl_clean_sa <-
  compl_clean_w_permit_exp_last_27w |>
  dplyr::filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"

## Not "compliant_" only ----
compl_clean_sa_non_compl <-
  compl_clean_sa |>
  dplyr::filter(compliant_ == 'NO')

check_new_vessels(compl_clean_sa_non_compl)
# 4

dim(compl_clean_sa_non_compl)
# [1] 18205    23
# [1] 11473    23
# [1] 10597    23
# [1] 12484    23
# [1] 15549    23
# [1] 13992    23
# [1] 14204    23
# [1] 12454    23

compl_clean_sa_non_compl |>
  count_uniq_by_column() |> head(1)
# vesselofficialnumber 1785
# today()
# "2023-06-23"
# vessel_official_number 1573
# [1] "2023-07-10"
# vessel_official_number 1403
# vessel_official_number 1369
# [1] "2023-08-01"
# vessel_official_number 1370
# vessel_official_number 1328

## dplyr::filter for egregious ----
### check if there is no "compliant_ == YES" since half_year_ago ----

last_week_start <- data_file_date - 6

compl_clean_sa |> check_new_vessels()
# 4

compl_clean_sa_non_c_not_exp <-
  compl_clean_sa |>
  # not compliant
  dplyr::filter(tolower(compliant_) == "no") |>
  # in the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_start < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

dim(compl_clean_sa_non_c_not_exp)
# [1] 10419    23
# [1] 9486   23
# [1] 9315   23

compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

compl_clean_sa_all_weeks_non_c_short <-
  compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  dplyr::arrange(dplyr::desc(compl_weeks_amnt),
                 vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # all weeks were...
  dplyr::filter(total_weeks >= (number_of_weeks_for_non_compliancy - 3)) |>
  # ...non compliant
  dplyr::filter(compl_weeks_amnt == total_weeks)

dim(compl_clean_sa_all_weeks_non_c_short)

compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  # dim()
  # [1] 9486    5
  # [1] 9315    5
  dplyr::arrange(dplyr::desc(compl_weeks_amnt), vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # dim()
  # [1] 1045    4
  # all weeks were non compliant
  # dplyr::filter(compl_weeks_amnt == total_weeks) |>
    dplyr::glimpse()

dim(compl_clean_sa_all_weeks_non_c_short)
# 121

### add back columns needed for the output ----
need_cols_names <- c(
  "vessel_official_number",
  "name",
  "permit_expired",
  "permitgroup",
  "permitgroupexpiration"
  # ,
  # "week_start"
)
compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

# dim(compl_clean_sa_non_c_not_exp)
compl_clean_sa_all_weeks_non_c <-
  compl_clean_sa_non_c_not_exp |>
  dplyr::select(all_of(need_cols_names)) |>
  inner_join(compl_clean_sa_all_weeks_non_c_short) |>
# Joining with `by = join_by(vessel_official_number)`
  dplyr::distinct()

dim(compl_clean_sa_all_weeks_non_c)
# [1] 130   8
# 0
# 127
# 121

## check the last report date ----
# ids only
compl_clean_sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_sa_all_weeks_non_c_short |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

dim(compl_clean_sa_all_weeks_non_c_short_vesl_ids)
# [1] 128   1

# check these ids in the full compliance information
compl_clean_sa |>
  dplyr::filter(
    vessel_official_number %in% compl_clean_sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  dplyr::group_by(vessel_official_number) |>
  dplyr::filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  # get only the latest compliant weeks
  dplyr::mutate(latest_compl = max(week_num)) |>
  dplyr::filter(week_num == latest_compl) |> 
  dplyr::ungroup() |> 
  dplyr::select(
    # vessel_official_number,
    year_month,
    latest_compl) |>
  dplyr::distinct() |> 
  dplyr::glimpse()
# $ year_month   <yearmon> Jul 2023
# $ latest_compl <int> 31

# TODO: add check for earlier weeks

## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  dplyr::filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean) |>
  head(1)
# vesselofficial_number   3223
# vessel_official_number  3371
# vesselofficial_number 3434

# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

two_attempts_filter <-
  quo(contact_freq > 1 &
        any(tolower(contacttype) == "call"))

corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  dplyr::filter(!!two_attempts_filter)

test_new_egr2 <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()

test_new_egr2[1] == 4
# T

dim(corresp_contact_cnts_clean)
# [1] 18629    23
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

data_overview(corresp_contact_cnts_clean_direct_cnt_2atmps) |>
  head(1)
# vesselofficial_number 2968
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

## ---- Combine compliance information with dplyr::filtered correspondence info by vesselofficialnumber ----

corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()
# 4

compl_clean_sa_all_weeks_non_c |>
  check_new_vessels()
# 2

compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps,
    compl_clean_sa_all_weeks_non_c,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation1)
# [1] 486  30
# [1] 522  30

# check
count_uniq_by_column(compl_corr_to_investigation1) |>
  head(1)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105
# 108
# 97
# vesselofficial_number 116

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# # put names into vars
# contactdate_field_name <-
#   find_col_name(compl_corr_to_investigation1, "contact", "date")[1]
# contacttype_field_name <-
#   find_col_name(compl_corr_to_investigation1, "contact", "type")[1]
# 
# write.csv(compl_corr_to_investigation1,
#           file.path(
#             my_paths$outputs,
#             paste0(              "more_than_24_compl_corr_to_investigation1_22_23__",
#               today(),
#               ".csv"
#             )
#           ),
#           row.names = FALSE)
# # 435 dplyr::distinct ids

get_date_contacttype <-
  function(compl_corr_to_investigation1) {
    compl_corr_to_investigation1 |>
      # add a new column date__contacttype with contactdate and contacttype
      dplyr::mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) |>
      # use 2 columns only
      dplyr::select(vessel_official_number, date__contacttype) |>
      # [1] 49903     2
      # sort
      dplyr::arrange(vessel_official_number, date__contacttype) |>
      dplyr::distinct() |>
      dplyr::group_by(vessel_official_number) |>
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = paste(date__contacttype, collapse = ", ")) %>%
      return()
  }

date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation1)
dim(date__contacttype_per_id)
# [1] 110    2
# 107
# 27: 177
# 188   2
# 105   2 (the new dplyr::filter)
# 108
# 97
# [1] 116   2 (2 contact attempts)

compl_corr_to_investigation1 |>
  check_new_vessels()
# 2

date__contacttype_per_id |>
  check_new_vessels()
# 2

# add permit and address info ----
# print_df_names(vessels_permits_participants)

### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants |> 
  dplyr::select(P_VESSEL_ID) |> 
  dplyr::distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1

setdiff(date__contacttype_per_id$vessel_official_number,
        vessels_permits_participants_v_ids$P_VESSEL_ID
) |> cat(sep = "', '")
# |> 
#   length()
# 6
# '1305388', '565041', 'FL0001TG', 'MI9152BZ', 'NC2851DH', 'VA1267CJ' 
# (wrong license_nbr in full_participants
# or entity_id in permits,
# check manually)

# setdiff(vessels_permits_participants_v_ids$P_VESSEL_ID,
#         date__contacttype_per_id$vessel_official_number
# ) |> 
#   length()
# 3185

vessels_permits_participants_space <-
  vessels_permits_participants |>
  dplyr::mutate(across(where(is.character),
                ~ replace_na(., ""))) |>
  dplyr::mutate(across(where(is.character),
                ~ str_trim(.)))

dim(vessels_permits_participants_space)
# [1] 31942    38

vessels_permits_participants_short_u <-
  vessels_permits_participants_space |>
  dplyr::group_by(P_VESSEL_ID) |>
  dplyr::mutate(
    sero_home_port = list(unique(
      paste(
        SERO_HOME_PORT_CITY,
        SERO_HOME_PORT_COUNTY,
        SERO_HOME_PORT_STATE
      )
    )),
    full_name = list(unique(
      paste(FIRST_NAME,
            MIDDLE_NAME,
            LAST_NAME,
            NAME_SUFFIX)
    )),
    full_address = list(unique(
        paste(ADDRESS_1,
              ADDRESS_2,
              STATE,
              POSTAL_CODE)
      ))
  ) |>
  dplyr::select(P_VESSEL_ID,
         sero_home_port,
         full_name,
         full_address) |>
  dplyr::ungroup() |>
  dplyr::distinct()

# dim(vessels_permits_participants_short)
# [1] 7858    4
# [1] 3302    4

# View(vessels_permits_participants_short_u)

# vessels_permits_participants_short_u |> 
#   # dplyr::filter(lengths(full_name) > 0) %>%
#   # unnest(full_name) %>%
#   # unnest_wider(full_name, names_sep = "_") |> 
#   rowwise() |> 
#   dplyr::mutate_if(is.list, ~paste(unlist(.), collapse = ', ')) %>% 
#   View()
 # cat()

vessels_permits_participants_short_u_flat <-
  vessels_permits_participants_short_u |>
  rowwise() |>
  dplyr::mutate_if(is.list, ~ paste(unlist(.), collapse = ', ')) %>%
  dplyr::ungroup()

data_overview(vessels_permits_participants_short_u_flat) |> 
  head(1)
# P_VESSEL_ID 3302

vessels_permits_participants_short_u_flat_sp <-
  vessels_permits_participants_short_u_flat |>
  # gdf %>% dplyr::mutate(across(v1:v2, ~ .x + n))
  dplyr::mutate(
    across(
    c(sero_home_port,
      full_name,
      full_address),
    ~ str_trim(.x)
  ),
    across(
    c(sero_home_port,
      full_name,
      full_address),
    ~ str_replace_all(.x, "\\s+,", ",")
  ),
  across(
    c(sero_home_port,
      full_name,
      full_address),
    ~ str_replace_all(.x, ",,+", ",")
  ),
  across(
    c(sero_home_port,
      full_name,
      full_address),
    ~ str_replace_all(.x, ",$", "")
  ),
    across(
    c(sero_home_port,
      full_name,
      full_address),
    ~ str_replace_all(.x, "^,", "")
  ))
# |>
#   dplyr::glimpse()
# 
# 
# vessels_permits_participants_short_u_flat_sp |>
#   dplyr::arrange(P_VESSEL_ID) |> 
#   head() |> 
#   str()

# add permit and address info ----
### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants |> 
  dplyr::select(P_VESSEL_ID) |> 
  dplyr::distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1

# how many vessels are missing from the db report
setdiff(
  date__contacttype_per_id$vessel_official_number,
  vessels_permits_participants_v_ids$P_VESSEL_ID
) |> cat(sep = "', '")
# |>
#   length()
# 6
# '1305388', '565041', 'FL0001TG', 'MI9152BZ', 'NC2851DH', 'VA1267CJ'
# (wrong license_nbr in full_participants
# or entity_id in permits,
# check manually)

# We don't need to check the reverse, there will be more vessels in the permit info we are not interested in

# clean up the report
vessels_permits_participants_space <-
  vessels_permits_participants |>
  # remove NAs
  dplyr::mutate(dplyr::across(where(is.character),
                ~ replace_na(., ""))) |>
  # trim trailing spaces, and replaces all internal whitespace with a single space.
  dplyr::mutate(dplyr::across(where(is.character),
                ~ str_squish(.)))

dim(vessels_permits_participants_space)
# [1] 31942    38

# combine info
vessels_permits_participants_short_u <-
  vessels_permits_participants_space |>
  # for each vessel
  dplyr::group_by(P_VESSEL_ID) |>
  dplyr::mutate(
    sero_home_port = list(unique(
      paste(
        SERO_HOME_PORT_CITY,
        SERO_HOME_PORT_COUNTY,
        SERO_HOME_PORT_STATE
      )
    )),
    full_name = list(unique(
      paste(FIRST_NAME,
            MIDDLE_NAME,
            LAST_NAME,
            NAME_SUFFIX)
    )),
    full_address = list(unique(
        paste(ADDRESS_1,
              ADDRESS_2,
              STATE,
              POSTAL_CODE)
      ))
  ) |>
  # use only new columns and the vessel id
  dplyr::select(P_VESSEL_ID,
         sero_home_port,
         full_name,
         full_address) |>
  dplyr::ungroup() |>
  dplyr::distinct()

dim(vessels_permits_participants_short_u)
# [1] 3302    4

# convert lists in comma separated strings
vessels_permits_participants_short_u_flat <-
  vessels_permits_participants_short_u |>
  dplyr::rowwise() |>
  dplyr::mutate_if(is.list,
            ~ paste(unlist(.),
                    collapse = ', ')) %>%
  # back to colwise
  dplyr::ungroup()

data_overview(vessels_permits_participants_short_u_flat) |> 
  head(1)
# P_VESSEL_ID 3302

# clean up weird comma and space combinations
vessels_permits_participants_short_u_flat_sp <-
  vessels_permits_participants_short_u_flat |>
  dplyr::mutate(
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove whitespace at the start and end, and replaces all internal whitespace with a single space.
    ~ stringr::str_squish(.x)
  ),
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove space characters before commas
    ~ stringr::str_replace_all(.x, "\\s+,", ",")
  ),
  dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # replace 2+ commas with one
    ~ stringr::str_replace_all(.x, ",,+", ",")
  ),
  dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove commas at the end
    ~ stringr::str_replace_all(.x, ",$", "")
  ),
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove commas in front
    ~ stringr::str_replace_all(.x, "^,", "")
  ))

## Manually check missing addresses ----

### From FHIER ----
# REPORTS / For-hire Primary Physical Address List
fhier_addr <-
  read_csv(
    file.path(
      all_inputs,
      "..",
      r"(my_outputs\egregious_violators\For-hire Primary Physical Address List.csv)"
    ),
    # read all as characters
    col_types = cols(.default = 'c'),
    # use the same function for names, see above
    name_repair = fix_names
  )

# fewer fields
fhier_addr_short <-
  fhier_addr |>
  dplyr::select(
    vessel_official_number,
    permit_holder_names,
    physical_address_1,
    physical_address_2,
    physical_city,
    physical_county,
    physical_state,
    physical_zip_code,
    phone_number,
    primary_email
  ) |>
  dplyr::mutate(
    fhier_address =
      paste(
        physical_address_1,
        physical_address_2,
        physical_city,
        physical_county,
        physical_state,
        physical_zip_code
      )
  ) |>
  dplyr::select(
    -c(
      physical_address_1,
      physical_address_2,
      physical_city,
      physical_county,
      physical_state,
      physical_zip_code
    )
  )

# join with the previous results from the db
fhier_addr__compl_corr <-
  right_join(
    fhier_addr_short,
    compl_corr_to_investigation1_short_dup_marked,
    join_by("vessel_official_number")
  )

dim(fhier_addr__compl_corr)
# [1] 117  17

### check if the address or name missing from the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  dplyr::filter((is.na(full_name) &
            !is.na(permit_holder_names)) |
           is.na(full_address) &
           !is.na(fhier_address))

dim(new_addr)
# 0

### check if the address or name is a "UN" in the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  dplyr::filter((full_name == "UN" &
            !is.na(permit_holder_names)) |
           full_address == "UN" &
           !is.na(fhier_address))

dim(addr_name_in_fhier)
# [1] 19 17

addr_name_in_not_fhier <-
  fhier_addr__compl_corr |>
  dplyr::filter(((!is.na(full_name) | !full_name == "UN") &
            is.na(permit_holder_names)) |
           (!is.na(full_address) | !full_address == "UN") &
           is.na(fhier_address))

dim(addr_name_in_not_fhier)
39

### check if the address or name missing from the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  dplyr::filter((is.na(full_name) &
            !is.na(permit_holder_names)) |
           is.na(full_address) &
           !is.na(fhier_address))

dim(new_addr)
# 0

### check if the address or name is a "UN" in the db is in FHIER ----
addr_name_in_fhier <-
  fhier_addr__compl_corr |>
  dplyr::filter((full_name == "UN" &
            !is.na(permit_holder_names)) |
           full_address == "UN" &
           !is.na(fhier_address))

dim(addr_name_in_fhier)
# [1] 19 17

### add info from FHIER to the results ----
setdiff(names(vessels_permits_participants_short_u_flat_sp),
  names(fhier_addr__compl_corr)
)

setdiff(
  names(fhier_addr__compl_corr),
  names(vessels_permits_participants_short_u_flat_sp)
  )
# print_df_names(fhier_addr__compl_corr)

vessels_permits_participants_short_u_flat_sp_add <-
  vessels_permits_participants_short_u_flat_sp |>
  left_join(
    fhier_addr__compl_corr,
    join_by(
      P_VESSEL_ID == vessel_official_number,
      sero_home_port,
      full_name,
      full_address
    )
  ) |>
  dplyr::mutate(
    full_name =
      dplyr::case_when(
        is.na(full_name) | full_name == "UN" ~
          permit_holder_names,
        .default = full_name
      ),
    full_address =
      dplyr::case_when(
        is.na(full_address) | full_address == "UN" ~
          fhier_address,
        .default = full_address
      )
  ) |>
  dplyr::select(P_VESSEL_ID, sero_home_port, full_name, full_address) |>
  dplyr::distinct()

library(diffdf)
diffdf(vessels_permits_participants_short_u_flat_sp,
       vessels_permits_participants_short_u_flat_sp_add)
  
  #    Variable    No of Differences 
  #  full_address         896        


# combine vessels_permits and date__contacttype ----

vessels_permits_participants_date__contacttype_per_id <-
  inner_join(
    date__contacttype_per_id,
    vessels_permits_participants_short_u_flat_sp,
    join_by(vessel_official_number == P_VESSEL_ID)
  )

dim(vessels_permits_participants_date__contacttype_per_id)
# 117

# ---- combine output ----
compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation1 |>
  inner_join(vessels_permits_participants_date__contacttype_per_id,
             by = "vessel_official_number")

dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31
# 309
# 271
# [1] 522  31

## ---- 2) remove extra columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

# print_df_names(vessels_permits_participants_date__contacttype_per_id)

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  dplyr::select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permitgroupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    date__contacttypes, 
    sero_home_port, 
    full_name, 
    full_address,
    # "week_start",
    "date__contacttypes"
  ) |>
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number")

# View(compl_corr_to_investigation1_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9
# 108
# str(compl_corr_to_investigation1_short)
# 97
# 116   9

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
# Download first
previous_egr_data_path <-
  file.path(
    my_paths$outputs, current_project_name,
    r"(from_web\egregious violators for investigation - 2023-01-24_to_2023-08-01.csv)"
  )

file.exists(previous_egr_data_path)
# T
vessels_to_mark <-
  read_csv(previous_egr_data_path)

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  # dplyr::filter(tolower(`Contacted 2x?`) == 'yes') |>
  dplyr::select(vessel_official_number)

# mark these vessels
compl_corr_to_investigation1_short_dup_marked <-
  compl_corr_to_investigation1_short |>
  dplyr::mutate(
    duplicate_w_last_time =
      dplyr::case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation1_short_dup_marked)
# [1] 177  11
# [1] 105  10
# 108
# 97
# [1] 110  10 2 atmpts
# [1] 116  10

dim(compl_corr_to_investigation1_short_dup_marked)
# 102
# 27: 164
# 177
# 31
# 108

#### check ----
  # no applicable method for 'distinct' applied to an object of class "character"

length(unique(compl_corr_to_investigation1_short_dup_marked$vessel_official_number))
# 107
# 102
# 27: 164
# 177
# 105
# 108
# 97
# 110
# 116

data_overview(compl_corr_to_investigation1_short_dup_marked) |> head(1)
# vessel_official_number
# 177
# 105
# 108
# 110
# 116

## add comments from the compliance crew (if already exist) ----
# results_with_comments_path <-
#   file.path(
#     my_paths$outputs,
#     current_project_name,
#     r"(from_web\egregious violators for investigation - 06-26-2023.csv)"
#   )

# file.exists(results_with_comments_path)
# T

# results_with_comments <-
#   readr::read_csv(results_with_comments_path,
#                   col_types = cols(.default = 'c'))
#
# dim(results_with_comments)
# 134 13

# all.equal(results_with_comments,
#           compl_corr_to_investigation1_short_output)
# F

# setdiff(results_with_comments$vessel_official_number,
#         compl_corr_to_investigation1_short_dup_marked$vessel_official_number) |>
#   length()
# 68
# 35 (new dplyr::filter)
# 67
# 71
# in_the_new_res_only <-
#   setdiff(
#     compl_corr_to_investigation1_short_dup_marked$vessel_official_number,
#     results_with_comments$vessel_official_number
#   )
# |> cat()
# 1266718 602091 FL0435LD FL6279PH FL7282LE FL8725DA
# length(in_the_new_res_only)
# 47
# 6
# 1061382 1069364 1168496 1209015 1224219 1259129 1266718 1296710 1308401 1318467 1331794 523112 602091 678141 970286 996263 FL0435LD FL2447TL FL2453TE FL3159TK FL3697PB FL3979EA FL4801NV FL6279PH FL6680JK FL6954LD FL7772SV FL8090RU FL8666CH FL8725DA FL9131RJ FL9446TH FL9793RU FL9914GX GA8847NJ MD9128BD MS8535ZG NC2851DH NC4246DP NC9819DF VA1460CJ
# 34

### join comments

# by = join_by(
#   vessel_official_number,
#   name,
#   permit_expired,
#   permitgroup,
#   permitgroupexpiration,
#   contactrecipientname,
#   contactphone_number,
#   contactemailaddress,
#   date__contacttypes
# )

# compl_corr_to_investigation1_short_output_w_comments <-
#   left_join(compl_corr_to_investigation1_short_dup_marked,
#             results_with_comments,
#             by,
#             # Override the default suffixes, c(".x", ".y") in not merged cols
#     suffix = c(".my_output",
#                ".commented_output")
#             )
# Joining with `by = join_by(vessel_official_number, name, permit_expired,
# permitgroup, permitgroupexpiration, contactrecipientname,
# contactphone_number, contactemailaddress, week_start, date__contacttypes)`

# dim(compl_corr_to_investigation1_short_output_w_comments)
# 38
# 280
# 0
# [1] 105  14
# 108
# 97
# [1] 110  14

#### check no comments ----
# no_comments_vsls <-
#   compl_corr_to_investigation1_short_output_w_comments |>
#   dplyr::filter(is.na(
#     `Confirmed Egregious? (missing past 6 months, 2 contacts with at least 1 call)`
#   ))
# # |>
# View(no_comments_vsls)
# Rows: 53
# Columns: 14

# in_the_new_res_only_df <-
#   as.data.frame(in_the_new_res_only)
# names(in_the_new_res_only_df) <- "vessel_official_number"

# no_comments_vsls_ids <-
#   no_comments_vsls |>
#   dplyr::select(vessel_official_number)
# dim(no_comments_vsls_ids)
# 62

# no_comments_vsls_ids |>
#   dplyr::filter(vessel_official_number == '1305207') |> dim()
# 1
# compl_corr_to_investigation1_short_output_w_comments |>
#   dplyr::filter(vessel_official_number == '1305207') |> dim()
# [1]  1 21

# setdiff(no_comments_vsls_ids$vessel_official_number, in_the_new_res_only_df) |>
#   length()
# 1305207
# 62

# setdiff(in_the_new_res_only_df, no_comments_vsls_ids$vessel_official_number)
# 0

# temp 1 ----
fhier_addr <-
  read_csv(
    r"(C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\For-hire Primary Physical Address List.csv)",
    col_types = cols(.default = 'c'),
    name_repair = fix_names
  )

# vessel_official_number, permits, effective_date, end_date, has_sa_permits_, has_gom_permits_, assigned_permit_region_grouping, permit_holder_names, physical_address_1, physical_address_2, physical_city, physical_county, physical_state, physical_zip_code, phone_number, primary_email

fhier_addr_short <-
  fhier_addr |>
  dplyr::select(
    vessel_official_number,
    permit_holder_names,
    physical_address_1,
    physical_address_2,
    physical_city,
    physical_county,
    physical_state,
    physical_zip_code,
    phone_number,
    primary_email
  ) |>
  dplyr::mutate(
    fhier_address =
      paste(
        physical_address_1,
        physical_address_2,
        physical_city,
        physical_county,
        physical_state,
        physical_zip_code
      )
  ) |>
  dplyr::select(
    -c(
      physical_address_1,
      physical_address_2,
      physical_city,
      physical_county,
      physical_state,
      physical_zip_code
    )
  )

res1 <-
  right_join(
    fhier_addr_short,
    compl_corr_to_investigation1_short_dup_marked,
    join_by("vessel_official_number")
  )

# View(res1)
no_addr_vessl <-
  c("1305388",
    "949058",
    "FL2555TF",
    "MI9152BZ",
    "NC2851DH")

res1 |> 
  dplyr::filter(vessel_official_number %in% no_addr_vessl) |> 
  dim()

fhier_addr_short |> 
  dplyr::filter(vessel_official_number %in% no_addr_vessl) |> 
  dim()
# 0

no_addr1 <-
  c("1066100",
    "1069364",
    "1209015",
    "1266505",
    "1316879",
    "622813",
    "678141",
    "938364",
    "996263",
    "FL0061PZ",
    "FL0380JY",
    "FL0435LD",
    "FL2153SM",
    "FL2367PW",
    "FL2453TE",
    "FL3002LF",
    "FL3017ME",
    "FL3262PM",
    "FL5736GJ",
    "FL6954LD",
    "FL7772SV",
    "FL8077RA",
    "FL8666CH",
    "FL9131RJ",
    "FL9793RU",
    "NC9819DF")

fhier_addr_short |>
  # dplyr::filter(vessel_official_number == "1308401") |>
  dplyr::filter(vessel_official_number %in% no_addr1) |>
  dplyr::select(vessel_official_number,
         permit_holder_names, fhier_address) |>
  write_csv("fhier_addr_short.csv")
# 22


# temp 2 ----
prev_res <-
  read_csv(r"(C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\egregious violators for investigation - 2023-01-24_to_2023-08-01_comment.csv)",
           col_types = cols(.default = 'c'))

intersect(names(prev_res),
          names(compl_corr_to_investigation1_short_dup_marked)) |> 
  cat(sep = ", ")

compl_corr_to_investigation1_short_dup_marked_ch <-
  compl_corr_to_investigation1_short_dup_marked |>
  dplyr::mutate(across(everything(), as.character)) |>
  dplyr::select(
    -c(
      name,
      permit_expired,
      permitgroup,
      permitgroupexpiration,
      contactrecipientname,
      contactphone_number,
      contactemailaddress,
      date__contacttypes,
      duplicate_w_last_time
    )
  )
# View(compl_corr_to_investigation1_short_dup_marked_ch)

new_join <-
  left_join(
    prev_res,
    compl_corr_to_investigation1_short_dup_marked_ch,
    join_by(
      vessel_official_number
    )
  )

# output ----
result_file_path <- file.path(
  my_paths$outputs,
  current_project_name,
  paste0(
    "egregious_violators_for_investigation_from_",
    half_year_ago,
    "_to_",
    data_file_date,
    ".csv"
  ))

# View(new_join)
readr::write_csv(
  new_join,
    # compl_corr_to_investigation1_short_output_w_comments,
  result_file_path,
  na = "")

compl_corr_to_investigation1_short_dup_marked |>
  check_new_vessels()
# 2
# FL4232JY
# FL7549EJ

## ---- who needs an email ----
# source(file.path(current_project_path, "need_an_email.R"))

# ## ---- no correspondence ----
# source(
#   file.path(
#     current_project_path,
#     "not_compliant_51_plus_weeks_and_no_correspondence.R"
#   )
# )
#
# ## ---- correspondence, no compliance information ----
# no_compl_info <-
#   setdiff(
#     corresp_contact_cnts_clean$vessel_official_number,
#     compl_clean$vessel_official_number
#   )
# length(no_compl_info)
# # 398
# # 136
# # Not in compliance info!
#
# # grep("1131132", compl_clean$vessel_official_number)
# # 0

current_project_name <- "egregious_violators"
current_project_path <-
  file.path(my_paths$git_r, current_project_name)

# make a flat file ----

# "C:\Users\anna.shipunova\Documents\R_code_github\egregious_violators\get_data_egregious_violators.R"

files_to_combine <-
  c("~/R_code_github/useful_functions_module.r",
    file.path(current_project_path, "db_functions.R"),
    file.path(current_project_path, "get_data_egregious_violators.R"),
    file.path(current_project_path, "egregious_violators.R")
  )

# run as needed
make_a_flat_file(
  file.path(current_project_path,   "egregious_violators_flat_file.R"),
  files_to_combine
)

