# 1) NO reports for all 26 weeks back from week ago today;
# 2) permits have not expired as of today;
# 3) the grace period is 7 days back from today.

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
library(diffdf)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()

current_project_path <- this.path::this.dir()

current_project_basename <-
  basename(current_project_path)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

curr_proj_input_path <- file.path(my_paths$inputs,
                         current_project_basename)

current_project_name <- current_project_basename

all_inputs <- my_paths$inputs

my_year1 <- "2023"
my_beginning1 <- str_glue("{my_year1}-01-01")
my_end1 <- str_glue("{my_year1}-12-31")

my_year2 <- "2024"
my_beginning2 <- str_glue("{my_year2}-01-01")
my_end2 <- str_glue("{my_year2}-12-31")

data_file_date <- today()
# [1] "2024-02-16"

number_of_weeks_for_non_compliancy = 26
days_in_non_compl_weeks <- 
  number_of_weeks_for_non_compliancy * 7
# 182

grace_period = 7 #days

half_year_ago <-
  data_file_date - days_in_non_compl_weeks - grace_period
# [1] "2023-08-11"

permit_expired_check_date <- data_file_date

last_week_start <- data_file_date - grace_period
# [1] "2024-02-10"
  
# get_data ----

get_data_path <- 
  file.path(current_project_path, "egregious_violators_get_data.R")
source(get_data_path)

# ---- Preparing compliance info ----

## ---- add permit_expired column ----
compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is today than "no"
  mutate(permit_expired =
                  dplyr::case_when(
                    permit_groupexpiration > (permit_expired_check_date) ~ "no",
                    .default = "yes"
                  ))

# glimpse(compl_clean_w_permit_exp)

## ---- add year_month column ----

compl_clean_w_permit_exp_last_half_year <-
  compl_clean_w_permit_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last check period
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)
# today()
# [1] "2023-08-10"
# [1] 235509     22
# [1] "2024-02-16"
# [1] 168740     21

dim(compl_clean_w_permit_exp_last_half_year)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d
# [1] 87826    23
# [1] 74169    23
# [1] 80413    22

## ---- Have only SA and dual permits ----

compl_clean_w_permit_exp_last_half_year__sa <-
  compl_clean_w_permit_exp_last_half_year |>
  filter(grepl("CDW|CHS|SC", permitgroup))

today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"
# [1] "2024-02-16"

dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 55194    22

## get only not expired last 27 weeks of data minus grace period ----
compl_clean_w_permit_exp_last_half_year__sa__not_exp <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  # the last 27 week
  filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  filter(week_end < last_week_start) |>
  # not expired
  filter(tolower(permit_expired) == "no")

# View(compl_clean_w_permit_exp_last_half_year__sa__not_exp)
# print_df_names(compl_clean_w_permit_exp_last_half_year__sa__not_exp)
min(compl_clean_w_permit_exp_last_half_year__sa__not_exp$permit_groupexpiration)
# [1] "2024-02-29 EST"

min(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_start)
# [1] "2023-08-14"
max(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_start)
# [1] "2024-01-29"

max(compl_clean_w_permit_exp_last_half_year__sa__not_exp$week_end)
# [1] "2024-02-04"

## fewer columns ----
remove_columns <- c(
  "name",
  "week",
  "gom_permitteddeclarations__",
  "captainreports__",
  "negativereports__",
  "complianceerrors__",
  "set_permits_on_hold_",
  "overridden_",
  "override_date",
  "override_by",
  "contactedwithin_48_hours_",
  "submittedpower_down_",
  "permit_expired"
)

compl_clean_w_permit_exp_last_half_year__sa__not_exp_short <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp |>
  select(-any_of(remove_columns)) |> 
  distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa)
# [1] 55194    22
dim(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short)
# [1] 44756    9

## work with the whole period ----
compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates <- 
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short |>
  select(vessel_official_number, compliant_) |>
  distinct()

glimpse(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates)
# [1] 3668    2

## add no_yes compliant ----
compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide <-
  get_compl_by(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates)

cols_names <- c()

compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide |>
  compl__back_to_longer_format(cols_names) |>
  filter(stats::complete.cases(is_compl_or_both))
# back_to_long: 21.31 sec elapsed with 22 cols
# back_to_long: 0.87 sec elapsed with 6 cols

compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long$is_compl_or_both |> 
  unique()
# [1] "YES"    "NO"     "NO_YES"

dim(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long)
# [1] 2246    2
# [1] 1917    2

n_distinct(compl_clean_w_permit_exp_last_half_year__sa$vessel_official_number)
# 2246

n_distinct(compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long$vessel_official_number)
# [1] 1917

## get only all "compliant_ == "NO" for the past half year ----
compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp <-
  compl_clean_w_permit_exp_last_half_year__sa__not_exp_short_no_dates__wide__long |>
  # not compliant
  filter(tolower(is_compl_or_both) == "no")

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp)
# [1] 10419    23
# [1] 9486   23
# [1] 9315   23
# [1] 7138   22
# [1] 141   2
# [1] 328   2

## add back columns needed for the output ----
need_cols_names <- c(
  "vessel_official_number",
  "name",
  "permit_expired",
  "permitgroup",
  "permit_groupexpiration"
  # ,
  # "week_start"
)

dim(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp)
# [1] 328   2

# View(compl_clean_w_permit_exp_last_half_year__sa)
compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c <-
  compl_clean_w_permit_exp_last_half_year__sa |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_w_permit_exp_last_half_year__sa_non_c__not_exp) |>
  # Joining with `by = join_by(vessel_official_number)`
  distinct()

# View(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c)
# [1] 130   8
# 0
# 127
# 121
# [1] 328   6

## check the last report date ----
### get ids only ----
compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c |>
  dplyr::select(vessel_official_number) |>
  dplyr::distinct()

dim(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids)
# [1] 128   1
# [1] 328   1

### check these ids in the full compliance information ----
compl_clean_w_permit_exp_last_half_year__sa |>
  filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # [1] 1938   22
  dplyr::group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  # dim()
# [1] 11 22
  glimpse()

# get only the latest compliant weeks
compliant_in_last_half_year <- 
  compl_clean_w_permit_exp_last_half_year__sa |>
  filter(
    vessel_official_number %in% compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  dplyr::group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  mutate(latest_compl = max(week_num)) |>
  filter(week_num == latest_compl) |>
  dplyr::ungroup() |>
  dplyr::select(vessel_official_number,
                year_month,
                week,
                latest_compl) |>
  dplyr::distinct()

glimpse(compliant_in_last_half_year)
# Rows: 9
# Columns: 3
# $ vessel_official_number <chr> "1332041", "1284153", "FL9738SR", "FL7361TJ", "FL…
# $ year_month             <yearmon> Aug 2023, Aug 2023, Aug 2023, Aug 2023, Aug 2…
# $ latest_compl           <int> 33, 33, 32, 32, 32, 32, 32, 32, 32

## Remove the vessels with the last compliant weeks ----

compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok <-
  compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c |>
  filter(!vessel_official_number %in% compliant_in_last_half_year$vessel_official_number)

dim(compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok)
# [1] 319   6

# ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

n_distinct(corresp_contact_cnts_clean$vesselofficial_number)
# vesselofficial_number   3223
# vessel_official_number  3371
# vesselofficial_number 3434
# 4118


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
  filter(!!two_attempts_filter)

# dim(corresp_contact_cnts_clean)
# [1] 18629    23
# dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23
# today()
# [1] "2024-02-16"
dim(corresp_contact_cnts_clean)
# [1] 29587    20
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 29089    20

n_distinct(corresp_contact_cnts_clean_direct_cnt_2atmps$vesselofficial_number)
# vesselofficial_number 2968
# 3620

## fix dates ----
head(corresp_contact_cnts_clean_direct_cnt_2atmps$contact_date, 1)
# [1] "02/15/2024 03:15PM"

corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  mutate(
    created_on_dttm =
      lubridate::parse_date_time(created_on,
                                 c("mdY R")),
    contact_date_dttm =
      lubridate::parse_date_time(contact_date,
                                 c("mdY R"))
  )

str(corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates$contact_date_dttm)
# POSIXct[1:29089], format: "2024-02-15 15:15:00" 

# Join correspondence with compliance ----
compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps_clean_dates,
    compl_clean_w_permit_exp_last_half_year__sa_all_weeks_non_c_ok,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation1)
# [1] 486  30
# [1] 522  30
# [1] 940  27
# [1] 2100   27

# check
n_distinct(compl_corr_to_investigation1$vesselofficial_number)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105
# 108
# 97
# vesselofficial_number 116
# 262

# View(compl_corr_to_investigation1)

## save number of vessels to investigate ----
num_of_vsl_to_investigate <- 
  n_distinct(compl_corr_to_investigation1$vesselofficial_number)
# 262

# ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) mark vessels already in the know list (prev_result)

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# # put names into vars
contactdate_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "date")[1]
contacttype_field_name <-
  find_col_name(compl_corr_to_investigation1, "contact", "type")[1]
 
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
      mutate(date__contacttype =
                      paste(!!sym(contactdate_field_name),
                            contacttype)) |>
      # use 2 columns only
      dplyr::select(vessel_official_number, date__contacttype) |>
      # sort
      dplyr::arrange(vessel_official_number, date__contacttype) |>
      dplyr::distinct() |>
      dplyr::group_by(vessel_official_number) |>
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = 
                  paste(date__contacttype, collapse = ", ")) %>%
      return()
  }

date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation1)
dim(date__contacttype_per_id)
# [1] 110    2
# 107
# 27: 177
# 188   2
# 105   2 (the new filter)
# 108
# 97
# [1] 116   2 (2 contact attempts)
# [1] 262   2

# glimpse(date__contacttype_per_id)

## add permit and address info ----
# print_df_names(vessels_permits_participants)

### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants |> 
  dplyr::select(P_VESSEL_ID) |> 
  dplyr::distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1
# [1] 3203    1

vessels_not_in_vessel_permit_from_db <-
  setdiff(
    date__contacttype_per_id$vessel_official_number,
    vessels_permits_participants_v_ids$P_VESSEL_ID
  ) |>
  sort()

length(vessels_not_in_vessel_permit_from_db)
# 32
vessels_not_in_vessel_permit_from_db |>
  cat(sep = "\n")
# cat(sep = "', '")
  # length()
# 32
# (wrong license_nbr in full_participants
# or entity_id in permits,
# check manually)

vessels_from_pims_needed <-
  vessels_from_pims |>
  filter(official__ %in% vessels_not_in_vessel_permit_from_db)

vessels_from_pims_needed |> 
  arrange(official__) |> 
  dim()
# [1] 30  8

vessels_from_pims_double |>
  filter(
    vessel_official_number1 %in% vessels_not_in_vessel_permit_from_db |
      vessel_official_number2 %in% vessels_not_in_vessel_permit_from_db
  ) |>
  arrange(vessel_official_number1, vessel_official_number2) |>
  dim()
# 3

clean_names_and_addresses <- function(my_df) {
  # to_remove <- c("", "UN", " UN", "UN ", ";, UN"
  # NA, "NA")
  
  my_df_cleaned <- 
    my_df |>
    mutate(
      across(where(is.character),
             ~ str_squish(.x)),
      across(where(is.character),
             ~ replace_na(.x, "")),
      across(where(is.character),
             ~ str_replace_all(.x, ", ;", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s+[,;]", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, ";,+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ";;+", ";")),
      across(where(is.character),
             ~ str_replace_all(.x, ",,+", ",")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;] *\\bUN\\b *", "")),
      across(where(is.character),

                          ~ str_replace_all(.x, "\\bUN\\b", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "\\s*\\bUN\\b\\s*", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;] ", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "^[,;]$", "")),
      across(where(is.character),
             ~ str_replace_all(.x, "[,;]$", "")),
      across(where(is.character),
             ~ str_squish(.x))
    )
  
  return(my_df_cleaned)
}

vessels_permits_participants_space <-
  vessels_permits_participants |>
  clean_names_and_addresses()

dim(vessels_permits_participants_space)
# [1] 31942    38
# [1] 30511    38

vessels_permits_participants_short_u <-
  vessels_permits_participants_space |>
  dplyr::group_by(P_VESSEL_ID) |>
  mutate(
    sero_home_port = list(unique(
      str_glue(
        "{SERO_HOME_PORT_CITY}, {SERO_HOME_PORT_COUNTY}, {SERO_HOME_PORT_STATE}"
      )
    )),
    full_name = list(unique(
      str_glue("{FIRST_NAME} {MIDDLE_NAME} {LAST_NAME}, {NAME_SUFFIX}")
    )),
    full_address = list(unique(
      str_glue("{ADDRESS_1}, {ADDRESS_2}, {STATE}, {POSTAL_CODE}")
    ))
  ) |>
  dplyr::select(P_VESSEL_ID,
                sero_home_port,
                full_name,
                full_address) |>
  dplyr::ungroup() |>
  dplyr::distinct()

dim(vessels_permits_participants_short_u)
# [1] 7858    4
# [1] 3302    4
# [1] 3203    4

# View(vessels_permits_participants_short_u)

# vessels_permits_participants_short_u |> 
#   # filter(lengths(full_name) > 0) %>%
#   # unnest(full_name) %>%
#   # unnest_wider(full_name, names_sep = "_") |> 
#   rowwise() |> 
#   mutate_if(is.list, ~paste(unlist(.), collapse = ', ')) %>% 
#   View()
 # cat()

vessels_permits_participants_short_u_flat <-
  vessels_permits_participants_short_u |>
  rowwise() |>
  mutate_if(is.list, ~ paste(unlist(.), collapse = '; ')) %>%
  dplyr::ungroup()

n_distinct(vessels_permits_participants_short_u_flat$P_VESSEL_ID)
# P_VESSEL_ID 3302
# 3203

vessels_permits_participants |>
  filter(P_VESSEL_ID == "635942") |>
  select(ADDRESS_1,
         ADDRESS_2,
         STATE,
         POSTAL_CODE) |>
  distinct() |>
  dim()
# 7
# $ ADDRESS_1   <chr> "160 SNUFF MILL RD", "160 SNUFF MILL ROAD", "P O BOX 625", "P…
# $ ADDRESS_2   <chr> NA, NA, NA, NA, NA, NA, NA
# $ STATE       <chr> "RI", "RI", "MD", "VA", "SC", "MD", NA
# $ POSTAL_CODE <chr> "02874", "02874", "21041", "23183", "295822571", "21014", NA

vessels_permits_participants_short_u_flat_sp <-
  vessels_permits_participants_short_u_flat |>
  clean_names_and_addresses()

filter(vessels_permits_participants_short_u_flat_sp,
       P_VESSEL_ID == "1173297") |>
  glimpse()

## combine with info from PIMS when missing ----
vessels_from_pims_needed_short <-
  vessels_from_pims_needed |>
  select(official__,
         hailing_port,
         owner) |>
  distinct()

### add an empty column ----
# to make the same amount of column as from db
vessels_from_pims_needed_short[ncol(vessels_from_pims_needed_short) + 1] <-
  c("")

names(vessels_from_pims_needed_short) <- 
  names(vessels_permits_participants_short_u_flat_sp)

vessels_permits_participants_short_u_flat_sp_full <-
  rbind(vessels_permits_participants_short_u_flat_sp,
        vessels_from_pims_needed_short)

### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants_short_u_flat_sp_full |> 
  dplyr::select(P_VESSEL_ID) |> 
  dplyr::distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1
# 3203
# 3232    

# how many vessels are missing from the db report
setdiff(
  date__contacttype_per_id$vessel_official_number,
  vessels_permits_participants_v_ids$P_VESSEL_ID
) |> 
  # cat(sep = "', '")
  length()
# 3

# We don't need to check the reverse, there will be more vessels in the permit info we are not interested in

# ## add missing addresses ----
# no_addr_vessel_ids <-
#   vessels_permits_participants_short_u_flat_sp_full |> 
#   filter(is.na(full_address) | 
#            full_address == "" |
#            is.na(full_name) | 
#            full_name == "") |> 
#   select(P_VESSEL_ID) |> 
#   distinct()
# 
# nrow(no_addr_vessel_ids)
# # 755
# # 1390 both addr and names
# # 1957 after removing "," etc.
# 
# fix_addresses_path <-
#   file.path(current_project_path,
#             str_glue("{current_project_basename}_fix_addresses.R"))
# 
# file.exists(fix_addresses_path)
# 
# source(fix_addresses_path)
# 
# ### add info from FHIER to the results ----
# intersect(names(vessels_permits_participants_short_u_flat_sp),
#         names(addr_name_in_fhier))
# # P_VESSEL_ID
# # print_df_names(fhier_addr__compl_corr)
# # glimpse(addr_name_in_fhier)
# 
# vessels_permits_participants_short_u_flat_sp_join <-
#   left_join(vessels_permits_participants_short_u_flat_sp,
#             addr_name_in_fhier,
#             join_by(P_VESSEL_ID),
#             suffix = c("__vsl_db",
#                        "__vsl_fhier"))
# 
# # View(vessels_permits_participants_short_u_flat_sp_join)
# 
# # print_df_names(vessels_permits_participants_short_u_flat_sp_join)
# 
# vessels_permits_participants_short_u_flat_sp_join |>
# # vessels_permits_participants_short_u_flat_sp |> 
#   filter(P_VESSEL_ID == "FL1431JU") |> 
#   # filter(!is.na(full_name__vsl_fhier) | 
#   #          !is.na(full_address__vsl_fhier) |
#   #          !is.na(fhier_address)) |> 
#   View()
# 
# vessels_permits_participants_short_u_flat_sp_add <- 
#   vessels_permits_participants_short_u_flat_sp_join |> 
#   mutate(
#     full_name =
#       dplyr::case_when(
#         is.na(full_name) | 
#           full_name %in% c("", "UN") ~
#           permit_holder_names,
#         .default = full_name
#       ),
#     full_address =
#       dplyr::case_when(
#         is.na(full_address) | 
#           full_address %in% c("", "UN") ~
#           fhier_address,
#         .default = full_address
#       )
#   ) |>
#   dplyr::select(P_VESSEL_ID, 
#                 sero_home_port, 
#                 full_name, 
#                 full_address) |>
#   dplyr::distinct()
# 
# diffdf::diffdf(vessels_permits_participants_short_u_flat_sp,
#        vessels_permits_participants_short_u_flat_sp_add)
# #    Variable    No of Differences 
# #   full_name          1008        
# #  full_address         747        
# 
# combine vessels_permits and date__contacttype ----

vessels_permits_participants_date__contacttype_per_id <-
  left_join(
    date__contacttype_per_id,
    vessels_permits_participants_short_u_flat_sp,
    join_by(vessel_official_number == P_VESSEL_ID)
  )

# View(vessels_permits_participants_date__contacttype_per_id)
n_distinct(date__contacttype_per_id$vessel_official_number)
# 262
dim(vessels_permits_participants_date__contacttype_per_id)
# 117
# [1] 262   5

# compare vsl numbers
num_of_vsl_to_investigate == n_distinct(vessels_permits_participants_date__contacttype_per_id$vessel_official_number)
# T

# combine output ----

compl_corr_to_investigation1__w_addr <-
  left_join(
    compl_corr_to_investigation1,
    vessels_permits_participants_date__contacttype_per_id
  )

dim(compl_corr_to_investigation1__w_addr)
# [1] 2100   31

# compare vsl numbers
num_of_vsl_to_investigate == n_distinct(compl_corr_to_investigation1__w_addr$vessel_official_number)
# T
# 262

# compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
#   compl_corr_to_investigation1 |>
#   inner_join(vessels_permits_participants_date__contacttype_per_id,
#              by = "vessel_official_number")

# dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31
# 309
# 271
# [1] 522  31

## ---- 2) remove extra columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

# print_df_names(compl_corr_to_investigation1__w_addr)

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1__w_addr |>
  # compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  dplyr::select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permit_groupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    sero_home_port,
    full_name,
    full_address,
    # "week_start",
    "date__contacttypes"
  ) |>
  group_by(vessel_official_number) |>
  summarise_all(concat_unique)

# compl_corr_to_investigation1_short |> glimpse()

  # combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number")

dim(compl_corr_to_investigation1_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9
# 108
# [1] 262  12

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
# Download first
previous_egr_data_path <-
  file.path(
    my_paths$inputs,
    current_project_name,
    r"(egregious violators for investigation_2023-01-24_to_2023-08-01_OLEAction(green).xlsx)"
  )

file.exists(previous_egr_data_path)
# T

vessels_to_mark <-
  read_xlsx(previous_egr_data_path) |> 
  remove_empty_cols()

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  # filter(tolower(`Contacted 2x?`) == 'yes') |>
  dplyr::select(vessel_official_number)

dim(vessels_to_mark_ids)
# [1] 96  1

# mark these vessels
compl_corr_to_investigation1_short_dup_marked <-
  compl_corr_to_investigation1_short |>
  mutate(
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
# [1] 262  13

### check ----
n_distinct(compl_corr_to_investigation1_short_dup_marked$vessel_official_number)
# 107
# 102
# 27: 164
# 177
# 105
# 108
# 97
# 110
# 116
# 2024-02-20
# 262

result_path <- 
  file.path(my_paths$outputs,
            current_project_basename,
            str_glue("egregious_violators_to_investigate_{today()}.csv"))

write_csv(compl_corr_to_investigation1_short_dup_marked,
          result_path)

# how many are duals? ----
