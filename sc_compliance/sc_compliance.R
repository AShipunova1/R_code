# We have a request that is going to require some new coding, but it shouldn't be too difficult. Essentially SC (Eric Hiltz) sends us a list of active SC/SEFHIER permitted vessels each month, which Anna has been using to update the FHIER flag ("SC permitted"). Eric will still be sending us that same list of vessels each month but with added columns that now reflect whether the vessel is compliant or not (0 or 1, respectively). Note, we still need to use the list to update FHIER but we need to add a couple new items to the task.
#
# So, in addition to updating FHIER, we need to create code that can do two things:
#
# read in this file of SC/SEFHIER vessel IDs for the given month (e.g. March 2024) and then pull out all vessels marked as "1" (non-compliant) - herein, "SC non-compliant vessels list". Then with that SC non-compliant vessel list we need to join//find all those vessels that are in the FHIER compliance table and create an output file that consists of a check that we will send back to Eric. In the check/output file we need, (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.
# If you have time or would like to attempt writing this code let me know ASAP. Ideally, I would like to have this operational by the next time Eric sends his list (which is usually the beginning of every month). I can start writing the code this week, if neither of you has time.

# set up ----
require(openxlsx)

my_year <- "2024"
db_year_1 <- "2023"
db_year_2 <- "2024"

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

current_project_dir_name <- this.path::this.dir()

current_project_basename <-
  basename(current_project_dir_name)

curr_proj_output_path <- file.path(my_paths$outputs,
                         current_project_basename)

# get data ----
## Import and prep compliance/override data ----

### Import compliance/override data ----
# Prepare 2 variables to use as parameters for read_rds_or_run_query()

# 1) Use file.path to construct the path to a file from components. It will add the correct slashes between path parts.
compl_override_data_file_path <-
  file.path(r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs\Raw_Oracle_Downloaded_compliance_2021_plus.rds)")

# Check if the file path is correct, optional
file.exists(compl_override_data_file_path)

# 2) Create a variable with a table name to call data from, define year.
# >= 2021 because of when the program started or between 2 years defined above
compl_err_query <-
  str_glue(
  "SELECT
  *
FROM
  srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
WHERE
  comp_year >= '{db_year_1}' AND comp_year <= '{db_year_2}'")

# Create the compliance/overridden data frame
# using the function pre-defined above to check if there is a file saved already,
# read it
# or run the query and write the file for future use

compl_override_data <-
  read_rds_or_run(compl_override_data_file_path,
                  my_function = compl_err_query)
# File: Raw_Oracle_Downloaded_compliance_2021_plus.rds modified 2024-04-02 12:18:54.299425

### prep the compliance/override data ----

# Change column names for consistency with other datasets
compl_override_data__renamed <-
  compl_override_data |>
  dplyr::rename(vessel_official_number =
                  "VESSEL_OFFICIAL_NBR",
                overridden = "IS_COMP_OVERRIDE") |>
  clean_headers()

# change data type of this column if needed
if (!class(compl_override_data__renamed$vessel_official_number) == "character") {
  compl_override_data__renamed$vessel_official_number <-
    as.character(compl_override_data__renamed$vessel_official_number)
}

# TODO: check in the other code
compl_override_data__renamed_m <-
  compl_override_data__renamed |>
  mutate(comp_month = month(comp_week_end_dt))

### keep fewer compl fields ----
print_df_names(compl_override_data__renamed_m)

compl_override_data__renamed_m_short <-
  compl_override_data__renamed_m |>
  select(
    vessel_official_number,
    permit_group,
    prm_grp_exp_date,
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    comp_month
  ) |>
  distinct() |>
  filter(comp_year %in% c(my_year, as.integer(my_year) - 1))

dim(compl_override_data__renamed_m)
dim(compl_override_data__renamed_m_short)

### add compliance/overridden combinations by week ----
tic("get comp/overridden")
compl_override_data__renamed_m_short__compl_overr_by_week <-
  add_compliant_after_override(compl_override_data__renamed_m_short)
toc()
# get comp/overridden: 72.5 sec elapsed

# View(compl_override_data__renamed_m_short__compl_overr_by_week)
# compl_override_data__renamed_m_short__compl_overr_by_week |>
#     select(is_comp, overridden, compliant_after_override) |>
#     distinct()
# 1       1          0 yes
# 2       0          0 no
# 3       0          1 yes
# 4       1          1 yes

### combine compliance by month ----
tic("get month_comp")
compl_override_data__renamed_m_short__m_compl <-
  compl_override_data__renamed_m_short__compl_overr_by_week |>
  group_by(vessel_official_number, comp_year, comp_month) |>
  mutate(all_m_comp = toString(unique(sort(compliant_after_override)))) |>
  mutate(month_comp =
           case_when(all_m_comp %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup()
toc()


# View(compl_override_data__renamed_m_short__m_compl)

# check
# compl_override_data__renamed_m_short__m_compl |>
#     select(compliant_after_override, is_comp, overridden, all_m_comp, month_comp) |>
#     distinct()

compl_override_data__renamed_m_short__m_compl |>
  filter(vessel_official_number == "657358"
         # comp_year == 2024,
         # comp_month == 2
         ) |>
  filter(month_comp == "non_compl") |>
  str()
#
#   glimpse()

## get logbooks ----
logbooks_path <- file.path(
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)",
  str_glue("SEFHIER_processed_Logbooks_{my_year}.rds")
)

logbooks <-
  read_rds(logbooks_path) |>
  clean_headers()

dim(logbooks)

## get dnfs ----
dnfs_path <- file.path(
  r"(~\R_files_local\my_inputs\processing_logbook_data\Outputs)",
  str_glue("SEFHIER_processed_dnfs_{my_year}.rds")
)

dnfs <-
  read_rds(dnfs_path) |>
  clean_headers()

dim(dnfs)

## get srhs vessels ----
srhs_2024_file_path <-
  file.path(my_paths$inputs,
            r"(SRHS_headboat_survey\Vessel_List_2024.csv)")

srhs_2024 <-
  read_csv(srhs_2024_file_path,
           col_types = cols(.default = 'c')) |>
  clean_headers()

# View(srhs_2024)

## read sc permitted data ----
xlsx_names_list = list(r"(sc_mismatches\2024_04\scdnrFedVessels_04012024.xlsx)")

length(xlsx_names_list)

myfiles <- lapply(xlsx_names_list, function(x) file.path(my_paths$inputs, x))

file.exists(myfiles[[1]])

SC_permittedVessels <- read_excel(
  myfiles[[1]],
  .name_repair = fix_names,
  guess_max = 21474836
)

### fix dates in headers ----
date_names <- names(SC_permittedVessels)[7:18] |>
  convertToDate() |> format("%m-%y")
  # format("%b-%y")

names(SC_permittedVessels)[7:18] <-
  date_names

names(SC_permittedVessels)

dim(SC_permittedVessels)
# [1] 215  18

### change sc data format ----

# glimpse(SC_permittedVessels)
SC_permittedVessels_longer <-
  SC_permittedVessels |>
  pivot_longer(
    !c(
      "vessel_reg_uscg_",
      "vessel_name",
      "reports_to_srhs",
      "federal_for_hire_permit_expiration",
      "marked_as_federally_permitted_in_vesl",
      "delinquent"
    ),
    names_to = "month_year",
    values_to = "delinquent_month"
  )

SC_permittedVessels_longer_m_y <-
  SC_permittedVessels_longer |>
  # filter(delinquent_month == 1) |>
  separate_wider_delim(cols = month_year,
                       delim = "-",
                       names = c("month_sc", "year_sc")) |>
  mutate(year_sc = paste0("20", year_sc)) |>
  mutate(across(all_of(c("month_sc", "year_sc")), as.numeric)) |>
  distinct()

glimpse(SC_permittedVessels_longer_m_y)

# SRHS: check and remove reports_to_srhs ----
sc__srhs_join <-
  full_join(SC_permittedVessels_longer_m_y,
            srhs_2024,
            join_by(vessel_reg_uscg_ == uscg__))



# combine data ----

sc__fhier_compl__join_w_month <-
  left_join(
    SC_permittedVessels_longer_m_y,
    compl_override_data__renamed_m_short__m_compl,
    join_by(
      vessel_reg_uscg_ == vessel_official_number,
      month_sc == comp_month,
      year_sc == comp_year,
    )
  )

dim(SC_permittedVessels)
# 215
dim(SC_permittedVessels_longer_m_y)
# 2580
dim(sc__fhier_compl__join_w_month)
# 8013
n_distinct(SC_permittedVessels) == n_distinct(sc__fhier_compl__join_w_month$vessel_reg_uscg_)
# T
# 215
# View(sc__fhier_compl__join_w_month)

# sc_fhier <-
#   left_join(
#     SC_permittedVessels,
#     compl_override_data__renamed,
#     join_by(vessel_reg_uscg_ == vessel_official_number)
#   )

# sc__fhier_compl__join_w_month_no_weeks <-
#   sc__fhier_compl__join_w_month |>
#   select(-c(comp_week, comp_week_start_dt, comp_week_end_dt, is_comp)) |>
#   distinct()

dim(sc__fhier_compl__join_w_month)
# [1] 2588   14
# [1] 8023   19 (w weeks)
#

# 1. the list of those SC non-compliant vessels that are also non-compliant in FHIER ----

non_compliant_vessels_in_sc_and_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent == 1 &
           month_comp == "non_compl") |>
  filter(delinquent_month == 1) |>
  # select(vessel_reg_uscg_, month_sc, year_sc) |>
  distinct()

# View(non_compliant_vessels_in_sc_and_fhier)
# 2
# 8 18 with weeks
# (?) which columns to the output?

non_compliant_vessels_in_sc_and_fhier__for_output <-
  non_compliant_vessels_in_sc_and_fhier |>
  select(
    vessel_reg_uscg_,
    vessel_name,
    delinquent,
    month_sc,
    year_sc,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    compliant_after_override
  ) |>
  distinct()

# View(non_compliant_vessels_in_sc_and_fhier__for_output)

# 2. non compliant in SC and compliant in FHIER ----
# 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
non_compliant_vessels_in_sc_and_compl_in_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent == 1 &
           month_comp == "compl")

dim(non_compliant_vessels_in_sc_and_compl_in_fhier)
# 40 14
# [1] 172  19 w weeks

## add logbooks info ----
# Logbook (list any dates for that month)
logbooks__sc_fhier <-
  logbooks |>
  filter(vessel_official_number %in%
           non_compliant_vessels_in_sc_and_compl_in_fhier$vessel_reg_uscg_)

dim(logbooks__sc_fhier)
# 4

logbooks__sc_fhier_for_output <-
  logbooks__sc_fhier |>
  select(
    vessel_official_number,
    vessel_name,
    trip_start_date,
    trip_end_date,
    vendor_app_name,
    trip_de,
    trip_ue
  ) |>
  distinct()

## add DNF info ----
# DNF (list week date range for any for that month)
dnfs__sc_fhier <-
  dnfs |>
  filter(vessel_official_number %in%
           non_compliant_vessels_in_sc_and_compl_in_fhier$vessel_reg_uscg_)

dim(dnfs__sc_fhier)
# 94

dnfs__sc_fhier_for_output <-
  dnfs__sc_fhier |>
  mutate(trip_date_month = month(trip_date)) |>
  group_by(vessel_official_number, trip_date_year, trip_date_month) |>
  mutate(all_m_comp = toString(unique(sort(compliant_after_override)))) |>
  mutate(month_comp =
           case_when(all_m_comp %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup() |>
  filter(month_comp == "compl" & compliant_after_override == "yes") |>
  select(
    vessel_official_number,
    vessel_name,
    month_comp,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    compliant_after_override
  ) |>
  distinct()

# View(dnfs__sc_fhier_for_output)
# 18

# 3. SC compliant vessels list ----
# 3) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent == 0 &
           month_comp == "non_compl")

dim(compliant_vessels_in_sc_and_non_compl_fhier)
# [1] 180  14
# [1] 739  19 with weeks
# View(compliant_vessels_in_sc_and_non_compl_fhier)

compliant_vessels_in_sc_and_non_compl_fhier__for_output <-
  compliant_vessels_in_sc_and_non_compl_fhier |>
  select(
    vessel_reg_uscg_,
    month_sc,
    year_sc,
    delinquent,
    comp_week_start_dt,
    comp_week_end_dt,
    is_comp,
    overridden,
    compliant_after_override
  ) |>
  filter(compliant_after_override == "no") |>
  distinct()

# View(compliant_vessels_in_sc_and_non_compl_fhier__for_output)
# [1] 739   7

# write results to xlsx ----
# (sheet 1) the list of those SC non-compliant vessels that are also non-compliant in FHIER, or
# (on sheet 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)
# we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER

output_file_name <-
  file.path(curr_proj_output_path,
            "sc_compliance.xlsx")

# non_compliant_vessels_in_sc_and_fhier__for_output
# logbooks__sc_fhier_for_output
# dnfs__sc_fhier_for_output
# compliant_vessels_in_sc_and_non_compl_fhier__for_output

result_list <-
  list(
    "non_compl_sc_and_fhier" =
      non_compliant_vessels_in_sc_and_fhier__for_output,
    "non_compl_sc__compl_fhier_lgb" =
      logbooks__sc_fhier_for_output,
    "non_compl_sc__compl_fhier_dnf" = dnfs__sc_fhier_for_output,
    "compl_sc__non_compl_fhier" =
      compliant_vessels_in_sc_and_non_compl_fhier__for_output
  )

openxlsx::write.xlsx(result_list, file = output_file_name)
