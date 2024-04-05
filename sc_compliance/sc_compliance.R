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

# TODO: change to the overlapping intervals (lubridate)
compl_override_data__renamed_interv <-
  compl_override_data__renamed |>
  group_by(comp_week_start_dt, comp_week_end_dt) |>
  mutate(comp_month_min =
           min(month(comp_week_start_dt),
               month(comp_week_end_dt))) |>
  mutate(comp_month_max =
           max(month(comp_week_start_dt),
               month(comp_week_end_dt))) |>
  # mutate(comp_month_interval =
  #          lubridate::interval(comp_month_min,
  #                              comp_month_max)) |>
  ungroup()
toc()

# check
compl_override_data__renamed_interv |>
  filter(!comp_month_min == comp_month_max) |>
  select(
    comp_year,
    comp_week,
    comp_week_start_dt,
    comp_week_end_dt,
    comp_month_min,
    comp_month_max
  ) |>
  distinct() |>
  glimpse()
### keep fewer compl fields ----
# Keep only entries for "my_year" defined earlier and the previous year. COuld be changed depending on the data provided by SC.

compl_override_data__renamed_m_short <-
  compl_override_data__renamed_interv |>
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
    comp_month_min,
    comp_month_max
  ) |>
  distinct() |>
  filter(comp_year %in% c(my_year, as.integer(my_year) - 1))

# dim(compl_override_data__renamed_m)
# dim(compl_override_data__renamed_m_short)

### add compliance/overridden combinations by week ----
# This is needed so that we can easily filter out compliant or non-compliant vessels in the dataset, by adding an extra column that states yes or no regarding compliance. The NA represents one of two possible scenarios:
# 1) a DNF was submitted for a vessel that is missing from the compliance module but is in metrics tracking, or
# 2) a DNF was submitted for a week when the vessel was not permitted. It is not simple to determine which. Deciding what to do with these DNFs will depend on the individual analysis question, and so is not addressed here, but simply left as NA.
## NOTE: IF “Is_Overriden == 1 & is_Comp == 0, then the vessel should be considered compliant in any compliance analyses

tic("get comp/overridden")
compl_override_data__renamed_m_short__compl_overr_by_week <-
  add_compliant_after_override(compl_override_data__renamed_m_short)
toc()
# get comp/overridden: 72.5 sec elapsed

# check all is_comp and overridden combinations
compl_override_data__renamed_m_short__compl_overr_by_week |>
    select(is_comp, overridden, compliant_after_override) |>
    distinct()
# 1       1          0 yes
# 2       0          0 no
# 3       0          1 yes
# 4       1          1 yes

### combine compliance by month ----

# Explanations:
# 1. Create a new data frame 'compl_override_data__renamed_m_short__m_compl'
# 2. Group the data by 'vessel_official_number', 'comp_year', and 'comp_month'.
# 3. Use 'mutate' to create a new column 'all_m_comp' containing a string representation of unique, sorted values of 'compliant_after_override'.
# 4. Use 'mutate' again to create a new column 'month_comp' based on conditions specified in 'case_when'.
#    - If 'all_m_comp' contains either "no, yes" or "no", set 'month_comp' to "non_compl". In other words, if at least one week of a month was non-compliant we consider the whole month as non-compliant.
#    - For all other cases, set 'month_comp' to "compl".
# 5. Use 'ungroup' to remove grouping from the data frame.

tic("get month_comp")
compl_override_data__renamed_m_short__m_compl__both_months <-
  compl_override_data__renamed_m_short__compl_overr_by_week |>
  group_by(vessel_official_number, comp_year, comp_month_min) |>
  mutate(all_m_comp_min =
           toString(unique(sort(
             compliant_after_override
           )))) |>
  mutate(month_comp_min =
           case_when(all_m_comp_min %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup() |>
    group_by(vessel_official_number, comp_year, comp_month_min) |>
  mutate(all_m_comp_min =
           toString(unique(sort(
             compliant_after_override
           )))) |>
  mutate(month_comp_min =
           case_when(all_m_comp_min %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup() |>

    group_by(vessel_official_number, comp_year, comp_month_max) |>
  mutate(all_m_comp_max =
           toString(unique(sort(
             compliant_after_override
           )))) |>
  mutate(month_comp_max =
           case_when(all_m_comp_max %in% c(c("no, yes"), "no") ~ "non_compl",
                     .default = "compl")) |>
  ungroup()
toc()
# get month_comp: 31.7 sec elapsed

# compl_override_data__renamed_m_short__m_compl |>
#     filter(!month_comp_min == month_comp_max) |>
#     glimpse()

# combine month compliance for each week
tic("min_max_compl")
compl_override_data__renamed_m_short__m_compl <-
  compl_override_data__renamed_m_short__m_compl__both_months |>
  group_by(vessel_official_number,
           comp_year,
           comp_week,
           comp_week_start_dt,
           comp_week_end_dt) |>
  mutate(
    common_month_compliance =
      case_when(
        month_comp_min == month_comp_max &
          month_comp_min == "compl" ~
          month_comp_min,
        .default = "non_compl"
      )
  ) |>
  ungroup()
toc()

# check
compl_override_data__renamed_m_short__m_compl |>
  select(contains("month")) |>
  # filter(!month_comp_min == month_comp_max) |>
  distinct() |>
  glimpse()

#   mutate(weeks_month_compl =
#            toString(unique(
#              month_comp_min, month_comp_max
#            ))) |>
#   ungroup() |>
#   filter(!weeks_month_compl == "compl") |> glimpse()
#   mutate(month_comp_min =
#            case_when(all_m_comp_min %in% c(c("no, yes"), "no") ~ "non_compl",
#                      .default = "compl")) |>
#


# check
# compl_override_data__renamed_m_short__m_compl |>
#     select(compliant_after_override, is_comp, overridden, all_m_comp, month_comp) |>
#     distinct()

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
            "SRHS_headboat_survey",
            str_glue("Vessel_List_{my_year}.csv"))

file.exists(srhs_2024_file_path)
# T

srhs_2024 <-
  read_csv(srhs_2024_file_path,
           col_types = cols(.default = 'c')) |>
  clean_headers()

# glimpse(srhs_2024)

## read sc permitted data ----
xlsx_name = r"(sc_mismatches\2024_04\scdnrFedVessels_04012024.xlsx)"

myfiles <-  file.path(my_paths$inputs, xlsx_name)

file.exists(myfiles)

SC_permittedVessels <- read_excel(
  myfiles,
  .name_repair = fix_names,
  guess_max = 21474836
)

# print_df_names(SC_permittedVessels)

# save the original data for future comparison
SC_permittedVessels_orig <- SC_permittedVessels

### fix dates in headers ----
# TODO: grab just the month of interest (depends on the SC file)
# grep for numbers only in the names and convert them

# save the not digit headers
not_date_names <-
  grep("^\\D+$", names(SC_permittedVessels), value = T)

# find all digit headers
date_names_raw <-
  grep("^\\d+$", names(SC_permittedVessels), value = T)

# Explanations:
# 1. Take the data frame 'date_names_raw'.
# 2. Use the function 'convertToDate()' to convert the Excel integer values to Date objects.
# 3. Use the 'format()' function to format the dates as "%m-%y", which represents month and year.
# 4. Assign the result to 'date_names_ok'.
date_names_ok <-
  date_names_raw |>
  convertToDate() |>
  format("%m-%y")

# combine the saved non-digit headers and the newly converted once
all_names <- c(not_date_names, date_names_ok)

# glimpse(all_names)

# apply the names to the DF
names(SC_permittedVessels) <-
  all_names

# check
# names(SC_permittedVessels)

dim(SC_permittedVessels)
# [1] 215  18

### change sc data format ----

# glimpse(SC_permittedVessels)

# Explanations:
# 1. 'SC_permittedVessels_longer' is created by reshaping the data frame 'SC_permittedVessels'.
# 2. The 'pivot_longer()' function from the 'tidyr' package is used to reshape the data from wide to long format.
# 3. All columns except those specified in "!c()" are pivoted.
# 4. The 'names_to' parameter specifies the name of the new column that will store the names of the original columns. Here, it's set to "month_year".
# 5. The 'values_to' parameter specifies the name of the new column that will store the values from the pivoted columns. Here, it's set to "delinquent_month".

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

glimpse(SC_permittedVessels_longer)

# Explanations:
# 1. 'SC_permittedVessels_longer_m_y' is created by processing the 'SC_permittedVessels_longer' data frame.
# 2. The 'separate_wider_delim()' function from the 'tidyr' package is used to separate the 'month_year' column into two separate columns ('month_sc' and 'year_sc') based on the specified delimiter '-'.
# 3. The 'names' parameter specifies the names for the new columns created by separation.
# 4. The 'delim' parameter specifies the delimiter used to separate the 'month_year' column.
# 5. The 'mutate()' function is used to modify the 'year_sc' column by pasteing "20" in front of each entry to ensure it's in a four-digit format. This is done, because we have the four-digit ("2024") format for a year in the compliance report.
# 6. The 'mutate()' function is applied across all columns containing month and year information ('month_sc' and 'year_sc') to convert them to numeric format. Again, for compatibility with the other data sets.
# 7. The 'distinct()' function is used to remove duplicate rows from the data frame.

SC_permittedVessels_longer_m_y <-
  SC_permittedVessels_longer |>
  separate_wider_delim(cols = month_year,
                       delim = "-",
                       names = c("month_sc", "year_sc")) |>
  mutate(year_sc = paste0("20", year_sc)) |>
  mutate(across(all_of(c("month_sc", "year_sc")), as.numeric)) |>
  distinct()

# glimpse(SC_permittedVessels_longer_m_y)

# SRHS: check and remove reports_to_srhs ----

# Join the SC data with the SRHS list by vessel
sc__srhs_join <-
  full_join(SC_permittedVessels_longer_m_y,
            srhs_2024,
            join_by(vessel_reg_uscg_ == uscg__))

# glimpse(sc__srhs_join)

# Get all the combinations of SC and SRHS list.
# In this results we have:
# 1               0 NA
# Both are not SRHS
# 2               1 Y
# Both are SRHS
# 3              NA Y
# The vessel is not in the SC list, which is expected.

# For this SC entry file there are no discrepancies, so we can simply remove all the vessel marked as reports_to_srhs from the future analysis. We don't have compliance information for them.

sc__srhs_join |>
  select(reports_to_srhs, is_insurvey) |>
  distinct()
#   reports_to_srhs is_insurvey
#             <dbl> <chr>
# 1               0 NA
# 2               1 Y
# 3              NA Y

# Kepp only non-SRHS vessels
SC_permittedVessels_longer_m_y_no_srhs <-
  SC_permittedVessels_longer_m_y |>
  filter(reports_to_srhs == 0)

# combine data ----

# Join the SC data with the compliance data we prepared, by vessel, month and year.
sc__fhier_compl__join_w_month <-
  left_join(
    SC_permittedVessels_longer_m_y_no_srhs,
    compl_override_data__renamed_m_short__m_compl,
    join_by(
      vessel_reg_uscg_ == vessel_official_number,
      between(month_sc, comp_month_min, comp_month_max),
      year_sc == comp_year,
    )
  )

dim(SC_permittedVessels)
# 215
dim(SC_permittedVessels_longer_m_y_no_srhs)
# SC_permittedVessels_longer_m_y_no_srhs
dim(sc__fhier_compl__join_w_month)
# 7927
n_distinct(SC_permittedVessels)
# 215
n_distinct(sc__fhier_compl__join_w_month$vessel_reg_uscg_)
# 207 (rm SRHS)
# glimpse(sc__fhier_compl__join_w_month)

dim(sc__fhier_compl__join_w_month)
# [1] 2588   14
# [1] 7927   19 (w weeks)

#
sc__fhier_compl__join_w_month |>
  select(contains("month")) |>
  distinct() |>
  filter(!month_comp_min == month_comp_max) |>
  glimpse()
# 44

# Answering the questions
# 1. SC non-compliant vessels that are also non-compliant in FHIER ----

# Keep vessels marked as non-compliant in both data sets
non_compliant_vessels_in_sc_and_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 1 &
           common_month_compliance == "non_compl") |>
  distinct()

dim(non_compliant_vessels_in_sc_and_fhier)
# 2
# 8 19 with weeks
# 12 24 counting non compliant in both month for straddling weeks

# Fewer column
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
  distinct() |>
  arrange(vessel_reg_uscg_, year_sc, comp_week_start_dt)

# glimpse(non_compliant_vessels_in_sc_and_fhier__for_output)

# 2. non compliant in SC and compliant in FHIER ----

# 2) if they are compliant for that month in FHIER then list all the dates of DNFs and/or logbooks we have in FHIER by vessel (probably 3 columns needed: vessel ID, Logbook (list any dates for that month), DNF (list week date range for any for that month)

test_vessels <- c("SC1663DK", "SC8348DB")

non_compliant_vessels_in_sc_and_compl_in_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 1 &
           common_month_compliance == "compl")

dim(non_compliant_vessels_in_sc_and_compl_in_fhier)
# 40 14
# [1] 172  19 w weeks
# 8 24

# Get month and weeks when the vessels are marked as non-compliant in SC, but are compliant in FHIER
non_compliant_vessels_in_sc_and_compl_in_fhier__m_w <-
  non_compliant_vessels_in_sc_and_compl_in_fhier |>
  select(vessel_reg_uscg_,
         delinquent_month,
         month_sc,
         comp_week,
         comp_week_start_dt,
         comp_week_end_dt,
         compliant_after_override) |>
  distinct() |>
  arrange(vessel_reg_uscg_, comp_week_start_dt)

# glimpse(non_compliant_vessels_in_sc_and_compl_in_fhier__m_w)

## add logbooks info ----
# Logbook (list any dates for that month)
# Get all logbooks info for this vessels filtered by month

logbooks__sc_fhier <-
  logbooks |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w,
    join_by(
      vessel_official_number == vessel_reg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    )
  )

# This is a good example of trip happens in Jan (1/30), in the week started in Jan and ended in Feb, hence it is marked as compliant in FHIER for February.
# glimpse(logbooks__sc_fhier)

logbooks__sc_fhier_for_output <-
  logbooks__sc_fhier |>
  select(
    vessel_official_number,
    delinquent_month,
    month_sc,
    trip_start_date,
    trip_end_date,
    comp_week_start_dt,
    comp_week_end_dt,
    vendor_app_name,
    trip_de,
    trip_ue
  ) |>
  distinct() |>
  arrange(vessel_official_number, trip_start_date)

# glimpse(logbooks__sc_fhier_for_output)

## add DNF info ----
# DNF (list week date range for any for that month)
dnfs__sc_fhier <-
  dnfs |>
  inner_join(
    non_compliant_vessels_in_sc_and_compl_in_fhier__m_w,
    join_by(
      vessel_official_number == vessel_reg_uscg_,
      comp_week_start_dt,
      comp_week_end_dt
    ),
    suffix = c("__dnf", "__fhier")
  )

dim(dnfs__sc_fhier)
# 0

dnfs__sc_fhier_for_output <-
  dnfs__sc_fhier |>
  select(
    vessel_official_number,
    comp_week_start_dt,
    comp_week_end_dt,
    compliant_after_override__fhier
  ) |>
  distinct() |>
  arrange(vessel_official_number, comp_week_start_dt)

# dim(dnfs__sc_fhier_for_output)
# 0

# 3. SC compliant and not compliant in FHIER ----

# 3) we also need a step that just grabs the compliant vessels (herein "SC compliant vessels list"), and then checks FHIER compliance to see if any that SC has as compliant are listed as non-compliant for any of the weeks in the given month. If any vessels are found to be compliant with SC but non-compliant with us/FHIER, then we need (on a 3rd sheet) to list those vessels and include what week (with date ranges) we are missing in FHIER. Eric will use this to more proactively alert us when a vessel is reporting only to SC, since we have so many recurring issues with this.

compliant_vessels_in_sc_and_non_compl_fhier <-
  sc__fhier_compl__join_w_month |>
  filter(delinquent_month == 0 &
           common_month_compliance == "non_compl")

dim(compliant_vessels_in_sc_and_non_compl_fhier)
# [1] 180  14
# [1] 739  19 with weeks
# [1] 758  19 with all non compl months
# [1] 1002   24
# glimpse(compliant_vessels_in_sc_and_non_compl_fhier)

# "all_m_comp" field shows if any weeks of that month were compliant. We consider the whole month non-compliant if even one week was non-compliant. If SC consider the month compliant if at least one week was compliant that makes the big difference in the monthly compliance counts between SC and FHIER.

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
    compliant_after_override,
    common_month_compliance
  ) |>
  filter(compliant_after_override == "no") |>
  distinct() |>
  arrange(vessel_reg_uscg_, comp_week_start_dt)

dim(compliant_vessels_in_sc_and_non_compl_fhier__for_output)
# [1] 394   9
# [1] 406   10
# [1] 474  10

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

output_df_list <-
  lst(
    non_compliant_vessels_in_sc_and_fhier__for_output,
    logbooks__sc_fhier_for_output,
    dnfs__sc_fhier_for_output,
    compliant_vessels_in_sc_and_non_compl_fhier__for_output
  )

sheet_names <-
  c(
    "non_compl_sc_and_fhier",
    "non_compl_sc__compl_fhier_lgb",
    "non_compl_sc__compl_fhier_dnf",
    "compl_sc__non_compl_fhier"
  )

# map_dfr(output_df_list,
#         ~ names(.x) %>% as_tibble())

colnames_for_each_df <-
  map2(output_df_list,
       sheet_names,
       \(my_df, sheet_name) {
         browser()

         print(sheet_name)

         df_names <-
           names(my_df) |> as.tibble()

         # output_df_list[[idx]] |> substitute() |> deparse()
       })


names(colnames_for_each_df) <- sheet_names

sheet_names_with_df_names <-
  cbind(sheet_names, names(output_df_list)) |>
  as_tibble()

# str(sheet_names_with_df_names)

names(sheet_names_with_df_names) <- c("Sheet name", "What is inside")

top_of_read_me_text <-
  list(today(),
       "Sheet definition:") |>
  as_tibble_col(column_name =  "Read me")


# TODO include headers definitions
readme_text <-
  c(
    top_of_read_me_text,
    sheet_names_with_df_names,
    colnames_for_each_df
  # )
  )
# |>
#   as.data.frame() |>
#   t()

# str(readme_text)
# colnames(readme_text) <- "Read.me"

result_list <-
  list(
    "readme" = readme_text
    # ,
    # output_df_list
  )

openxlsx::write.xlsx(result_list, file = output_file_name)
