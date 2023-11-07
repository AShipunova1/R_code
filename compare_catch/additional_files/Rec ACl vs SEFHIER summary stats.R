#----General Notes on how code works ----

# Input data
# https://drive.google.com/drive/u/0/folders/1T0DqxoaNFOIsU-bYL-sizulMUFkANIJ2

# (1) SEFHIER data
# "FHIER_all_logbook_data.csv"

# (2) MRIP : "O:\Fishery Data\ACL Data\FES_Rec_data(mail_survey)\MRIP_FES_rec81_22wv6_01Mar23\MRIP_FES_rec81_22wv6_01Mar23w2014to2021LACreel.xlsx"

# 2022 only: local mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel_2022.xlsx

## MRIP data: field background info ----
# landing	Total Harvest (A+B1)	The total number of fish removed from the fishery resource.  May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
# tot_cat	Total Catch (A+B1+B2)	The number of fish caught but not necessarily brought ashore.  May be obtained by summing catch types A (CLAIM), B1 (HARVEST), and B2 (RELEASE).

# sp_code	Species = ITIS_CODE (SA or GOM_LABEL has common name)

# sub_reg	Region	" Subregion code for region of trip
# 4   = North Atlantic (ME; NH; MA; RI; CT)
# 5   = Mid-Atlantic (NY; NJ; DE; MD; VA)
# 6   = South Atlantic (NC; SC; GA; EFL)
# 7   = Gulf of Mexico (WFL; AL; MS; LA; TX)
#SRHS: 6=Atlantic (NC-FL Keys areas 1-17), 7=Gulf of Mexico (Dry Tortugas-TX areas 18-29)
#TPWD and LA CREEL: 7=Gulf of Mexico
# 8   = West Pacific (HI)
# 11 = U. S. Caribbean (Puerto Rico and Virgin Islands"

# Use all
# area_x	Fishing Area	" Collapsed area of fishing
# 1 = Ocean <= 3 mi (all but WFL)
# 2 = Ocean > 3 mi (all but WFL)
# 3 = Ocean <= 10 mi (WFL only)
# 4 = Ocean > 10 mi (WFL only)
# 5 = Inland"	CHAR

# Use DS column to dplyr::filter out SRHS (headboat)

# Use "new mode" column to dplyr::filter out private and shore modes (private = rec;
#            shore mode = private rec fishing from shore)
# new_mode = recorded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)

# ab1			type A + type B1 catch estimate (number of fish killed or kept)

#general set up:
#load required packages; or install first if necessary
library(tidyverse) #Collection of packages (visualization, manipulation): ggplot2, dplyr, purrr, etc.
library(readxl) # to read in XL files
library(zoo) #converting dates
library(magrittr) #for data piping (%<>% allows piping in both direction)
# to combine plots
library(gridExtra)
library(grid)
# add color palettes
library(viridis)

# set working directories
  # change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "/SEFHIER/R code/Rec ACL vs SEFHIER stats/"

  in_dir <- "Inputs"
  # file.path instead of paste, because it provides correct concatenation, "\" or "/" etc.
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "Outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # git_r_dir <- "R_code_github"
  # full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir) #,
                   #"git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

# call that function
my_paths <- set_work_dir()


# auxiliary functions (called throughout the code) ----

# count unique values
count_uniq_by_column <- function(my_df) {
    sapply(my_df, function(x) length(unique(x))) %>%
    as.data.frame()
}

# quick look at the data structure
data_overview <- function(my_df) {
  # each column
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  # call to the next function
  count_uniq_by_column(my_df)
}

# trim col_name_to_trim in csvs_clean_ws
# E.g. vesselofficialnumber (default), there are 273 white spaces in Feb 2023
trim_all_vessel_ids_simple <-
  function(csvs_clean_ws, col_name_to_trim = NA) {
    csvs_clean <- lapply(csvs_clean_ws, function(x) {
      if (is.na(col_name_to_trim)) {
        col_name_to_trim <- grep("vessel.*official.*number",
                                 tolower(names(x)),
                                 value = T)
      }
      # convert the parameter string to a symbol
      col_name_to_trim_s <- sym(col_name_to_trim)
      # Hard code vessel_official_number as vessel id
      x %>%
        # trim white spaces from the column
        dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
        return()
    })
    return(csvs_clean)
  }

# use fix_names on column names of my_df and assign the result back
clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    fix_names()
  return(my_df)
}

# to use in a function,
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  x %>%
    # remove dots
    str_replace_all("\\.", "") %>%
    # all not letters and numbers to underscores
    str_replace_all("[^A-z0-9]", "_") %>%
    # letters only in the beginning (move underscores)
    str_replace_all("^(_*)(.+)", "\\2\\1") %>%
    # tolower
    my_headers_case_function()
}

# cleaning, regularly done for csvs downloaded from PHIER
clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
  # unify headers (see the function above)
  csvs_clean0 <- lapply(csvs, clean_headers)
  # trim vessel_official_number col, in case there are trailing spaces
  csvs_clean1 <-
    trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
  return(csvs_clean1)
}

#use function to get local path and then read in the input files
load_csv_names <- function(my_paths, csv_names_list) {
  # use local paths from above
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <-
    lapply(csv_names_list, function(x)
      file.path(my_inputs, x))
  # read all csv files with columns as character (change later to other types)
  contents <-
    lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  return(contents)
}

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

#end auxiliary functions --------------------


# Load all data ----
# ---- 1) SEFHIER data ----

#this function loads all FHIER data files from the main dir into R
load_all_logbooks <- function() {
  #load species_list.csv from Inputs folder
  #r"()" - will correct slashes, so I can "copy path" from the finder and paste it inside the parenthesis without worrying about "\" or "//" etc.
  #species_count_csv_names_list = c(r"(logbooks_from_fhier\FHIER_all_logbook_data.csv)")
  species_count_csv_names_list = c(r"(FHIER_all_logbook_data.csv)")

  fhier_all_logbook_data <-
    load_csv_names(my_paths, species_count_csv_names_list)

  logbooks_content <-
    # see an aux function above
    clean_all_csvs(fhier_all_logbook_data,
                   vessel_id_field_name = "vessel_official_nbr")

  return(logbooks_content[[1]])
}

# call the function
logbooks_content <- load_all_logbooks()

# ---- 2) ACL (Annual Catch Limits surveys) (MRIP) ----
load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {
  # use prepared paths from above
  my_inputs <- my_paths$inputs

  # add input directory path in front of each file name.
  myfiles <-
    lapply(xls_names_list, function(x) file.path(my_inputs, x))

  ## read all files
  contents <- map_df(
    myfiles,
    ~ read_excel(
      # file name
      .x,
      sheet = sheet_n,
      # use my fix_names function for col names
      .name_repair = fix_names,
      guess_max = 21474836,
      # read all columns as text
      col_types = "text"
    )
  )
  return(contents)
}

load_acl_data <- function() {
  #my MRIP data is in my Path already, in Inputs, so skip lines 226-228
  # # my special dir with data
  # acl_dir_path <- "compare_catch/MRIP data"
  # species_list.csv is identical for all areas
  acl_csv_names_list_raw <- c("MRIPspecies_list.csv")
  # a file recommended by Mike
  acl_xls_names_list_raw <-
    c(r"(mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel.xlsx)")

  # add prefix to each file name if needed
  # acl_csv_names_list <-
  #   map_chr(acl_csv_names_list_raw, ~ file.path(acl_dir_path, .x))
  # acl_xls_names_list <-
  # map_chr(acl_xls_names_list_raw, ~ file.path(acl_dir_path, .x))

  # use functions from above
  acl_species_list <-
    load_csv_names(my_paths, acl_csv_names_list_raw)

  acl_estimate_usa <-
    load_xls_names(my_paths, acl_xls_names_list_raw,
                   sheet_n = "mripaclspec_rec81_22wv6_01mar23")

  output <-
    list(acl_species_list, acl_estimate_usa)
  return(output)
}

# run the function, get the list of dfs
acl_temp <- load_acl_data()

acl_species_list <- acl_temp[[1]]
acl_estimate <- acl_temp[[2]]

# data preparation ----
## prepare FHIER data ----

## get column names vars ----
# There are different formats in different available files.
# Find a column name with "itis" in it
itis_field_name <- grep("itis", names(logbooks_content), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content), value = T)
# vessel_official_nbr

## Fix dates ----
# Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"
change_to_dates <- function(my_df, field_name, date_format) {
  my_df %>%
    dplyr::mutate({
      {
        field_name
      }
    } := as.POSIXct(pull(my_df[field_name]),
                    format = date_format)) %>%
    return()
}

fhier_logbooks_content <-
  logbooks_content %>%
  # create a new column
  dplyr::mutate(trip_start_date_time =
           # trip start: combine a date without time, a space and a time
           paste(substr(trip_start_date, 1, 10),
                 trip_start_time)) %>%
  # Same for the trip end
  dplyr::mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
  # change the new column types to a date using the function from above
  change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
  change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
  # change this column type to a number
  dplyr::mutate(reported_quantity = as.integer(reported_quantity))

## view the result
# fhier_logbooks_content %>% dplyr::select(starts_with("trip")) %>% str()

### fix wrong dates ----
fhier_logbooks_content_date_fixed_tmp <-
  fhier_logbooks_content %>%
  # if a "trip_end_date" is before 2020 - use "notif_trip_end_date" column instead
  dplyr::mutate(trip_end_date1 = ifelse(
    trip_end_date < "2020-01-01",
    notif_trip_end_date,
    trip_end_date
  ))

#some logbooks have 1992 end dates, correct those here by converting 1992 to 2022
fhier_logbooks_content_date_fixed <-
  fhier_logbooks_content_date_fixed_tmp %>%
  # manually change the wrong value
  dplyr::mutate(trip_end_date2 = ifelse(
    # find it
    grepl(
      "1992",
      fhier_logbooks_content_date_fixed_tmp$trip_end_date1
    ),
    # change it
    "2022-10-16 01:00:00",
    # don't change anything else
    trip_end_date1
  ))

### keep only 2022 ---- note: year() is a fxn from package "tidyverse"
fhier_logbooks_content_date_fixed %<>%
  dplyr::filter(year(trip_end_date) == "2022")

## add waves column to FHIER DF----
fhier_logbooks_content_waves <-
  fhier_logbooks_content_date_fixed %>%
  # add a new column with a trip end Month
  dplyr::mutate(end_month = as.yearmon(trip_end_date2)) %>%
  # add a new column with a trip end Year
  dplyr::mutate(end_year =
           year(trip_end_date2)) %>%
  # add a new column with a number for each trip end Month
  dplyr::mutate(end_month_num = month(trip_end_date2)) %>%
  # add a new column with a Wave number
  dplyr::mutate(end_wave  = floor((end_month_num + 1) / 2))

#| classes: test
# test: show the new columns ----
fhier_logbooks_content_waves %>%
  dplyr::select(end_month, end_year, end_month_num, end_wave) %>%
  unique() %>%
  # sort by end_month_num
  dplyr::arrange(end_month_num)

## add region info ----
# Create variable that organize Florida counties into Gulf or SA regions (from the Internet) ----
fl_counties <- list(
  "SA" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "GOM" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",  #may need to revise monroe catch into Gulf or SA region, based on Mike Larkin's input
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

# Florida regions
fhier_logbooks_content_waves_fl_county <-
  fhier_logbooks_content_waves %>%
  # create a new column "end_port_fl_reg" with SA, GOM or whatever else left
  dplyr::mutate(
    end_port_fl_reg = dplyr::case_when(
      # check in the list
      # if there is no end county, use the start
      fix_names(start_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(start_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      # if not on the list - keep it
      .default = end_port_county
    )
  )

## test: check Florida regions ----
fhier_logbooks_content_waves_fl_county %>%
  # get FL only
  dplyr::filter(end_port_state == "FL") %>%
  # sort by county
  dplyr::arrange(end_port_county) %>%
  dplyr::distinct() %>%
  # data_overview()
  # 37 counties
  # vessel_official_number          1096
  dplyr::select(end_port_fl_reg) %>%
  table()

## now convert the Other states' counties to regions ----
# list of states in the South Atlantic region (from the Internet)
# https://safmc.net/about/#:~:text=The%20South%20Atlantic%20Council%20is,east%20Florida%20to%20Key%20West
# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    #"Delaware",
    #"District of Columbia",
    # "Florida", # exclude, we have it separated by county
    "Georgia",
    #"Maryland",
    "North Carolina",
    "South Carolina"#,
    #"Virginia",
    #"West Virginia"
  )
)

# Reformat the R state df (create a DF of state abbreviations and state names as cols; 2x50)
state_tbl <- data.frame(state.abb, tolower(state.name))
names(state_tbl) = c("state_abb", "state_name")

#from the DF created in lines 445-446, only grab the SA states defined in lines 430-442
sa_state_abb <-
  # a table from above
  state_tbl %>%
  # get only these in our list
  dplyr::filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  dplyr::select(state_abb)

# add regions to the FHIER logbook DF
fhier_logbooks_content_waves__sa_gom <-
  fhier_logbooks_content_waves_fl_county %>%
  # add a new column "end_port_sa_gom" with sa or gom for each state
  # use fix_name aux function to unify state names (lower case, no spaces etc.)
  dplyr::mutate(end_port_sa_gom = dplyr::case_when(
    # if a name is in our SA list - "sa", otherwise - "gom"
    fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
    .default = "gom"
  )) %>%
  # go through the new column again
  # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
  # otherwise don't change
  dplyr::mutate(end_port_sa_gom = ifelse(
    tolower(end_port_state) == "fl",
    end_port_fl_reg,
    end_port_sa_gom
  )) %>%
  # remove this column, we don't need it anymore
  dplyr::select(-end_port_fl_reg)

## test: check new cols of states and regions ----
fhier_logbooks_content_waves__sa_gom %>%
  # look at states and regions
  dplyr::select(end_port_state, end_port_sa_gom) %>%
  unique() %>%
  dplyr::glimpse()

# dplyr::glimpse(fhier_logbooks_content_waves__sa_gom)

## combine dolphin and dolphinfish for FHIER data ----
fhier_logbooks_content_waves__sa_gom_dolph <-
  fhier_logbooks_content_waves__sa_gom %>%
  rename(common_name_orig = common_name) %>%
  dplyr::mutate(common_name = if_else(
    tolower(common_name_orig) %in% c("dolphin", "dolphinfish"),
    "DOLPHIN",
    common_name_orig
  ))

# dplyr::glimpse(fhier_logbooks_content_waves__sa_gom_dolph)

### test: dolphins to ensure they now have the same common name in new "common_name" col----
fhier_logbooks_content_waves__sa_gom_dolph %>%
  dplyr::filter(tolower(common_name_orig) %in% c("dolphin", "dolphinfish")) %>%
  dplyr::select(common_name_orig, common_name) %>% unique()
# ---

## calculate catch ----

#select only relevant columns from FHIER logbooks DF, and group all cols by reported quantity
fhier_catch_by_species_state_region_waves <-
  fhier_logbooks_content_waves__sa_gom_dolph %>%
  dplyr::select(
    catch_species_itis,
    common_name,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave,
    reported_quantity
  ) %>%
  # group by all of them but "reported_quantity"
  dplyr::group_by(
    catch_species_itis,
    common_name,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave
  ) %>%
  # save a sum of reported_quantity in each group in fhier_quantity_by_4
  # remove NAs
  summarise(fhier_quantity_by_4 = sum(as.integer(reported_quantity),
                                      na.rm = TRUE)) %>%
  as.data.frame()

# data_overview(fhier_catch_by_species_state_region_waves)

### test: cnts for 1 sp. ----
test_species_itis <-
  fhier_logbooks_content %>%
  dplyr::filter(tolower(common_name) == "mackerel, spanish") %>%
  dplyr::select(catch_species_itis) %>%
  unique() %>%
  # get a string, not a df
  use_series(catch_species_itis)

#test: cnts for 1 sp. in both regions (GOM vs SA)
fhier_test_cnts <-
  fhier_catch_by_species_state_region_waves %>%
  # get the same species
  dplyr::filter(catch_species_itis == test_species_itis) %>%
  # group by region
  dplyr::group_by(catch_species_itis, end_port_sa_gom) %>%
  # sum the FHIER catch
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## ACL data preparations ----

## specifically for "O:\Fishery Data\ACL Data\" ----

#in case read.xls or .csv created a extra value, or if you read data in as character,
 #then use integer() here to ensure all values in the ab1 col are now actually numbers
acl_estimate %<>%
  # using ab1 for catch counts
  # convert to numbers
  dplyr::mutate(ab1 = as.integer(ab1))

# str(acl_estimate)

### dplyr::filtering ----
acl_estimate_2022 <-
  acl_estimate %>%
  dplyr::filter(year == "2022") %>%
  # dplyr::filtering here for just SA (6) and Gulf (7) sub regions
  dplyr::filter(sub_reg %in% c(6, 7)) %>%
  # Exclude the SRHS survey according to Dominique and Mike May 1, 2023
  dplyr::filter(!(ds == "SRHS")) %>%
  # dplyr::select(new_mode) %>% unique()
  # the "new_mode" column only has options 1,3 & 4 remaining
  # -	New variable ‘agg_moden’ divides all estimates into for-hire (cbt, hbt, or cbt/hbt) or private (private or shore) mode fishing
  # new_mode	recoded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)
  # new_moden		alpha description of ‘new_mode’
  dplyr::filter(new_mode %in% c(2, 3, 5))

#just for checking we actually dplyr::filtered the raw data
  # View(acl_estimate)
  dim(acl_estimate)
  # [1] 347379 67
  dim(acl_estimate_2022)
  # 1442

### sum catch by state, region and wave ----
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_2022 %>%
  # dplyr::select the relevant columns only
  dplyr::select(itis_code, new_sta, sub_reg, year, wave, ab1) %>%
  # group by all except the counts
  dplyr::group_by(itis_code, new_sta, sub_reg, year, wave) %>%
  # save the sum of "ab1" for each group in "acl_estimate_catch_by_4"
  # remove NAs
  summarise(acl_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
  # back to an ungrouped form
  as.data.frame()

# dplyr::glimpse(acl_estimate_catch_by_species_state_region_waves)
# 'data.frame':	878 obs. of  6 variables

# convert "year" and "wave" to numbers (were <chr> in acl_estimate_catch_by_species_state_region_waves)
acl_estimate_catch_by_species_state_region_waves1 <-
  acl_estimate_catch_by_species_state_region_waves %>%
  dplyr::mutate(year = as.double(year)) %>%
  dplyr::mutate(wave = as.double(wave))

## change regions to the same format as in FHIER ----
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_catch_by_species_state_region_waves1 %>%
  # change a 6 to "sa" and a 7 "gom", leave everything else in place
  dplyr::mutate(
    sa_gom = dplyr::case_when(sub_reg == "6" ~ "sa",
                       sub_reg == "7" ~ "gom",
                       .default = sub_reg),
    # put the new column after sub_reg (by default at the end)
    .after = sub_reg
  ) %>%
  # drop sub_reg
  dplyr::select(-sub_reg)

### make a test acl cnts DF of just one sp.----
# names(acl_estimate_catch_by_species_state_region_waves)
acl_test_cnts <-
  acl_estimate_catch_by_species_state_region_waves %>%
  # get one species
  dplyr::filter(itis_code == test_species_itis) %>%
  # group by region
  dplyr::group_by(itis_code, sa_gom) %>%
  # sum the ACL catch
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## rename fields to compare to FHIER----

# common field names
wave_data_names_common <- c("species_itis",
                            "state",
                            "sa_gom",
                            "year",
                            "wave")

# to be sure columns are in the same order
names(acl_estimate_catch_by_species_state_region_waves)

#rename cols of ACL DF so we can compare to FHIER DF
acl_names <- c("itis_code",
               "new_sta",
               "sa_gom",
               "year",
               "wave",
               "acl_estimate_catch_by_4")

# rename in place
acl_estimate_catch_by_species_state_region_waves %<>%
  # rename the first 2 columns from the list
  rename_at(vars(acl_names[1:2]),
            function(x) wave_data_names_common[1:2])

fhier_names <- c(
  "catch_species_itis",
  "common_name",
  "end_port_state",
  "end_port_sa_gom",
  "end_year",
  "end_wave",
  "fhier_quantity_by_4"
)

# names(fhier_catch_by_species_state_region_waves)
# rename in place
fhier_catch_by_species_state_region_waves %<>%
  # rename the first and columns 3 to 6 from the list
  rename_at(vars(fhier_names[c(1, 3:6)]),
            function(x) wave_data_names_common[1:5])

### rename fields in the test variables ----
names(fhier_test_cnts) <-
  c("species_itis", "sa_gom", "mackerel_fhier_cnt")  # was: "catch_species_itis" "end_port_sa_gom"    "mackerel_fhier_cnt"

### test: rename fields ----
names(fhier_catch_by_species_state_region_waves) #col names same order as ACL?
names(acl_estimate_catch_by_species_state_region_waves) #col names same order as FHIER?
#use indentical() to check they are the same
identical(
  names(fhier_catch_by_species_state_region_waves)[c(1, 3:6)],
  names(acl_estimate_catch_by_species_state_region_waves)[1:5]
)  # RESULT = TRUE

## All FHIER common names and itis in a separate data frame ----
fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  dplyr::select(catch_species_itis, common_name) %>%
  unique()

# add column names
names(fhier_common_names) <- c("species_itis", "common_name")

## Join Fhier and ACL DFs----
fhier_acl_catch_by_species_state_region_waves <-
  # use a "full" join to keep entries from each df even if there is no counterpart in another one
  full_join(
    fhier_catch_by_species_state_region_waves,
    acl_estimate_catch_by_species_state_region_waves,
    # have to specify columns to join by, because some other columns might have the same name, but different meaning, e.g common_name
    by = join_by(species_itis, state, sa_gom, year, wave)
  )

## Change NA counts to 0 ----
# change NAs to 0 where one or another agency doesn't have counts for this species (discussed if it is better than simply remove the entries)
fhier_acl_catch_by_species_state_region_waves %<>%
  dplyr::mutate(
    fhier_quantity_by_4 =
      replace_na(fhier_quantity_by_4, 0),
    acl_estimate_catch_by_4 =
      replace_na(acl_estimate_catch_by_4, 0)
  )

### test join ----
# look at the first 20 entries for mackerel spanish
fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>% head(20)

### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----

fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(fhier_test_cnts$mackerel_fhier_cnt)  #[1] TRUE

fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(species_itis, sa_gom) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_acl_cnt) %>%
  identical(acl_test_cnts$mackerel_acl_cnt)  #[1] TRUE

# Data by 12 categories ----
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state

# 3 sets of spp:
# 1a) SEDAR;
# 2b) Recreational ACL tops;
# 3c) All FHIER spp
