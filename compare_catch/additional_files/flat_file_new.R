#----General Notes on how code works ----

# Input data
# https://drive.google.com/drive/u/0/folders/1T0DqxoaNFOIsU-bYL-sizulMUFkANIJ2

# (1) SEFHIER data
# (a) "FHIER_all_logbook_data.csv"
# https://drive.google.com/file/d/1eQdbGXR8PXx_8ZJ5SARP0yCo3vr-7Dgy/view?usp=share_link

# (b) SEFHIER_species.xlsx
# https://docs.google.com/spreadsheets/d/1xLxC9CyDPcttP8JUXO6S4AtIIIqoYMgt/edit?usp=share_link&ouid=104137968095602765842&rtpof=true&sd=true
# needed to join with MRIP data

# (2) MRIP
# (a)
# Recommended by Mike May 11, 2023. This file doesn't have ITIS for all spp., use scientific names to compare with FHIER
# MRIP_FES_rec81_22wv6_01May23w2014to2022LACreel.xlsx
# https://docs.google.com/spreadsheets/d/1YpfKfYFyUm3dWY7PK0XbEfUAM3onsnys/edit?usp=share_link&ouid=104137968095602765842&rtpof=true&sd=true

# (b) mrip_species_list.csv
# https://drive.google.com/file/d/1naWQtliI-5ke8jLz2Q-ZvICMuB99B0Ho/view?usp=share_link

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

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories
  # change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()

  # for others
  add_dir <- ""
  # for Anna's computer
  if (get_username() == "anna.shipunova") {
    add_dir <- "R_files_local/test_dir"
  }

  # add an empty or Anna's folder in front
  main_r_dir <- file.path(add_dir, "SEFHIER/R code/Rec ACL vs SEFHIER stats/")

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

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

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

# use fix_names on column names of my_df and assign the result back
clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    fix_names()
  return(my_df)
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

# end auxiliary functions ----

# Load all data ----
## 1) SEFHIER data ----

load_all_logbooks <- function() {
  #load FHIER_all_logbook_data.csv from Inputs folder
  species_count_csv_names_list = c("FHIER_all_logbook_data.csv")

  fhier_all_logbook_data <-
    load_csv_names(my_paths, species_count_csv_names_list)

  logbooks_content <-
    # see an aux function above
    clean_all_csvs(fhier_all_logbook_data,
                   vessel_id_field_name = "vessel_official_nbr")

  return(logbooks_content[[1]])
}

# use functions from above
logbooks_content <- load_all_logbooks()

### load sefhier spp. ----
#r"()" - will correct slashes, so I can "copy path" from the finder and paste it inside the parenthesis without worrying about "\" or "//" etc.

sefhier_sp_file_name <- "SEFHIER_species.xlsx"
sheet_name <- "Species Tree"

sefhier_sp_all <-
  # see functions above
  load_xls_names(my_paths, c(sefhier_sp_file_name),
                 sheet_n = sheet_name)

## 2) MRIP rec ACL (Annual Catch Limit surveys) ----
load_acl_data <- function() {
  acl_csv_names_list <- c(
    "mrip_species_list.csv" # identical for all areas
  )
  # a file recommended by Mike
  acl_xls_names_list <- c("MRIP_FES_rec81_22wv6_01May23w2014to2022LACreel.xlsx")

  acl_species_list <- load_csv_names(my_paths, acl_csv_names_list)

  # str(acl_species_list)

  acl_estimate_usa <-
    load_xls_names(my_paths, acl_xls_names_list,
                   sheet_n = "MRIP_FES_rec81_22wv6_01May23")

    output <- list(acl_species_list, acl_estimate_usa)
  return(output)
}

# run the function, get the list of dfs
acl_temp <- load_acl_data()

acl_species_list <- acl_temp[[1]]
acl_estimate <- acl_temp[[2]]

# data_overview(acl_estimate)

# Data preparation ----
## Prepare FHIER data ----

### use only entries with sero_vessel_permit ----
logbooks_content_sero <-
  logbooks_content %>%
  dplyr::filter(!is.na(sero_vessel_permit))

### get column names vars ----
# There are different formats in different available files.
# Find a column name with "itis" in it
itis_field_name <- grep("itis", names(logbooks_content_sero), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content_sero), value = T)
# vessel_official_nbr

### Fix dates ----
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
  logbooks_content_sero %>%
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

### keep only 2022 ---- note: year() is a function from package "tidyverse"
fhier_logbooks_content_date_fixed %<>%
  dplyr::filter(lubridate::year(trip_end_date) == "2022")

### add waves column to FHIER DF ----
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
#### test: show the new columns ----
fhier_logbooks_content_waves %>%
  dplyr::select(end_month, end_year, end_month_num, end_wave) %>%
  unique() %>%
  # sort by end_month_num
  dplyr::arrange(end_month_num)

### add region info ----
#### Create variable that organize Florida counties into Gulf or SA regions (from the Internet) ----
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
    end_port_fl_reg = case_when(
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

#### test: check Florida regions ----
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

### now convert the Other states' counties to regions ----
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
  dplyr::mutate(end_port_sa_gom = case_when(
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

#### test: check new cols of states and regions ----
fhier_logbooks_content_waves__sa_gom %>%
  # look at states and regions
  dplyr::select(end_port_state, end_port_sa_gom) %>%
  unique() %>%
  dplyr::glimpse()

### add scientific names ----
sefhier_spp <-
  sefhier_sp_all %>%
  dplyr::select(species_itis, scientific_name, common_name) %>%
  unique()

fhier_logbooks_content_waves__sa_gom %<>%
  # rename a column
  rename(species_itis = catch_species_itis)

#### add scientific names to fhier data ----
fhier_catch_by_species_state_region_waves_w_spp <-
full_join(fhier_logbooks_content_waves__sa_gom,
          sefhier_spp,
          by = join_by(species_itis, common_name))

# check if sci name is a NA
fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(is.na(scientific_name)) %>%
  dplyr::select(species_itis, common_name, reported_quantity) %>%
  dplyr::group_by(species_itis, common_name) %>%
  summarise(sum_cnt = sum(reported_quantity)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(sum_cnt)) %>%
  head()
# 1 172734       FLOUNDERS, PARALICHTHYS    3007
# Taxonomic Serial No.: 172734
# Genus	Paralichthys Girard, 1858 – Summer flounders, southern flounders
# many spp
# https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=172734#null

### check DOLPHIN ----
grep("DOLPHIN", fhier_catch_by_species_state_region_waves_w_spp$common_name, value = T, ignore.case = T) %>%
  unique()

# 3 in FHIER
fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(grepl("DOLPHIN", fhier_catch_by_species_state_region_waves_w_spp$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name      species_itis scientific_name
# 1 DOLPHINFISH      168790       CORYPHAENA
# 2 DOLPHIN          168791       CORYPHAENA HIPPURUS
# 3 DOLPHIN, POMPANO 168792       CORYPHAENA EQUISETIS

# 1 in MRIP
grep("CORYPHAENA", acl_estimate$new_sci, value = T, ignore.case = T) %>%
  unique()
# [1] "Coryphaena hippurus"

### combine dolphin and dolphinfish for FHIER data ----
fhier_catch_by_species_state_region_waves_w_spp_dolph <-
  fhier_catch_by_species_state_region_waves_w_spp %>%
  # "save" the original column
  rename(common_name_orig = common_name) %>%
  # rename all DOLPHINs to "DOLPHIN"
  dplyr::mutate(common_name = case_when(startsWith(tolower(common_name_orig), "dolphin") ~ "DOLPHIN",
                                 .default = common_name_orig)) %>%
  # the same for scientific names
  rename(scientific_name_orig = scientific_name) %>%
  # as in MRIP
    dplyr::mutate(scientific_name = case_when(startsWith(
    tolower(scientific_name_orig), "coryphaena"
  ) ~ "CORYPHAENA HIPPURUS",
  .default = scientific_name_orig))

### test: dolphins to ensure they now have the same common and scientific names
fhier_catch_by_species_state_region_waves_w_spp_dolph %>%
  # dplyr::filter(tolower(common_name_orig) %in% c("dolphin", "dolphinfish")) %>%
  dplyr::filter(startsWith(tolower(common_name_orig), "dolphin")) %>%
  dplyr::select(common_name_orig, common_name, scientific_name) %>% unique()

### add sci name for FLOUNDERS, PARALICHTHYS for FHIER data ----

# rename back
fhier_catch_by_species_state_region_waves_w_spp <-
fhier_catch_by_species_state_region_waves_w_spp_dolph

fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(grepl("FLOUNDERS", fhier_catch_by_species_state_region_waves_w_spp$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name             species_itis scientific_name
# 1 FLOUNDERS, PARALICHTHYS 172734       NA

fhier_logbooks_content_waves__sa_gom_fla <-
  fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::mutate(scientific_name = ifelse(
    is.na(scientific_name) & species_itis == "172734",
    "PARALICHTHYS",
    scientific_name
  ))

#### test: FLOUNDERS ----
fhier_logbooks_content_waves__sa_gom_fla %>%
  dplyr::filter(grepl("FLOUNDERS", fhier_logbooks_content_waves__sa_gom_fla$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name             species_itis scientific_name
# 1 FLOUNDERS, PARALICHTHYS 172734       PARALICHTHYS

## Calculate catch ----

#select only relevant columns from FHIER logbooks DF, and group all cols by reported quantity
fhier_catch_by_species_state_region_waves <-
  fhier_logbooks_content_waves__sa_gom_fla %>%
  dplyr::select(
    scientific_name,
    species_itis,
    common_name,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave,
    reported_quantity
  ) %>%
  dplyr::group_by(
    scientific_name,
    species_itis,
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

#| classes: test
### test: cnts for 1 sp. ----
test_species_name <-
  fhier_catch_by_species_state_region_waves %>%
  # dplyr::filter(tolower(common_name) == "mackerel, spanish") %>%
  dplyr::filter(tolower(scientific_name) == "scomberomorus maculatus") %>%
  dplyr::select(scientific_name) %>%
  unique() %>%
  # get a string, not a df
  use_series(scientific_name)

#test: cnts for 1 sp. in both regions (GOM vs SA)
fhier_test_cnts <-
  fhier_catch_by_species_state_region_waves %>%
  # get the same species
  dplyr::filter(scientific_name == test_species_name) %>%
  # group by region
  dplyr::group_by(scientific_name, end_port_sa_gom) %>%
  # sum the FHIER catch
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## ACL data preparations ----

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

## change case for scientific_names to the same as in FHIER ----
acl_estimate_2022 %<>%
  dplyr::mutate(new_sci = toupper(new_sci))

#### check FL sa_gom ----
acl_estimate_2022 %>%
  dplyr::select(new_sta, sub_reg, fl_reg) %>% unique() %>%
  dplyr::filter(new_sta %in% c("FLE", "FLW"))
#   new_sta sub_reg fl_reg
# 1 FLE     6       5
# 2 FLE     6       4
# 3 FLW     7       1
# 4 FLW     7       3
# 5 FLW     7       2

### change FLE and FLW to FL ----
# to compare with FHIER

acl_estimate_2022 %<>%
  dplyr::mutate(state = case_when(new_sta %in% c("FLE", "FLW") ~ "FL",
                           .default = new_sta))

#### test: just for checking we actually dplyr::filtered the raw data ----
# View(acl_estimate)
dim(acl_estimate)[1]
# 372065
dim(acl_estimate_2022)[1]
# 2088

## Get MRIP (rec ACL) counts ----
### sum catch by species, state, region and wave
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_2022 %>%
  # dplyr::select the relevant columns only
  dplyr::select(new_sci,
         itis_code,
         new_com,
         state,
         sub_reg,
         year,
         wave,
         ab1) %>%
  # group by all except the counts
  dplyr::group_by(new_sci,
           itis_code,
           new_com,
           state,
           sub_reg,
           year,
           wave) %>%
  # save the sum of "ab1" for each group in "acl_estimate_catch_by_4"
  # remove NAs
  summarise(acl_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
  # back to an ungrouped form
  as.data.frame()

# dplyr::glimpse(acl_estimate_catch_by_species_state_region_waves)

# convert "year" and "wave" to numbers (were <chr> in acl_estimate_catch_by_species_state_region_waves)
acl_estimate_catch_by_species_state_region_waves1 <-
  acl_estimate_catch_by_species_state_region_waves %>%
  dplyr::mutate(year = as.double(year)) %>%
  dplyr::mutate(wave = as.double(wave))

## change regions to the same format as in FHIER ----
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_catch_by_species_state_region_waves1 %>%
  # change a 6 to "sa" and a 7 "gom", leave everything else in place
  dplyr::mutate(sa_gom = case_when(sub_reg == "6" ~ "sa",
                            sub_reg == "7" ~ "gom",
                            .default = sub_reg),
                            # put the new column after sub_reg (by default at the end)
                            .after = sub_reg) %>%
  # drop sub_reg
  dplyr::select(-sub_reg)

### make a test acl cnts DF of just one sp.----

acl_test_cnts <-
  acl_estimate_catch_by_species_state_region_waves %>%
  # get one species
  dplyr::filter(tolower(new_sci) == "scomberomorus maculatus") %>%
  # group by region
  dplyr::group_by(new_sci, sa_gom) %>%
  # sum the ACL catch
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## Rename fields to compare to FHIER ----
#### FHIER names ----
names(fhier_catch_by_species_state_region_waves) %>%
  paste0(collapse = "', '")

fhier_names <-
  c(
    'scientific_name',
    'species_itis',
    'common_name',
    'end_port_state',
    'end_port_sa_gom',
    'end_year',
    'end_wave',
    'fhier_quantity_by_4'
  )

#### MRIP names ----
names(acl_estimate_catch_by_species_state_region_waves) %>%
  paste0(collapse = "', '")

acl_names <- c(
  'new_sci',
  'itis_code',
  'new_com',
  'state',
  'sa_gom',
  # 'fl_reg',
  'year',
  'wave',
  'acl_estimate_catch_by_4'
)

#### common field names ----
wave_data_names_common <- c(
    'scientific_name',
    'species_itis',
    'common_name',
    "state",
    "sa_gom",
    "year",
    "wave"
)

acl_names_len_to_change <- length(acl_names) - 1

acl_estimate_catch_by_species_state_region_waves_renamed <-
  acl_estimate_catch_by_species_state_region_waves %>%
    # rename all columns from the list, except the last one

  rename_at(vars(acl_names[1:acl_names_len_to_change]),
            function(x) wave_data_names_common[1:acl_names_len_to_change])

fhier_names_len_to_change <- length(fhier_names) - 1

fhier_catch_by_species_state_region_waves_renamed <-
  fhier_catch_by_species_state_region_waves %>%
  rename_at(vars(fhier_names[1:fhier_names_len_to_change]),
            function(x)
              wave_data_names_common[1:fhier_names_len_to_change])

### rename fields in the test variables ----
names(fhier_test_cnts) <- c("scientific_name", "sa_gom", "mackerel_fhier_cnt")

### test: rename fields ----
# names(fhier_catch_by_species_state_region_waves)
# names(acl_estimate_catch_by_species_state_region_waves)
identical(names(fhier_catch_by_species_state_region_waves_renamed)[1:7],
          names(acl_estimate_catch_by_species_state_region_waves_renamed)[1:7])
# T

## fhier_spp data to a separate DF ----
fhier_spp <-
  fhier_catch_by_species_state_region_waves_renamed %>%
  dplyr::select(species_itis, common_name, scientific_name) %>%
  unique()

## Join Fhier and ACL DFs ----
fhier_acl_catch_by_species_state_region_waves <-
  # use a "full" join to keep entries from each df even if there is no counterpart in another one
  full_join(
    fhier_catch_by_species_state_region_waves_renamed,
    acl_estimate_catch_by_species_state_region_waves_renamed,
    # have to specify columns to join by, because some other columns might have the same name, but different meaning, e.g common_name
    by = join_by(scientific_name, state, sa_gom, year, wave),
    # Override the default suffixes, c(".x", ".y") in not merged cols
    suffix = c("_fhier", "_mrip")
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
# look at the first 20 entries for mackerel spanish (test_species_name)
fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(scientific_name == test_species_name) %>% head(20)

### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----
# names(fhier_acl_catch_by_species_state_region_waves)
fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(scientific_name == test_species_name) %>%
#
#   dplyr::filter(species_itis == test_species_itis) %>%
  dplyr::group_by(scientific_name, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(fhier_test_cnts$mackerel_fhier_cnt) #[1] TRUE

fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::filter(scientific_name == test_species_name) %>%
  dplyr::group_by(scientific_name, sa_gom) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_acl_cnt) %>%
  identical(acl_test_cnts$mackerel_acl_cnt)
  #[1] TRUE
