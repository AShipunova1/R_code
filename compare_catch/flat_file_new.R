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
# Recommended by Mike May 11, 2023. This file doesn't have ITIS for all spp., use scientific names to compare with FHIER
# MRIP_FES_rec81_22wv6_01May23w2014to2022LACreel.xlsx
# https://docs.google.com/spreadsheets/d/1YpfKfYFyUm3dWY7PK0XbEfUAM3onsnys/edit?usp=share_link&ouid=104137968095602765842&rtpof=true&sd=true

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

# Use DS column to filter out SRHS (headboat)

# Use "new mode" column to filter out private and shore modes (private = rec;
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
  add_dir <- ""
  # for Anna only
  add_dir <- "R_files_local/test_dir"
  
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
        mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
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

#end auxiliary functions --------------------

# Load all data ----
## ---- 1) SEFHIER data ----

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

# TODO: slow, benchmark
logbooks_content <- load_all_logbooks()

### load sefhier spp. ----
#r"()" - will correct slashes, so I can "copy path" from the finder and paste it inside the parenthesis without worrying about "\" or "//" etc.
sefhier_sp_file_name <- "SEFHIER_species.xlsx"
sheet_name <- "Species Tree"

sefhier_sp_all <-
  # see functions above
  load_xls_names(my_paths, c(sefhier_sp_file_name),
                 sheet_n = sheet_name)

# ---- 2) MRIP rec ACL (Annual Catch Limit surveys) ----
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

# TODO: benchmark, too slow
acl_temp <- load_acl_data()

acl_species_list <- acl_temp[[1]]
acl_estimate <- acl_temp[[2]]

# data_overview(acl_estimate)

