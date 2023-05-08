#----General Notes on how code works ----

# Input data
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

      # Use DS column to filter out SRHS (headboat)

      # Use "new mode" column to filter out private and shore modes (private = rec;
      #            shore mode = private rec fishing from shore)
      # # new_mode = recorded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)

      # ab1			type A + type B1 catch estimate (number of fish killed or kept)

#general set up:
#load required packages; or install first if necessary
library(tidyverse) #Collection of packages (visualization, manipulation): ggplot2, dplyr, purrr, etc.
library(readxl) # to read in XL files
library(zoo) #converting dates
library(magrittr) #for data piping (%<>% allows piping in both direction)

# set working directories
# then you can use it in the code like my_paths$input etc.

set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()
  main_r_dir <- "R_files_local"

  in_dir <- "my_inputs"
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "my_outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  git_r_dir <- "R_code_github"
  full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir,
                   "git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

my_paths <- set_work_dir()

# auxilary functions ----
# quick look at the data structure
data_overview <- function(my_df) {
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  count_uniq_by_column(my_df)
}

# trim vesselofficialnumber, there are 273 white spaces in Feb 2023
trim_all_vessel_ids_simple <-
  function(csvs_clean_ws, col_name_to_trim = NA) {
    csvs_clean <- lapply(csvs_clean_ws, function(x) {
      if (is.na(col_name_to_trim)) {
        col_name_to_trim <- grep("vessel.*official.*number",
                                 tolower(names(x)),
                                 value = T)
      }
      col_name_to_trim_s <- sym(col_name_to_trim)
      # Hard code vessel_official_number as vessel id
      x %>%
        mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
        return()
    })
    return(csvs_clean)
  }

# cleaning, regularly done for csvs downloaded from PHIER
clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
  # unify headers
  csvs_clean0 <- lapply(csvs, clean_headers)
  # trim vesselofficialnumber, in case tthere are trailing spaces
  csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
  return(csvs_clean1)
}

load_csv_names <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <- lapply(csv_names_list, function(x) file.path(my_inputs, x))
  # read all csv files
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  return(contents)
}

# Load all data ----
# ---- 1) SEFHIER data ----

load_all_logbooks <- function() {
  species_count_csv_names_list_lb = c(r"(logbooks_from_fhier\FHIER_all_logbook_data.csv)")
  fhier_all_logbook_data <- load_csv_names(my_paths, species_count_csv_names_list_lb)
  logbooks_content <-
    # see an aux function above
    clean_all_csvs(fhier_all_logbook_data,
                   vessel_id_field_name = "vessel_official_nbr")

  return(logbooks_content[[1]])
}
# TODO: slow, benchmark
logbooks_content <- load_all_logbooks()
# str(logbooks_content)

# ---- 2) ACL (Annual Catch Limits surveys) ----
load_acl_data <- function() {
  acl_dir_path <- "compare_catch/MRIP data"
  acl_csv_names_list_raw <- c(
    "mrip_aux/species_list.csv" # identical for all areas
  )
  # a file recommended by Mike
  acl_xls_names_list_raw <- c(r"(mrip_US\mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel.xlsx)")
  # add prefix to each file name
  acl_csv_names_list <-
      map_chr(acl_csv_names_list_raw, ~file.path(acl_dir_path, .x))
  acl_xls_names_list <- map_chr(acl_xls_names_list_raw, ~file.path(acl_dir_path, .x))
# browser()
  acl_species_list <- load_csv_names(my_paths, acl_csv_names_list)

  # str(acl_species_list)

  acl_estimate_usa <-
    load_xls_names(my_paths, acl_xls_names_list,
                   sheet_n = "mripaclspec_rec81_22wv6_01mar23")

    output <- list(acl_species_list, acl_estimate_usa)
  return(output)
}

# TODO: benchmark, too slow
acl_temp <- load_acl_data()

acl_species_list <- acl_temp[[1]]
acl_estimate <- acl_temp[[2]]

# data_overview(acl_estimate)

# ---- 3) Auxilary ----
get_permit_type_from_compiance <- function() {
  # to get permit type info use compliance reports for now
  # use all years, to get more vessel ids wihttp://127.0.0.1:15607/graphics/6d3879ae-f56a-451e-ab55-36d42ea6b820.pngth permit info
  compliance_csv_names_list_21_23 = c(
    "FHIER_Compliance_23__03_01_23.csv",
    "FHIER_Compliance_22__02_24_23.csv",
    "FHIER_Compliance_21__03_01_23.csv")
  full_file_names <- prepare_csv_names(compliance_csv_names_list_21_23)
  csv_contents <- load_csv_names(my_paths, full_file_names)

  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents,
                                vessel_id_field_name = "vessel_official_number")
  compl_clean <- csvs_clean1[[1]]
  return(compl_clean)
}
compl_clean <- get_permit_type_from_compiance()

get_scientific_names <- function() {
  scientific_names_xls_names_list = c("compare_catch/SEFHIER data/SEFHIER_species.xlsx")
  scientific_names_xls_content <- load_xls_names(my_paths,
                                                 scientific_names_xls_names_list,
                                                 sheet_n = "Species Only")
  return(scientific_names_xls_content)
}
scientific_names <- get_scientific_names()

# ---- rename all field names to upper case for comparability ----
data_list_names <- list("fhier_species_count_by_disposition", "acl_species_list", "acl_estimate", "scientific_names")

rename_all_field_names <- function() {
# TODO: benchmark with map()
  for (d_name in data_list_names) {
    # get an object (df) by its name
    tmp0 <- get(d_name)
    # change field names to upper case
    tmp1 <- rename_with(tmp0, toupper)
    # assign newly renamed df back to the same df name
    assign(d_name, tmp1)
  }
}
# names(fhier_species_count_by_disposition)
# str(acl_species_list)
# grep("Tile", acl_species_list$common_name,
# ignore.case = T, value = T)

# ## ---- get geographical data ----
# read_shapefile <- function(filename) {
#   shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)
#
#   x <- readOGR(shapefile_file_name)
#   return(x)
# }
# sa_shp <- read_shapefile("osa_n_gom/SA_EEZ_off_states.shp")
# gom_shp <- read_shapefile("osa_n_gom/ReefFish_EFH_GOM.shp")
#
# read_port_coords <- function(){
#   port_coords_file_name <- file.path(my_paths$inputs,
#                                      "ports.csv")
#   if(file.exists(port_coords_file_name)) {
#     read_csv(port_coords_file_name,
#                      show_col_types = FALSE) %>%
#              mutate(across(.fns = as.character))
#   }
# }
# port_coords <- read_port_coords()
#
## ---- get state coords ----
# https://www.latlong.net/category/states-236-14.html
# states_coords_raw <- read_csv(file.path(my_paths$inputs, "coordinates", "states_decimal_deg.csv"))
# head(states_coords_raw)

## ---- add state abbreviations ----
# state.name[grep("FL", state.abb)]

# str(data.frame(as.list(state_tbl)))
# str(states_coords_raw)
state_tbl <- data.frame(state.abb, tolower(state.name))
names(state_tbl) = c("state_abb", "state_name")
  # setNames(state.abb, tolower(state.name))

# str(state_tbl)
# str(states_coords_raw)

## ---- ports coordinates from SERO ----
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\View Landing Locations.xlsx"
# ports_coords_raw <- read_csv(file.path(my_paths$inputs, "View_Landing_Locations.csv"))
# str(ports_coords_raw)
