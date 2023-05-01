# library(sp) # vector data
# library(rgdal) # input/output, projections

# get data for compare catch

# see read.me.R
# SEFHIER data
# "FHIER_all_logbook_data.csv"

# MRIP
# "O:\Fishery Data\ACL Data\FES_Rec_data(mail_survey)\MRIP_FES_rec81_22wv6_01Mar23\MRIP_FES_rec81_22wv6_01Mar23w2014to2021LACreel.xlsx"

# ---- 1) SEFHIER data ----

load_all_logbooks <- function() {
  # "~\Documents\R_files_local\my_inputs\logbooks_from_fhier\FHIER_all_logbook_data.csv"
  species_count_csv_names_list_lb = c(r"(logbooks_from_fhier\FHIER_all_logbook_data.csv)")
  fhier_all_logbook_data <- load_csv_names(my_paths, species_count_csv_names_list_lb)
  # browser()
  # clean_all_csvs(csv_contents, vessel_id_field_name = "vessel_official_number")
  logbooks_content <- 
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
    "acl_aux/species_list.csv" # identical for all areas
  )
  # a file recommended by Mike
  acl_xls_names_list_raw <- c(r"(acl_US\aclaclspec_rec81_22wv6_01mar23w2014to2021LACreel.xlsx)")

  # add prefix to each file name
  acl_csv_names_list <- 
      map_chr(acl_csv_names_list_raw, ~file.path(acl_dir_path, .x))
  acl_xls_names_list <- map_chr(acl_xls_names_list_raw, ~file.path(acl_dir_path, .x))
# browser()  
  acl_species_list <- load_csv_names(my_paths, acl_csv_names_list)
  
  # str(acl_species_list)
  
  acl_estimate_usa <- 
    load_xls_names(my_paths, acl_xls_names_list,
                   sheet_n = "aclaclspec_rec81_22wv6_01mar23") 

    output <- list(acl_species_list, acl_estimate_usa)
  return(output)
}

# TODO: benchmark, too slow
acl_temp <- load_acl_data()

acl_species_list <- acl_temp[[1]]
acl_estimate <- acl_temp[[2]]

# data_overview(acl_estimate)

## ---- specifically for "O:\Fishery Data\ACL Data\"

# str(acl_estimate)
acl_estimate_2022 <-
  acl_estimate %>%
  filter(year == "2022")

dim(acl_estimate)
# [1] 347379 67
dim(acl_estimate_2022)
# [1] 8332   67
# names(acl_estimate)
acl_estimate <-
  acl_estimate_2022 %>%
  # filtering here for just SA (6) and Gulf (7) sub regions
    filter(sub_reg %in% c(6, 7)) %>%
# dim(acl_estimate)
# [1] 7479   67
  # -	New variable ‘agg_moden’ divides all estimates into for-hire (cbt, hbt, or cbt/hbt) or private (private or shore) mode fishing
  # new_mode	recoded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)
  # new_moden		alpha description of ‘new_mode’
  # filter(new_moden == "Cbt") # charterboat
  filter(new_mode %in% c(2,3,5))  %>%
  filter(!(ds == "SRHS"))
# Explanations: ----
# 5=charter/headboat - obsolete - check dates
View(acl_estimate)
# dim(acl_estimate)
# [1] 1442   67

# grep("mode", names(acl_estimate), value = T)
# [1] "new_mode"  "new_moden" "mode_fx"   "agg_moden"

# acl_estimate %>% select(new_moden) %>% unique()
## ---- specifically for acl_catch_year_2022_preliminary.csv and/or
# acl_SA/acl_estim_catch_year_2022_2022_SA.csv
# use sub_reg 6 & 7 for now (SA & GOM)
# And federal waters only
# acl_estimate <-
#   acl_estimate_all %>% 
#   filter(sub_reg %in% c(6, 7))
# %>%
# ? WFL 10?
  # filter(area_x %in% c(2, 3, 4))

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
  csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name = "vessel_official_number")
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

