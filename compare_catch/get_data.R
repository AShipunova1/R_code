library(sp) # vector data
library(rgdal) # input/output, projections

# get data for compare catch

# see read.me.R
# SEFHIER data
# "FHIER_all_logbook_data.csv"
# or
# Reports / SAFIS Catches Extended
# or
# Reports / Species Count by Disposition

# MRIP
# https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#
# or
# "O:\Fishery Data\ACL Data\FES_Rec_data(mail_survey)\MRIP_FES_rec81_22wv6_01Mar23\MRIP_FES_rec81_22wv6_01Mar23w2014to2021LACreel.xlsx"
# 2022 only: local mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel_2022.xlsx

# ---- 1) SEFHIER data ----

load_species_count_by_disposition <- function() {
  species_count_csv_names_list_22 = c("compare_catch/SEFHIER data/Catch by Disposition Detail (Unit Measure = CN Only).csv")
  species_count_contents <- load_csv_names(my_paths, species_count_csv_names_list_22)
  
  # vessel_id_field_name <- grep("vessel.*official.*number", 
  #                                                      tolower(names(x)),
  #                                                      value = T)
  
  species_count <- clean_all_csvs(species_count_contents)
  
  return(species_count[[1]])
}
# fhier_species_count_by_disposition <- load_species_count_by_disposition()
# str(fhier_species_count_by_disposition)
# 'data.frame':	316171 obs. of  9 variables:

load_safis_catch <- function() {
  # download Reports / SAFIS Catches Extended for each month, one year is too slow
  safis_catch <- 
    list.files(path = file.path(my_paths$inputs, "compare_catch/SAFIS CATCHES EXTENDED_2022"), 
               pattern = "*.csv",
               full.names = TRUE)  %>%
    map_df(~read_csv(.x,
                     show_col_types = FALSE) %>% 
             mutate(across(.fns = as.character))) %>%
    type_convert() %>%
    rename_with(toupper) %>%
    unique()
  
  # str(safis_catch)
  # A tibble: 327,397 × 59
  return(safis_catch)
}

load_all_logbooks <- function() {
  # "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\logbooks_from_fhier\FHIER_all_logbook_data.csv"
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
str(logbooks_content)

# ---- 2) MRIP ----
load_mrip_data <- function() {
  mrip_dir_path <- "compare_catch/MRIP data"
  mrip_csv_names_list_raw <- c(
    "mrip_aux/species_list.csv" # identical for all areas
  )
  # a file recommended by Mike
  mrip_xls_names_list_raw <- c(r"(mrip_US\mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel.xlsx)")
  # mrip_xls_names_list_raw <- c(r"(mrip_US\mripaclspec_rec81_22wv6_01mar23w2014to2021LACreel_2022.xlsx)")
  
    # r"(mrip_US\mripaclspec_rec81_22wv6_01mar23.csv)"
    # "mrip_US/mrip_catch_year_2022_preliminary.csv"
    # "mrip_SA/mrip_estim_catch_year_2022_2022_SA.csv",
    # "mrip_GOM/mrip_estim_catch_year_2022_2022_gom.csv"

  # add prefix to each file name
  mrip_csv_names_list <- 
      map_chr(mrip_csv_names_list_raw, ~file.path(mrip_dir_path, .x))
  mrip_xls_names_list <- map_chr(mrip_xls_names_list_raw, ~file.path(mrip_dir_path, .x))
# browser()  
  mrip_species_list <- load_csv_names(my_paths, mrip_csv_names_list)
  
  mrip_estimate_usa <- 
    load_xls_names(my_paths, mrip_xls_names_list,
                   sheet_n = "mripaclspec_rec81_22wv6_01mar23") 
  # %>%
    # guess integer types for whole numbers
    # type_convert(guess_integer = TRUE)

  # ---
  # map_df(~read_csv(.x,
  #                  name_repair = fix_names,
  #                  show_col_types = FALSE) %>% 
  #          mutate(across(.fns = as.character))) %>%
  #   # Re-convert character columns
  #   # guess integer types for whole numbers
  #   type_convert(guess_integer = TRUE)
  # 
  # ---
  
  # mrip_estimate_sa <- temp_var[[2]]
  # mrip_estimate_gom <- temp_var[[3]]
  
  # mrip_estimate <- rbind(mrip_estimate_sa, mrip_estimate_gom)
  output <- list(mrip_species_list, mrip_estimate_usa)
  return(output)
}
# TODO: benchmark, too slow
mrip_temp <- load_mrip_data()

mrip_species_list <- mrip_temp[[1]]
mrip_estimate <- mrip_temp[[2]]

# data_overview(mrip_estimate)

## ---- specifically for "O:\Fishery Data\ACL Data\"
# "FES_Rec_data(mail_survey)\MRIP_FES_rec81_22wv6_01Mar23\" and 
# "MRIP Based Rec Data(CHTS)\MRIPACLspec_rec81_22wv6_01mar2\" ----
# str(mrip_estimate)
mrip_estimate_2022 <-
  mrip_estimate %>%
  filter(year == "2022")

dim(mrip_estimate)
# [1] 347379 67
dim(mrip_estimate_2022)
# [1] 8332   67
# names(mrip_estimate)
mrip_estimate <-
  mrip_estimate_2022 %>%
    filter(sub_reg %in% c(6, 7)) %>%
# dim(mrip_estimate)
# [1] 7479   67
  filter(new_moden == "Cbt")
# dim(mrip_estimate)
# [1] 1442   67

# grep("mode", names(mrip_estimate), value = T)
# [1] "new_mode"  "new_moden" "mode_fx"   "agg_moden"

# mrip_estimate %>% select(new_moden) %>% unique()
## ---- specifically for mrip_catch_year_2022_preliminary.csv and/or
# mrip_SA/mrip_estim_catch_year_2022_2022_SA.csv
# use sub_reg 6 & 7 for now (SA & GOM)
# And federal waters only
# mrip_estimate <-
#   mrip_estimate_all %>% 
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
# compl_clean <- get_permit_type_from_compiance()

get_scientific_names <- function() {
  scientific_names_xls_names_list = c("compare_catch/SEFHIER data/SEFHIER_species.xlsx")
  scientific_names_xls_content <- load_xls_names(my_paths,
                                                 scientific_names_xls_names_list,
                                                 sheet_n = "Species Only")
  return(scientific_names_xls_content)
}
scientific_names <- get_scientific_names()

# ---- rename all field names to upper case for comparability ----
data_list_names <- list("fhier_species_count_by_disposition", "mrip_species_list", "mrip_estimate", "scientific_names")

rename_all_field_names <- function() {
# TODO: benchmark with map()
  for(d_name in data_list_names) {
    # get an object (df) by its name
    tmp0 <- get(d_name)
    # change field names to upper case
    tmp1 <- rename_with(tmp0, toupper)
    # assign newly renamed df back to the same df name
    assign(d_name, tmp1)
  }
}
# names(fhier_species_count_by_disposition)
# str(mrip_species_list)

## ---- get geographical data ----
read_shapefile <- function(filename) {
  shapefile_file_name <- file.path(my_paths$inputs, "shapefiles", filename)
  
  x <- readOGR(shapefile_file_name)
  return(x)
}
sa_shp <- read_shapefile("osa_n_gom/SA_EEZ_off_states.shp")
gom_shp <- read_shapefile("osa_n_gom/ReefFish_EFH_GOM.shp")

read_port_coords <- function(){
  port_coords_file_name <- file.path(my_paths$inputs, 
                                     "ports.csv")
  if(file.exists(port_coords_file_name)) {
    read_csv(port_coords_file_name,
                     show_col_types = FALSE) %>% 
             mutate(across(.fns = as.character))
  }
}
port_coords <- read_port_coords()

## ---- make coord_table ----
# GIS_LATHBEG, GIS_LATHEND, GIS_LONHBEG, GIS_LONHEND

str(lat_lon_cnts)
str(most_frequent_fhier10_w_info)

lat_lon_cnts_w_info <-
  most_frequent_fhier10_w_info %>% 
  mutate(latitude = as.double(latitude) %>% 
           round(digits = 2)) %>% 
  mutate(longitude = as.double(longitude) %>% 
           round(digits = 2)) %>% 
  filter(abs(latitude) >= 0 & abs(longitude) >= 0) %>%
  # to all positive
  mutate(latitude = abs(latitude)) %>%
    # to all negative
  mutate(longitude = (abs(longitude) * -1)) %>%
  # data_overview()
    group_by(common_name, latitude, longitude) %>%
    summarise(fhier_quantity_by_sp_geo = sum(as.integer(reported_quantity)))
    # latitude     
    # Min.   :-87.30  
    # Max.   : 90.00    
    
#     longitude      
    # Min.   :-105.14  
    # Max.   : 137.59  

lat_lon_cnts_w_info %>%
  filter(abs(longitude) < 80) %>%
select(common_name, latitude, longitude) %>% unique %>% dim()
# 6693

lat_lon_cnts_w_info %>%
  filter(abs(latitude) < 10) %>% unique %>% dim()
# 623

dim(lat_lon_cnts_w_info)
# 58871

clean_geo_data <- function(lat_lon_cnts_w_info) {
  # cbind(stack(lat_lon_data_all[1:2]), stack(lat_lon_data_all[3:4])) -> res1
  res2 <- 
    lat_lon_cnts_w_info %>%
    ungroup() %>%
    select(latitude, longitude)
    
  colnames(res2) <- c("lat", "lon")
  # remove NAs
  clean_lat_lon <- res2[complete.cases(res2), ]
  return(clean_lat_lon)
}

lat_lon_data <- clean_geo_data(lat_lon_cnts_w_info)
tail(lat_lon_data)

