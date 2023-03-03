# get data

# see read.me.R
# SEFHIER data
# "FHIER_all_logbook_data.csv"
# or
# Reports / SAFIS Catches Extended
# or
# Reports / Species Count by Disposition

# MRIP
# https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#

# ---- 1) SEFHIER data ----

load_species_count_by_disposition <- function() {
  species_count_csv_names_list_22 = c("compare_catch/SEFHIER data/Catch by Disposition Detail (Unit Measure = CN Only).csv")
  species_count_contents <- load_csv_names(my_paths, species_count_csv_names_list_22)
  
  species_count <- clean_all_csvs(species_count_contents)
  
  return(species_count[[1]])
}
fhier_species_count_by_disposition <- load_species_count_by_disposition()
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
    unique()
  
  # str(safis_catch)
  # A tibble: 327,397 × 59
  return(safis_catch)
}

# ---- 2) MRIP ----
load_mrip_data <- function() {
  mrip_dir_path <- "compare_catch/MRIP data"
  mrip_csv_names_list_raw <- c(
    "mrip_aux/species_list.csv", # identical for all areas
    "mrip_US/mrip_catch_year_2022_preliminary.csv"
    # "mrip_SA/mrip_estim_catch_year_2022_2022_SA.csv",
    # "mrip_GOM/mrip_estim_catch_year_2022_2022_gom.csv"
      )
  # add prefix to each file name
  mrip_csv_names_list <- 
      map_chr(mrip_csv_names_list_raw, ~file.path(mrip_dir_path, .x))
  
  temp_var <- load_csv_names(my_paths, mrip_csv_names_list)
  mrip_species_list <- temp_var[[1]]
  mrip_estimate_usa <- temp_var[[2]]
  # mrip_estimate_sa <- temp_var[[2]]
  # mrip_estimate_gom <- temp_var[[3]]
  
  # mrip_estimate <- rbind(mrip_estimate_sa, mrip_estimate_gom)
  output <- list(mrip_species_list, mrip_estimate_usa)
  return(output)
}
mrip_temp <- load_mrip_data()
mrip_species_list <- mrip_temp[[1]]
mrip_estimate_all <- mrip_temp[[2]]
mrip_estimate_6_7 <-
  mrip_estimate_all %>% filter(sub_reg %in% c(6, 7))

# ---- 3) Auxilary ----
get_permit_type_from_compiance <- function() {
  # to get permit type info use compliance reports for now
  # use all years, to get more vessel ids with permit info
  compliance_csv_names_list_21_23 = c(
    "FHIER_Compliance_23__03_01_23.csv",
    "FHIER_Compliance_22__02_24_23.csv",
    "FHIER_Compliance_21__03_01_23.csv")
  full_file_names <- prepare_csv_names(compliance_csv_names_list_21_23)
  csv_contents <- load_csv_names(my_paths, full_file_names)
  
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
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
