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
  
}

load_safis_catch < function() {
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
}

# 2) MRIP
load_mrip_data <- function() {
  mrip_dir_path <- "compare_catch/MRIP data/"
  mrip_csv_names_list <- c(
    "/mrip_aux/species_list.csv", # identical for all areas
    "/mrip_SA/mrip_estim_catch_year_2022_2022_SA.csv",
    "/mrip_GOM/mrip_estim_catch_year_2022_2022_gom.csv"
      )
  a <- mrip_csv_names_list %>%
    map_chr(file.path(mrip_dir_path, x))
  temp_var <- load_csv_names(my_paths, mrip_csv_names_list)
  mrip_species_list <- temp_var[[1]]
  mrip_estimate_sa <- temp_var[[2]]
  mrip_estimate_gom <- temp_var[[3]]
  
  mrip_estimate <- rbind(mrip_estimate_sa, mrip_estimate_gom)
}

# str(logbooks)
# str(mrip_estimate)

# 3) Auxilary
## ---- to get permit type info use compliance reports for now ----
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
# str(compl_clean)
