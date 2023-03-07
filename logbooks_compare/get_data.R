# get data for logbooks comparison

# see read.me.R
# Survey data:
# https://drive.google.com/drive/folders/1D1ksBarjJzvbmqWa5cEE-k0s6ZJi0VAM
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\logbooks_compare\survey\May 2022-20230307T132845Z-001.zip"

# detach("package:haven", unload = TRUE)
# install.packages("haven")
library(haven)

# install.packages("sas7bdat")
library(sas7bdat)

my_add_path <- "logbooks_compare"

## ---- get survey data for 2022 May-Dec ----

#    read_sas(unz("examp;e.zip", "'targetfilename.sas7bdat'"))
# read.sas7bdat
# haven::read_sas is faster

## ---- extract survey data ----
extract_to_dir <- file.path(my_paths$inputs, my_add_path, "survey_05_to_12_2022")

extract_zipped_survey_data <- function() {
  # get a list of zip archive file names 
  list.files(path = file.path(my_paths$inputs, my_add_path, "survey"), 
             pattern = "*zip",
             full.names = TRUE) %>%
    # unzip all of them
    map(~unzip(.x, 
               # to see what's in the archive without extracting
               # list = T,
               exdir = extract_to_dir))

}
# Use once
# extract_zipped_survey_data()

## ---- read survey data from SAS format ----
# read all sas files in all subdirectories
sas_file_list <- 
  list.files(path = file.path(extract_to_dir), 
           pattern = "*.sas7bdat",
           recursive = T,
           full.names = TRUE)

# str(sas_file_list)
# 40

# posslm1 = possibly(.f = lm, otherwise = "Error")
# When I use posslm1() in my model fitting loop, you can see that loop now finishes. Model b contains the string “Error” instead of a model.
# 
# map(dat_split, ~posslm1(y ~ x, data = .x) )

poss_read_sas = possibly(.f = haven::read_sas, otherwise = "Error")
# poss_read_sas7 = possibly(.f = read.sas7bdat, otherwise = "Error")
# 
# poss_read_sas7(sas_file_list[2]) %>% str()

survey_data <-
  sas_file_list %>%
    map(~poss_read_sas(.x,
                       .name_repair = "universal"))

  #          mutate(across(.fns = as.character))) %>%
  # type_convert() %>%
  # rename_with(toupper) %>%
  # unique()

# ===
## ---- get logbooks from FHIER ----
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