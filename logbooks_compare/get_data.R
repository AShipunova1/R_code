# get data for logbooks comparison

# see read.me.R
# Survey data:
# https://drive.google.com/drive/folders/1D1ksBarjJzvbmqWa5cEE-k0s6ZJi0VAM
# "my_inputs\logbooks_compare\survey\May 2022-20230307T132845Z-001.zip"

# detach("package:haven", unload = TRUE)
# install.packages("haven")
library(haven)

my_add_path <- "logbooks_compare"

## ---- get survey data for 2022 May-Dec ----

## ---- 1) extract survey data from zip ----
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
# read all sas files in all sub directories
sas_file_list <-
  list.files(path = file.path(extract_to_dir), 
           pattern = "*.sas7bdat",
           recursive = TRUE,
           full.names = TRUE)

str(sas_file_list)
# 45

sas_file_list_short_names <-
  list.files(path = file.path(extract_to_dir), 
             pattern = "*.sas7bdat",
             recursive = TRUE,
             full.names = FALSE)

# Instead of stopping the loop if there is an error, the cycle keeps going printing an error
poss_read_sas = possibly(
  # what function is used
  # .f = haven::read_sas,
  ~ haven::read_sas(.x, .name_repair = "universal"),
  # what to do if an error occurs
  otherwise = "Error")

# loop through all files from the list and run the function on each one
survey_data_df <-
  sas_file_list %>%
  # use "_df" to combine all into one df
    map_df(~poss_read_sas(.x
                          # ,
                       # Make the names unique and syntactic
                       # .name_repair = "universal"
                       ) %>% 
             # convert all columns to char to use in bind
             mutate(across(.fns = as.character))) %>%
  # guess and change each column type
  type_convert()

str(survey_data_df) %>% head()

# read sas files into a list of tibbles
survey_data_list <-
  sas_file_list %>%
  map(~poss_read_sas(.x))
           
str(survey_data_list) %>% head()

# use sas_file_list_short_names as names for the list of dfs
names(survey_data_list) <- sas_file_list_short_names

## ---- check names and data inside ----
# names(survey_data_list)[[4]]
# survey_data_list[[4]] %>% select(YEAR, MONTH) %>% unique()
# 
# names(survey_data_list)[[19]]
# survey_data_list[[19]] %>% 
#   select(YEAR, WAVE) %>% unique()

survey_data %>%
  map(~names(.x)) %>%
  unique() ->
  all_sas_names
str(all_sas_names)
# 4

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