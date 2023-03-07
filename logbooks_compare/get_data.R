# get data for logbooks comparison

# see read.me.R
# Survey data:
# https://drive.google.com/drive/folders/1D1ksBarjJzvbmqWa5cEE-k0s6ZJi0VAM
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\logbooks_compare\survey\May 2022-20230307T132845Z-001.zip"

library(haven)

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


  map(~haven::read_sas(.x,
                          .name_repair = "universal")) %>% str()
  #   ~read_csv(.x,
  #                  show_col_types = FALSE) %>% 
  #          mutate(across(.fns = as.character))) %>%
  # type_convert() %>%
  # rename_with(toupper) %>%
  # unique()
