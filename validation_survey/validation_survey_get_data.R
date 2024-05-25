# get data for logbooks annd catch comparison

my_paths <- set_work_dir()
# see read.me.R
# Survey data:

# detach("package:haven", unload = TRUE)
# install.packages("haven")
library(haven)

my_add_path <- "logbooks_compare"

## ---- get survey data for 2022 May-Dec ----

## ---- 1) extract survey data from zip ----
extract_to_dir <- file.path(my_paths$inputs, my_add_path, "survey_05_to_12_2022")

extract_zipped_survey_data <- function() {
  # get a list of zip archive file names
  list.files(path = file.path(my_paths$inputs, my_add_path, "survey_zip"),
             pattern = "*zip",
             full.names = TRUE) %>%
  # unzip all of them
    purrr::map(~unzip(.x,
               # to see what's in the archive without extracting
               # list = T,
               exdir = extract_to_dir))

}
# Use once
extract_zipped_survey_data()
#
## ---- read survey data from SAS format ----
# read all sas files in all sub directories
sas_file_list <-
  list.files(path = file.path(extract_to_dir),
           pattern = "*.sas7bdat",
           recursive = TRUE,
           full.names = TRUE)

str(sas_file_list)
# 45

# Instead of stopping the loop if there is an error, the cycle keeps going printing an error
poss_read_sas = possibly(
  # what function is used
  # .f = haven::read_sas,
  ~ haven::read_sas(.x,
                    # Make the names unique and syntactic
                    .name_repair = fix_names),
  # what to do if an error occurs
  otherwise = "Error")

# my_select <- function(x) {
#   f = possibly(function() select(x, -mpg), otherwise = x)
#   f()
# }

# todo: test
poss_read_sas1 <- function(x) {
  f = possibly(function() haven::read_sas(x, .name_repair = fix_names),
               otherwise = x)
               # otherwise = paste("Error in : ", x))
  f()
}

str(sas_file_list)
# loop through all files from the list and run the function on each one
survey_data_df <-
  sas_file_list %>%
  # use "_df" to combine all into one df
    purrr::map_df(~poss_read_sas(.x) %>%
             # convert all columns to char to use in bind
             dplyr::mutate(across(.fns = as.character))) %>%
  # Re-convert character columns
  # guess integer types for whole numbers
  type_convert(guess_integer = TRUE)

# survey_data_df %>% head()

# ---- add file_names to the df ----
add_file_names_to_the_df <- function() {
  sas_file_list %>%
  # use "_df" to combine all into one df
  purrr::map_df(~poss_read_sas(.x) %>%
           # convert all columns to char to use in bind
           dplyr::mutate(across(.fns = as.character)) %>%
           # add file name
           dplyr::mutate(FILE_NAME = tools::file_path_sans_ext(basename(.x)))
  ) %>%
  # Re-convert character columns
  # guess integer types for whole numbers
  type_convert(guess_integer = TRUE)
}
survey_data_df_w_fnames <- add_file_names_to_the_df()
str(survey_data_df_w_fnames)

# ---- separate df into separate dfs by file type ----

split_df_by_file_name <- function() {
  survey_data_df_w_fnames %>%
    split(f = survey_data_df_w_fnames$FILE_NAME) %>%
    return()
}

split_df_by_file_type <- function() {
  survey_data_df_w_fnames %>%
    split(f = substr(survey_data_df_w_fnames$FILE_NAME,
                   start = 1, stop = 3)
        ) %>%
    return()
}
survey_data_df_w_fnames_split <- split_df_by_file_type()

## ---- remove fileds with all NAs ----
not_all_na <- function(x) any(!is.na(x))

remove_all_na_fileds <- function() {
  survey_data_df_w_fnames_split %>%
    purrr::map(. %>% select(where(not_all_na))) %>%
    return()
}
survey_data_df_w_fnames_split_clean <- remove_all_na_fileds()
survey_data_df_w_fnames_split[[1]] %>% str()
# tibble [766 × 75] (S3: tbl_df/tbl/data.frame)

data_overview(survey_data_df_w_fnames_split[[1]])

# ---- write the survey df to a csv ----
# data_overview(survey_data_df)

otput_csv_file <- file.path(my_paths$inputs,
                            r"(logbooks_compare\survey_data_df_6_22_to_2_23.csv1)")
write.csv(survey_data_df,
          file = otput_csv_file, row.names = F)

## ---- write the list of survey dfs into csvs ----
survey_data_df_w_fnames_split_clean %>%
  # Apply a function to each element of a vector, and its index
  purrr::imap(~write.csv(.x,
                         file.path(my_paths$inputs,
                                   "logbooks_compare",
                                   paste0(.y, ".csv")
                                   ),
                         row.names = FALSE))


## ---- read sas files into a list of tibbles ----
# use sas_file_list_short_names as names for the list of dfs
sas_file_list_short_names <-
  list.files(path = file.path(extract_to_dir),
             pattern = "*.sas7bdat",
             recursive = TRUE,
             full.names = FALSE)

survey_data_list <-
  sas_file_list %>%
  purrr::map(poss_read_sas) %>%
# name the df as its file
    setNames(sas_file_list_short_names)

str(survey_data_list) %>% head()


## ---- there are 4 types of files ----
survey_data_list %>%
  purrr::map(~names(.x)) %>%
  unique() ->
  all_sas_names
str(all_sas_names)
# 4

## ---- read sas files by category into a list of tibbles ----
file_categories <- list("ref", "aga", "i1_", "i2_", "i3_")

file_lists_by_cat <-
  purrr::map(file_categories,
      ~list.files(path = file.path(extract_to_dir),
                  pattern = paste0(., "*"),
                  recursive = TRUE,
                  full.names = TRUE)
  ) %>%
  setNames(file_categories)

# survey_data_list <-
#   sas_file_list %>%
#   purrr::map(poss_read_sas) %>%

# each file in its df
read_by_category <-
  file_lists_by_cat %>%
  purrr::map(function(x) {
    x %>%
      purrr::map(poss_read_sas) %>%
      ## name the df as its file
      setNames(tools::file_path_sans_ext(basename(x)))
  }
)

str(read_by_category)
# View(read_by_category)

# each category in a df
read_by_category_df1 <-
  file_lists_by_cat %>%
  purrr::map(~map_df(.x, poss_read_sas))

# View(read_by_category_df1)


sas_file_list_ref <-
  list.files(path = file.path(extract_to_dir),
             pattern = "ref*",
             recursive = TRUE,
             full.names = TRUE)

survey_data_list_cat <-
  sas_file_list %>%

  purrr::map(~poss_read_sas(.x))

# ===

## ---- get logbooks from FHIER - not enough fields ----

fhier_logbooks_path_add <- "logbooks_from_fhier"

# all logbooks, by month, not all fields
load_all_fhier_logbooks <- function() {
  fhier_logbooks <-
    list.files(path = file.path(my_paths$inputs,
                                fhier_logbooks_path_add),
               pattern = "*.csv",
               full.names = TRUE)  %>%
    purrr::map_df(~read_csv(.x,
                     name_repair = fix_names,
                     show_col_types = FALSE) %>%
             dplyr::mutate(across(.fns = as.character))) %>%
    # Re-convert character columns
    # guess integer types for whole numbers
    type_convert(guess_integer = TRUE)

  return(fhier_logbooks)
}

fhier_logbooks <- load_all_fhier_logbooks()

# data_overview(fhier_logbooks)

## get "logbooks_from_fhier\FHIER_all_logbook_data.csv"
# is it different from "All logbooks" downloaded from FHIER?

fhier_all_logbook_data_csv <-
  file.path(my_paths$inputs,
            fhier_logbooks_path_add,
            "FHIER_all_logbook_data.csv"
            ) %>%
    read_csv(name_repair = fix_names,
           show_col_types = FALSE) %>%
    # dplyr::mutate(across(.fns = as.character))
# %>%
    # Re-convert character columns
    # guess integer types for whole numbers
    type_convert(guess_integer = TRUE)

# problems(fhier_all_logbook_data_csv)
# data_overview(fhier_all_logbook_data_csv)
