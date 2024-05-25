# get data for logbooks and catch comparison
# see read.me.R

# Turn off the scientific notation
options(scipen = 999)

# load Validation Survey data ----
# https://drive.google.com/drive/folders/1JDlzVXcTkdY17Sux8hZOZbxFnj2_E9eh?usp=drive_link
# Dominique Lazarre, Sept 14, 2023
# "my_inputs\validation_survey\Merged_Validation_Survey_Data.zip"

validation_survey_data_dir_path <- file.path(my_paths$inputs,
                                         "validation_survey")

# dir.exists(validation_survey_data_dir_path)

# read 
csv_filenames <-
  list.files(validation_survey_data_dir_path,
             pattern = "*.csv",
             full.names = TRUE)

str(csv_filenames)
# 5

# loop through all files from the list and run the function on each one
survey_data_l <-
  csv_filenames |>
  purrr::map(
    ~readr::read_csv(
      .x,
      col_types = readr::cols(.default = 'c'),
      trim_ws = TRUE,
      na = c("", "NA", "NaN"),
      name_repair = auxfunctions::fix_names
    ) |>
      readr::type_convert(guess_integer = TRUE)
  )

## make short names ----
short_names <-
  csv_filenames |>
  purrr::map(basename) |>
  stringr::str_replace("([^_]+)_.+", "\\1")

names(survey_data_l) <- short_names

glimpse(survey_data_l)

## remove fields with all NAs ----

survey_data_l_not_na <-
  survey_data_l |>
  purrr::map(auxfunctions::remove_empty_cols)

# check
survey_data_l |>
  purrr::imap(\(x, idx) {
    diffdf::diffdf(survey_data_l[[idx]], survey_data_l_not_na[[idx]])
  })
# $aga
# Differences found between the objects!
# 
# A summary is given below.
# 
# There are columns in BASE that are not in COMPARE !!
# All rows are shown in table below
# 
#   =========
#    COLUMNS 
#   ---------
#    intcd2  
#    start4  
#     stop4  
#    tsite4  
#   ---------
# 
# 
# $i1
# No issues were found!
# 
# $i2
# No issues were found!
# 
# $i3
# No issues were found!
# 
# $ref
# Differences found between the objects!
# 
# A summary is given below.
# 
# There are columns in BASE that are not in COMPARE !!
# All rows are shown in table below
# 
#   ==========================
#            COLUMNS          
#   --------------------------
#    la_charter_permit_number 
#       interviewee_m_name    
#       interviewee_suffix    
#   --------------------------

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
