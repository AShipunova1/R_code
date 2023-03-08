# nolint: commented_code_linter
# useful functions

##--- start functions ---
# How to use:
# my_paths <- set_work_dir()
# csv_names_list = list("report1.csv", "report2.csv")
# xls_names_list = list("report1a.xls", "report2a.xls")
# csv_content_1 <- load_csv_names(my_paths, csv_names_list)[[1]]
# xls_content_1 <- load_xls_names(my_paths, xls_names_list, sheet_num = 2)[[1]]

## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths, filenames = csv_names_list_22_23)
# compl_clean <- temp_var[[1]]
# corresp_contact_cnts_clean <- temp_var[[2]]

#---

#install.packages("tidyverse")
library(tidyverse)
library(magrittr)
library(readxl)  # reading in .xlsx

# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower
  
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

load_csv_names <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, x))

  # read all csv files
  contents <- sapply(myfiles, read.csv, skipNul = TRUE, header = TRUE, simplify = FALSE)

  return(contents)
}

load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {
  my_inputs <- my_paths$inputs

  # add input directory path in front of each file name.
  myfiles <- sapply(xls_names_list, function(x) file.path(my_inputs, x))
  
  # browser()
  # print("map:")
  # start_time <- Sys.time()
  ## read all files
  contents <- map_df(myfiles, 
         ~read_excel(.x, sheet = sheet_n, .name_repair = "universal"))
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(contents)
}

clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    str_replace_all("\\s", "_") %<>%
    str_replace_all("\\.", "") %<>%
    my_headers_case_function()
  return(my_df)
}

# to use in a function, 
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  x %>%
    str_replace_all("\\s+", "_") %>%
    str_replace_all("\\.", "") %>%
    str_replace_all("\\W", "_") %>%
    my_headers_case_function()
}


## ---- functions to clean FHIER compliance and correspondense reports ----

# split week column ("52: 12/26/2022 - 01/01/2023") into 3 columns with proper classes, week_num (week order number), week_start and week_end
clean_weeks <- function(my_df) {
  my_df %>%
    separate_wider_delim(week, ":", names = c("week_num", "week_rest")) %>%
    separate_wider_delim(week_rest, " - ", names = c("week_start", "week_end")) ->
    temp_df

  my_df$week_num <- as.integer(trimws(temp_df$week_num))
  my_df$week_start <- as.Date(trimws(temp_df$week_start), "%m/%d/%Y")
  my_df$week_end <- as.Date(trimws(temp_df$week_end), "%m/%d/%Y")

  return(my_df)
}

# trim vesselofficialnumber, there are 273 w spaces in Feb 2023
trim_all_vessel_ids_simple <- function(csvs_clean_ws) {
  csvs_clean <- lapply(csvs_clean_ws, function(x) {
    # browser()
    x$vesselofficialnumber <- trimws(x$vesselofficialnumber)
    x
  }
  )
  return(csvs_clean)
}

# cleaning, regularly done for csvs downloaded from PFIER
clean_all_csvs <- function(csvs) {
  # unify headers
  csvs_clean0 <- lapply(csvs, clean_headers)
  # trim vesselofficialnumber, just in case
# browser()
  csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0)
  return(csvs_clean1)
}

join_same_kind_csvs <- function(csvs_list_2_plus) {
  return(bind_rows(csvs_list_2_plus))
}

# Combine correspondence and compliance information into one dataframe by "vesselofficialnumber" only. Not by time!
join_all_csvs <- function(corresp_arr, compl_arr) {
  corresp <- corresp_arr
  if (!is.data.frame(corresp_arr)) {
    corresp <- join_same_kind_csvs(corresp_arr)
  }

  compl <- compl_arr
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  compl %>%
    full_join(corresp,
              by = c("vesselofficialnumber"),
              multiple = "all") %>%
    return()
}

# Change a column class to POSIXct in the "my_df" for the field "field_name" using the "date_format"
change_to_dates <- function(my_df, field_name, date_format) {
  my_df %>%
    mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
    format = date_format)) %>%
    return()
}

aux_fun_for_dates <- function(x, date_format) {
  out <- as.POSIXct(x,
                    format = date_format)
  out
}

change_fields_arr_to_dates <- function(my_df, field_names_arr, date_format) {
  my_df %>%
    mutate(across(all_of(field_names_arr), aux_fun_for_dates, date_format)) %>%

    # mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
                                        # format = date_format)) %>%
    return()
}

# Use for contacts in the setup function before combining with compliant dataframes
add_count_contacts <- function(all_data_df_clean) {
  # browser()
  all_data_df_clean %>%
    # add a new column with a "yes" if there is a contactdate (and a "no" if not)
    # TODO: as.factor
    mutate(was_contacted = if_else(is.na(contactdate), "no", "yes")) %>%
    # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
    add_count(vesselofficialnumber, was_contacted, name = "contact_freq") %>%
    return()
}

# Get frequencies for each column in the list
# usage:
# group_by_arr <- c("vesselofficialnumber", "contacttype")
# count_by_column_arr(my_df, group_by_arr)
count_by_column_arr <- function(my_df, group_by_arr) {
  my_df %>%
    arrange(vesselofficialnumber) %>%
    group_by_at(group_by_arr) %>%
    summarise(my_freq = n()) %>%
    return()
}

data_overview <- function(my_df) {
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  count_uniq_by_column(my_df)
  # sapply(my_df, function(x) length(unique(x))) %>% as.data.frame()
}

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>% as.data.frame()
}

# from https://stackoverflow.com/questions/53781563/combine-rows-based-on-multiple-columns-and-keep-all-unique-values
# concat_unique <- function(x){paste(unique(x),  collapse=', ')}

concat_unique <- function(x){paste0(unique(x[!is.na(x)]), collapse= ", ")}
combine_rows_based_on_multiple_columns_and_keep_all_unique_values <- function(my_df, group_by_arr) {
  my_df %>%
    group_by_at(group_by_arr) %>%
    summarise_all(concat_unique) %>%
    return()
}

concat_unique_sorted <- function(x){paste0(unique(sort(x[!is.na(x)])), collapse= ", ")}

combine_rows_based_on_multiple_columns_and_keep_all_unique_sorted_values <- function(my_df, group_by_arr) {
  my_df %>%
    group_by_at(group_by_arr) %>%
    summarise_all(concat_unique_sorted) %>%
    return()
}

## usage:
# my_paths <- set_work_dir()
#
## get csv data into variables
# temp_var <- get_compl_and_corresp_data(my_paths)
# compl_clean <- temp_var[[1]]
# corresp_clean <- temp_var[[2]]

csv_names_list_22_23 = c("Correspondence.csv",
                         "FHIER_Compliance_22.csv",
                         "FHIER_Compliance_23.csv")

# add my additional folder names to each filename
prepare_csv_names <- function(filenames) {
  add_path_corresp <- "Correspondence"
  add_path_compl <- "FHIER Compliance"

  my_list <- sapply(filenames, function(x) {
    case_when(startsWith(my_headers_case_function(x), "correspond") ~
                file.path(add_path_corresp,  x),
              startsWith(my_headers_case_function(x), "fhier_compliance") ~
                file.path(add_path_compl,  x),
              .default = ""
    )
  } )
  paste(my_list) %>% as.list() %>% return()
}

get_compl_and_corresp_data <- function(my_paths, filenames = csv_names_list_22_23) {
  # browser()
  # add my additional folder names
  csv_names_list <- prepare_csv_names(filenames)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
  
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
  
  # ---- specific correspondence manipulations ----
  corresp_arr <- csvs_clean1[[1]]
  # add a new column with a "yes" if there is a contactdate (and a "no" if not),
  # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
  # change classes from char to POSIXct
  corresp_arr_contact_cnts %>%
    change_to_dates("createdon", "%m/%d/%Y %H:%M") %>%
    change_to_dates("contactdate", "%m/%d/%Y %I:%M %p") ->
    corresp_arr_contact_cnts_clean

  ## ---- specific compliance manipulations ----
  # browser()
  compl_arr <- csvs_clean1[2:length(csvs_clean1)]

  # if it is one df already, do nothing
  compl <- compl_arr
  # else combine separate dataframes for all years into one
  if (!length(compl_arr) == 1) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  compl %>%
    # split week column (52: 12/26/2022 - 01/01/2023) into 3 columns with proper classes, week_num (week order number), week_start and week_end
    clean_weeks() %>%
    # change dates classes from char to POSIXct
    change_to_dates("permitgroupexpiration", "%m/%d/%Y") ->
    compl_clean

  return(list(compl_clean, corresp_arr_contact_cnts_clean))
}

# read csv file with EOF within quoted strings
read_csv_w_eofs <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
  # add input directory path in front of each file name.
  myfiles <- sapply(csv_names_list, function(x) file.path(my_inputs, add_csv_path, x))

  # read csv files
  contents <- sapply(myfiles, fread, header = TRUE)
  # convert the first one into a data frame
  # TODO change this function to deal with multiple files
  contents[, 1] %>%
    as.data.frame() %>%
    return()
}

# To use as a filter in FHIER
cat_filter_for_fhier <- function(my_characters) {
  cat(my_characters,
      sep = ', ',
      file = file.path(my_paths$outputs,
                       "cat_out.txt"))
}