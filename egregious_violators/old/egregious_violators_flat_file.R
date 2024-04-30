#### Current file: ~/R_code_github/useful_functions_module.r ----

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
library(rbenchmark)
library(ROracle)

# Do not show warnings about groups
options(dplyr.summarise.inform = FALSE)
# Turn off the scientific notation
options(scipen = 999)

# Use my function in case we want to change the case in all functions
my_headers_case_function <- tolower

# current user name
get_username <- function(){
    return(as.character(Sys.info()["user"]))
}

# set working directories
  # change main_r_dir, in_dir, out_dir, git_r_dir to your local environment
  # then you can use it in the code like my_paths$input etc.
set_work_dir <- function() {
  setwd("~/")
  base_dir <- getwd()

  # for others
  add_dir <- ""
  # for Anna's computer
  if (get_username() == "anna.shipunova") {
    add_dir <- "R_files_local/test_dir"
  }

  # add an empty or Anna's folder in front
  main_r_dir <- file.path(add_dir, "SEFHIER/R code")

  in_dir <- "Inputs"
  # file.path instead of paste, because it provides correct concatenation, "\" or "/" etc.
  full_path_to_in_dir <- file.path(base_dir, main_r_dir, in_dir)
  out_dir <- "Outputs"
  full_path_to_out_dir <- file.path(base_dir, main_r_dir, out_dir)

  # git_r_dir <- "R_code_github"
  # full_path_to_r_git_dir <- file.path(base_dir, git_r_dir)

  setwd(file.path(base_dir, main_r_dir))

  my_paths <- list("inputs" = full_path_to_in_dir,
                   "outputs" = full_path_to_out_dir) #,
                   #"git_r" = full_path_to_r_git_dir)
  return(my_paths)
}

set_work_dir_local <- function() {
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

if (get_username() == "anna.shipunova") {
  set_work_dir <- set_work_dir_local
}

load_csv_names <- function(my_paths, csv_names_list) {
  my_inputs <- my_paths$inputs
# add input directory path in front of each file name.
  myfiles <- lapply(csv_names_list, function(x) file.path(my_inputs, x))
  # read all csv files
  # contents <- lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE)
  contents <- lapply(myfiles, read_csv, col_types = cols(.default = 'c'))

  return(contents)
}

load_xls_names <- function(my_paths, xls_names_list, sheet_n = 1) {
  my_inputs <- my_paths$inputs

  # add input directory path in front of each file name.
  myfiles <- lapply(xls_names_list, function(x) file.path(my_inputs, x))

  # browser()
  # print("map:")
  # start_time <- Sys.time()
  ## read all files
  contents <- map_df(myfiles,
         ~read_excel(.x,
                     sheet = sheet_n,
                     .name_repair = fix_names,
                     guess_max = 21474836,
                     col_types = "text"))
  # %>%
  # , col_types = "character"
  #   type_convert(guess_integer = TRUE)
  # end_time <- Sys.time()
  # print(end_time - start_time)
  return(contents)
}

clean_headers <- function(my_df) {
  colnames(my_df) %<>%
    fix_names()
  return(my_df)
}

# to use in a function,
# e.g. read_csv(name_repair = fix_names)
fix_names <- function(x) {
  x %>%
    # remove dots
    str_replace_all("\\.", "") %>%
    # all not letters and numbers to underscores
    str_replace_all("[^A-z0-9]", "_") %>%
    # letters only in the beginning
    str_replace_all("^(_*)(.+)", "\\2\\1") %>%
    # tolower
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

# trim vesselofficialnumber, there are 273 white spaces in Feb 2023
trim_all_vessel_ids_simple <-
  function(csvs_clean_ws, col_name_to_trim = NA) {
    csvs_clean <- lapply(csvs_clean_ws, function(x) {
      if (is.na(col_name_to_trim)) {
        col_name_to_trim <- grep("vessel.*official.*number",
                                 tolower(names(x)),
                                 value = T)
      }
      col_name_to_trim_s <- sym(col_name_to_trim)
      # Hard code vessel_official_number as vessel id
      x %>%
        mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
        # mutate({{col_name_to_trim_s}} := trimws(!!col_name_to_trim_s)) %>%
        return()
    })
    return(csvs_clean)
  }

# cleaning, regularly done for csvs downloaded from PHIER
clean_all_csvs <- function(csvs, vessel_id_field_name = NA) {
  # unify headers
  csvs_clean0 <- lapply(csvs, clean_headers)
  # trim vesselofficialnumber, just in case
  # browser()
  csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0, vessel_id_field_name)
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
  # # Previously
  # across(a:b, mean, na.rm = TRUE)
  #
  # # Now
  # across(a:b, \(x) mean(x, na.rm = TRUE))
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
  contactdate_field_name <- find_col_name(all_data_df_clean, "contact", "date")[1]
  vessel_id_field_name <- find_col_name(all_data_df_clean, "vessel", "number")[1]

  # browser()
  all_data_df_clean %>%
    # add a new column with a "yes" if there is a contactdate (and a "no" if not)
    # TODO: as.factor
    mutate(was_contacted = if_else(is.na(contactdate_field_name), "no", "yes")) %>%
    # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
    add_count(!!sym(vessel_id_field_name), was_contacted, name = "contact_freq") %>%
    return()
}

# Get frequencies for each column in the list
# usage:
# group_by_arr <- c("vesselofficialnumber", "contacttype")
# count_by_column_arr(my_df, group_by_arr)
count_by_column_arr <- function(my_df, group_by_arr) {
  my_df %>%
    arrange(group_by_arr[1]) %>%
    group_by_at(group_by_arr) %>%
    summarise(my_freq = n()) %>%
    return()
}

count_uniq_by_column <- function(my_df) {
  sapply(my_df, function(x) length(unique(x))) %>%
    as.data.frame()
}

data_overview <- function(my_df) {
  summary(my_df) %>% print()
  cat("\nCount unique values in each column:")
  count_uniq_by_column(my_df)
}

# from https://stackoverflow.com/questions/53781563/combine-rows-based-on-multiple-columns-and-keep-all-unique-values
# concat_unique <- function(x){paste(unique(x),  collapse=', ')}

concat_unique <-
  function(x) {
    paste0(unique(x[!is.na(x)]), collapse = ", ")
  }

print_df_names <- function(my_df, names_num = 100) {
  names(my_df) %>%
    head(names_num) %>%
    paste0(collapse = ", ") %>%
    return()
}

combine_rows_based_on_multiple_columns_and_keep_all_unique_values <- function(my_df, group_by_arr) {
  my_df %>%
    group_by_at(group_by_arr) %>%
    summarise_all(concat_unique) %>%
    return()
}

concat_unique_sorted <-
  function(x) {
    paste0(unique(sort(x[!is.na(x)])), collapse = ", ")
  }

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
              .default = file.path(add_path_compl,  x)
    )
  } )
  paste(my_list) %>% as.list() %>% return()
}

get_compl_and_corresp_data <- function(my_paths, filenames = csv_names_list_22_23, vessel_id_field_name = NA) {
  # browser()
  # add my additional folder names
  csv_names_list <- prepare_csv_names(filenames)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
# browser()
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents, vessel_id_field_name)

  # ---- specific correspondence manipulations ----
  corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

  ## ---- specific compliance manipulations ----
  compl_arr <- csvs_clean1[2:length(csvs_clean1)]

  compl_clean <- compliance_cleaning(compl_arr)
  return(list(compl_clean, corresp_arr_contact_cnts_clean))
}

# ---- specific correspondence manipulations ----
corresp_cleaning <- function(csvs_clean1){
  corresp_arr <- csvs_clean1[[1]]
  # add a new column with a "yes" if there is a contactdate (and a "no" if not),
  # group by vesselofficialnumber and count how many "contacts" are there for each. Save in the "contact_freq" column.
  # browser()
  corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
  createdon_field_name <- find_col_name(corresp_arr, "created", "on")[1]
  contactdate_field_name <- find_col_name(corresp_arr, "contact", "date")[1]
  # change classes from char to POSIXct
  corresp_arr_contact_cnts %>%
    change_to_dates(createdon_field_name, "%m/%d/%Y %H:%M") %>%
    change_to_dates(contactdate_field_name, "%m/%d/%Y %I:%M %p") ->
    corresp_arr_contact_cnts_clean

  return(corresp_arr_contact_cnts_clean)
}

## ---- specific compliance manipulations ----
compliance_cleaning <- function(compl_arr){
  # if it is one df already, do nothing
  compl <- compl_arr
  # else combine separate dataframes for all years into one
  if (!length(compl_arr) == 1) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  permitgroupexpiration <- grep("permit.*group.*expiration",
                           tolower(names(compl)),
                           value = T)

  compl %>%
    # split week column (52: 12/26/2022 - 01/01/2023) into 3 columns with proper classes, week_num (week order number), week_start and week_end
    clean_weeks() %>%
    # change dates classes from char to POSIXct
    change_to_dates(permitgroupexpiration, "%m/%d/%Y") %>%
    return()
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

#
# benchmarking to insert inside a function
# browser()
# time_for_appl <<- benchmark(replications=rep(10, 3),
                            # lapply(myfiles, read.csv, skipNul = TRUE, header = TRUE),
                            # sapply(myfiles, read.csv, skipNul = TRUE, header = TRUE, simplify = TRUE)
                            # ,
                            # columns = c('test', 'elapsed', 'relative')
# )

# write.csv(time_for_appl, "time_for_appl.csv")

# or
# browser()
# sappl_exp <- function(){
#   sapply(my_df, function(x) length(unique(x))) %>% as.data.frame()
# }
#
# map_exp <- function(){
#   my_fun <- function(x) length(unique(x))
#   map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% group_by(test) %>% summarise(sum(elapsed))

connect_to_secpr <- function() {
  # usage:
  # con <- connect_to_secpr()
  my_username <- keyring::key_list("SECPR")[1, 2]
  con = dbConnect(
    dbDriver("Oracle"),
    username = my_username,
    password = keyring::key_get("SECPR",
                                my_username),
    dbname = "SECPR"
  )
  return(con)
}

# usage: complianceerrors_field_name <- find_col_name(compl_clean_sa, ".*xcompliance", "errors.*")[1]
# TODO what if two names?
find_col_name <- function(mydf, start_part, end_part) {
  to_search <- paste0(start_part, ".*", end_part)
  grep(to_search,
       tolower(names(mydf)),
       value = T)
}

# https://stackoverflow.com/questions/23986140/how-to-call-exists-without-quotation-marks
# usage: vexists(con_psql, bogus_variable_name)
vexists <- function(...) {
  vars <- as.character(substitute(...()))
  sapply(vars, exists)
}

# make a separate legend for grid.arrange
legend_for_grid_arrange <- function(legend_plot) {
  # legend_plot <-
  #   ggplot(data = legend_data, aes(x1, y1, colour = ll)) +
  #   geom_text(dat = legend_data,
  #             aes(label = ll),
  #             hjust = 0) +
  #   scale_color_manual(
  #     name = 'Lines',
  #     breaks = c('Mean', 'Num of weeks'),
  #     values = my_colors
  #   )
  #
  # legend_plot

  my_legend <-
    cowplot::get_legend(legend_plot)

  return(my_legend)
}

make_a_flat_file <-
  function(flat_file_name,
           files_to_combine_list) {
    # write to file
    sink(flat_file_name)

    for (i in 1:length(files_to_combine_list)) {
      current_file = readLines(files_to_combine_list[i])
      cat("\n\n#### Current file:", files_to_combine_list[i], "----\n\n")
      cat(current_file, sep = "\n")
    }

    sink()
  }

separate_permits_into_3_groups <- function(my_df, permit_group_field_name = "permitgroup") {
  my_df %>%
  mutate(permit_sa_gom =
           case_when(
             !grepl("RCG|HRCG|CHG|HCHG", !!sym(permit_group_field_name)) ~ "sa_only",
             !grepl("CDW|CHS|SC", !!sym(permit_group_field_name)) ~ "gom_only",
             .default = "dual"
           )) %>%
    return()
}

# read_rds_or_run <-
#   function(my_file_path,
#            my_data_list_of_dfs,
#            my_function) {
#     # browser()
#
#     if (file.exists(my_file_path)) {
#       # read a binary file saved previously
#       my_df <-
#         readr::read_rds(my_file_path)
#     } else {
#       tic("run the function")
#       my_df <-
#         my_function(my_data_list_of_dfs)
#       toc()
#
#       # write all as binary
#       readr::write_rds(my_df,
#                        my_file_path)
#     }
#
#     return(my_df)
#   }

read_rds_or_run <-
  function(my_file_path,
           my_data = as.data.frame(""),
           my_function) {
    # browser()

    if (file.exists(my_file_path)) {
      # read a binary file saved previously
      my_result <-
        readr::read_rds(my_file_path)
    } else {
      msg_text <- paste(today(), "run the function")
      tic(msg_text)
      my_result <-
        my_function(my_data)
      toc()

      # write all as binary
      readr::write_rds(my_result,
                       my_file_path)
    }

    return(my_result)
  }


#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/egregious_violators/get_data_egregious_violators.R ----

# get data for egregious violators
# Download from FHIER first
csv_names_list_22_23 = c("Correspondence__08_01_2023.csv",
                         r"(FHIER_Compliance_2022__08_01_2023.csv)",
                         r"(FHIER_Compliance_2023__08_01_2023.csv)")

data_file_date <- today()
  # lubridate::mdy("06_22_2023") 

## ---- get csv data into variables ----
my_paths$inputs <- file.path(my_paths$inputs, "from_Fhier")
# [1] "C:/Users/anna.shipunova/Documents/R_files_local/my_inputs"

# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\from_Fhier\Correspondence\Correspondence_22_23__06_22_2023.csv"
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]


#### Current file: C:/Users/anna.shipunova/Documents/R_code_github/egregious_violators/egregious_violators.R ----

# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

library(zoo)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
current_project_name <- "egregious_violators"
current_project_path <-
  file.path(my_paths$git_r, current_project_name)

source(file.path(current_project_path, "get_data_egregious_violators.R"))

## check ----
check_new_vessels <-
  function(my_df) {
    list_to_check <-
      c("FL1848EY",
        "FL4232JY",
        "1246468",
        "FL7549EJ")
    my_df |>
      filter(vessel_official_number %in% list_to_check) |>
      select(vessel_official_number) |>
      distinct() |>
      dim() %>%
      return()
  }


# ---- Preparing compliance info ----

## ---- add permit_expired column ----
check_new_vessels(compl_clean)
# 4

compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is more than a month from data_file_date than "no"
  mutate(permit_expired = case_when(permitgroupexpiration > (data_file_date + 30) ~ "no",
                                    .default = "yes"))

## ---- add year_month column ----

number_of_weeks_for_non_compliancy = 27
days_in_27_weeks <- number_of_weeks_for_non_compliancy*7

half_year_ago <-
  data_file_date - days_in_27_weeks

compl_clean_w_permit_exp_last_27w <-
  compl_clean_w_permit_exp |>
  mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last 28 weeks
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)
# today()
# [1] "2023-08-10"
# [1] 235509     22

check_new_vessels(compl_clean_w_permit_exp)
# 4

# 185538
# [1] 217772     22
dim(compl_clean_w_permit_exp_last_27w)
# [1] 74809    23
# [1] 70118    23
# [1] 92370    23 (7m)
# [1] 81153    23 189 d
# [1] 87826    23
# [1] 74169    23

check_new_vessels(compl_clean_w_permit_exp_last_27w)
# 4

## ---- Have only SA permits, exclude those with Gulf permits ----

compl_clean_sa <-
  compl_clean_w_permit_exp_last_27w |>
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

today()
# [1] "2023-08-01"
# [1] "2023-07-10"
# [1] "2023-08-10"

## Not "compliant_" only ----
compl_clean_sa_non_compl <-
  compl_clean_sa |>
  filter(compliant_ == 'NO')

check_new_vessels(compl_clean_sa_non_compl)
# 4

dim(compl_clean_sa_non_compl)
# [1] 18205    23
# [1] 11473    23
# [1] 10597    23
# [1] 12484    23
# [1] 15549    23
# [1] 13992    23
# [1] 14204    23
# [1] 12454    23

compl_clean_sa_non_compl |>
  count_uniq_by_column() |> head(1)
# vesselofficialnumber 1785
# today()
# "2023-06-23"
# vessel_official_number 1573
# [1] "2023-07-10"
# vessel_official_number 1403
# vessel_official_number 1369
# [1] "2023-08-01"
# vessel_official_number 1370
# vessel_official_number 1328

## filter for egregious ----
### check if there is no "compliant_ == YES" since half_year_ago ----

last_week_start <- data_file_date - 6

compl_clean_sa |> check_new_vessels()
# 4

compl_clean_sa_non_c_not_exp <-
  compl_clean_sa |>
  # not compliant
  filter(tolower(compliant_) == "no") |>
  # in the last 27 week
  dplyr::filter(week_start > half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_start < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

dim(compl_clean_sa_non_c_not_exp)
# [1] 10419    23
# [1] 9486   23
# [1] 9315   23

compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

compl_clean_sa_all_weeks_non_c_short <-
  compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  dplyr::arrange(dplyr::desc(compl_weeks_amnt),
                 vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # all weeks were...
  filter(total_weeks >= (number_of_weeks_for_non_compliancy - 3)) |> 
  # ...non compliant
  filter(compl_weeks_amnt == total_weeks)

compl_clean_sa_non_c_not_exp |>
  dplyr::select(vessel_official_number, week, compliant_) |>
  dplyr::add_count(vessel_official_number,
                   name = "total_weeks") |>
  dplyr::add_count(vessel_official_number, compliant_,
                   name = "compl_weeks_amnt") |>
  # dim()
  # [1] 9486    5
  # [1] 9315    5
  dplyr::arrange(dplyr::desc(compl_weeks_amnt), vessel_official_number) |>
  dplyr::select(-week) |>
  dplyr::distinct() |>
  # dim()
  # [1] 1045    4
  # all weeks were non compliant
  filter(compl_weeks_amnt == total_weeks) |>
    glimpse()

dim(compl_clean_sa_all_weeks_non_c_short)
# 121

### add back columns needed for the output ----
need_cols_names <- c(
  "vessel_official_number",
  "name",
  "permit_expired",
  "permitgroup",
  "permitgroupexpiration"
  # ,
  # "week_start"
)
compl_clean_sa_non_c_not_exp |> check_new_vessels()
# 3

# dim(compl_clean_sa_non_c_not_exp)
compl_clean_sa_all_weeks_non_c <-
  compl_clean_sa_non_c_not_exp |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_sa_all_weeks_non_c_short) |>
# Joining with `by = join_by(vessel_official_number)`
  distinct()

dim(compl_clean_sa_all_weeks_non_c)
# [1] 130   8
# 0
# 127
# 121

## check the last report date ----
compl_clean_sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_sa_all_weeks_non_c_short |>
  select(vessel_official_number) |>
  distinct()

# compl_clean_sa_non_c_not_exp |> 
dim(compl_clean_sa_all_weeks_non_c_short_vesl_ids)
# [1] 121   1

compl_clean_sa |>
  filter(
    vessel_official_number %in% compl_clean_sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # View()
  group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes") |>
  mutate(latest_compl = max(week_num)) |>
  glimpse()

# TODO: add check for earlier weeks

## ---- Preparing Correspondence ----

## ---- remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean) |>
  head(1)
# vesselofficial_number   3223
# vessel_official_number  3371
# vesselofficial_number 3434

# "2023-08-09"
# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

## new requirement 2023-08-09 ----
# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

two_attempts_filter <-
  quo(contact_freq > 1 &
        any(tolower(contacttype) == "call"))

corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  filter(!!two_attempts_filter)

test_new_egr2 <-
  corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()

test_new_egr2[1] == 4
# T

dim(corresp_contact_cnts_clean)
# [1] 18629    23
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

data_overview(corresp_contact_cnts_clean_direct_cnt_2atmps) |>
  head(1)
# vesselofficial_number 2968
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

## ---- Combine compliance information with filtered correspondence info by vesselofficialnumber ----

corresp_contact_cnts_clean_direct_cnt_2atmps |>
  check_new_vessels()
# 4

compl_clean_sa_all_weeks_non_c |>
  check_new_vessels()
# 2

compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps,
    compl_clean_sa_all_weeks_non_c,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation1)
# [1] 486  30
# [1] 522  30

# check
count_uniq_by_column(compl_corr_to_investigation1) |>
  head(1)
# 110
# 107
# 27: 177
# vesselofficial_number 188
# vesselofficial_number 105
# 108
# 97
# vesselofficial_number 116

## ---- output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## ---- 1) create additional columns ----

## ----- list of contact dates and contact type in parentheses  -----

# # put names into vars
# contactdate_field_name <-
#   find_col_name(compl_corr_to_investigation1, "contact", "date")[1]
# contacttype_field_name <-
#   find_col_name(compl_corr_to_investigation1, "contact", "type")[1]
# 
# write.csv(compl_corr_to_investigation1,
#           file.path(
#             my_paths$outputs,
#             paste0(              "more_than_24_compl_corr_to_investigation1_22_23__",
#               today(),
#               ".csv"
#             )
#           ),
#           row.names = FALSE)
# # 435 dplyr::distinct ids

get_date_contacttype <-
  function(compl_corr_to_investigation1) {
    compl_corr_to_investigation1 |>
      # add a new column date__contacttype with contactdate and contacttype
      mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) |>
      # use 2 columns only
      select(vessel_official_number, date__contacttype) |>
      # [1] 49903     2
      # sort
      arrange(vessel_official_number, date__contacttype) |>
      dplyr::distinct() |>
      group_by(vessel_official_number) |>
      # [1] 1125    2
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = paste(date__contacttype, collapse = ", ")) %>%
      # [1] 435   2
      return()
  }

date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation1)
dim(date__contacttype_per_id)
# [1] 110    2
# 107
# 27: 177
# 188   2
# 105   2 (the new filter)
# 108
# 97
# [1] 116   2 (2 contact attempts)

compl_corr_to_investigation1 |>
  check_new_vessels()
# 2

date__contacttype_per_id |>
  check_new_vessels()
# 2

## ---- combine output ----
compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation1 |>
  inner_join(date__contacttype_per_id,
             by = "vessel_official_number")

dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 264  31
# 309
# 271
# [1] 522  31

## ---- 2) remove duplicated columns ----

contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  select(
    "vessel_official_number",
    "name",
    "permit_expired",
    "permitgroup",
    "permitgroupexpiration",
    "contactrecipientname",
    !!contactphonenumber_field_name,
    "contactemailaddress",
    # "week_start",
    "date__contacttypes"
  ) |>
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number")

dim(compl_corr_to_investigation1_short)
# [1] 107   9
# 27: [1] 177  10
# [1] 105   9
# 108
# str(compl_corr_to_investigation1_short)
# 97
# 116   9

## ---- 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# today()
# [1] "2023-07-11"
# Data from the previous tab of "egregious violators for investigation"
# Download first
previous_egr_data_path <-
  file.path(
    my_paths$outputs, current_project_name,
    r"(from_web\egregious violators for investigation - 2023-01-24_to_2023-08-01.csv)"
  )

file.exists(previous_egr_data_path)
# T
vessels_to_mark <-
  read_csv(previous_egr_data_path)

# data_overview(vessels_to_remove)

vessels_to_mark_ids <-
  vessels_to_mark |>
  # filter(tolower(`Contacted 2x?`) == 'yes') |>
  select(vessel_official_number)

# mark these vessels
compl_corr_to_investigation1_short_dup_marked <-
  compl_corr_to_investigation1_short |>
  mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation1_short_dup_marked)
# [1] 177  11
# [1] 105  10
# 108
# 97
# [1] 110  10 2 atmpts
# [1] 116  10

dim(compl_corr_to_investigation1_short_dup_marked)
# 102
# 27: 164
# 177
# 31
# 108

#### check ----
  # no applicable method for 'distinct' applied to an object of class "character"

length(unique(compl_corr_to_investigation1_short_dup_marked$vessel_official_number))
# 107
# 102
# 27: 164
# 177
# 105
# 108
# 97
# 110
# 116

data_overview(compl_corr_to_investigation1_short_dup_marked) |> head(1)
# vessel_official_number
# 177
# 105
# 108
# 110
# 116

## add comments from the compliance crew (if already exist) ----
# results_with_comments_path <-
#   file.path(
#     my_paths$outputs,
#     current_project_name,
#     r"(from_web\egregious violators for investigation - 06-26-2023.csv)"
#   )

# file.exists(results_with_comments_path)
# T

# results_with_comments <-
#   readr::read_csv(results_with_comments_path,
#                   col_types = cols(.default = 'c'))
#
# dim(results_with_comments)
# 134 13

# all.equal(results_with_comments,
#           compl_corr_to_investigation1_short_output)
# F

# setdiff(results_with_comments$vessel_official_number,
#         compl_corr_to_investigation1_short_dup_marked$vessel_official_number) |>
#   length()
# 68
# 35 (new filter)
# 67
# 71
# in_the_new_res_only <-
#   setdiff(
#     compl_corr_to_investigation1_short_dup_marked$vessel_official_number,
#     results_with_comments$vessel_official_number
#   )
# |> cat()
# 1266718 602091 FL0435LD FL6279PH FL7282LE FL8725DA
# length(in_the_new_res_only)
# 47
# 6
# 1061382 1069364 1168496 1209015 1224219 1259129 1266718 1296710 1308401 1318467 1331794 523112 602091 678141 970286 996263 FL0435LD FL2447TL FL2453TE FL3159TK FL3697PB FL3979EA FL4801NV FL6279PH FL6680JK FL6954LD FL7772SV FL8090RU FL8666CH FL8725DA FL9131RJ FL9446TH FL9793RU FL9914GX GA8847NJ MD9128BD MS8535ZG NC2851DH NC4246DP NC9819DF VA1460CJ
# 34

### join comments

# by = join_by(
#   vessel_official_number,
#   name,
#   permit_expired,
#   permitgroup,
#   permitgroupexpiration,
#   contactrecipientname,
#   contactphone_number,
#   contactemailaddress,
#   date__contacttypes
# )

# compl_corr_to_investigation1_short_output_w_comments <-
#   left_join(compl_corr_to_investigation1_short_dup_marked,
#             results_with_comments,
#             by,
#             # Override the default suffixes, c(".x", ".y") in not merged cols
#     suffix = c(".my_output",
#                ".commented_output")
#             )
# Joining with `by = join_by(vessel_official_number, name, permit_expired,
# permitgroup, permitgroupexpiration, contactrecipientname,
# contactphone_number, contactemailaddress, week_start, date__contacttypes)`

# dim(compl_corr_to_investigation1_short_output_w_comments)
# 38
# 280
# 0
# [1] 105  14
# 108
# 97
# [1] 110  14

#### check no comments ----
# no_comments_vsls <-
#   compl_corr_to_investigation1_short_output_w_comments |>
#   filter(is.na(
#     `Confirmed Egregious? (missing past 6 months, 2 contacts with at least 1 call)`
#   ))
# # |>
# View(no_comments_vsls)
# Rows: 53
# Columns: 14

# in_the_new_res_only_df <-
#   as.data.frame(in_the_new_res_only)
# names(in_the_new_res_only_df) <- "vessel_official_number"

# no_comments_vsls_ids <-
#   no_comments_vsls |>
#   select(vessel_official_number)
# dim(no_comments_vsls_ids)
# 62

# no_comments_vsls_ids |>
#   filter(vessel_official_number == '1305207') |> dim()
# 1
# compl_corr_to_investigation1_short_output_w_comments |>
#   filter(vessel_official_number == '1305207') |> dim()
# [1]  1 21

# setdiff(no_comments_vsls_ids$vessel_official_number, in_the_new_res_only_df) |>
#   length()
# 1305207
# 62

# setdiff(in_the_new_res_only_df, no_comments_vsls_ids$vessel_official_number)
# 0

# output ----
result_file_path <- file.path(
  my_paths$outputs,
  current_project_name,
  paste0(
    "egregious_violators_for_investigation_from_",
    half_year_ago,
    "_to_",
    data_file_date,
    ".csv"
  ))
# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\egregious_violators_for_investigation_from_2023-01-24_to_2023-08-01.csv"

readr::write_csv(
  compl_corr_to_investigation1_short_dup_marked,
    # compl_corr_to_investigation1_short_output_w_comments,
  result_file_path,
  na = "")

compl_corr_to_investigation1_short_dup_marked |>
  check_new_vessels()
# 2
# FL4232JY
# FL7549EJ

