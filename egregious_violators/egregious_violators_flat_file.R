# Current file: ~/R_code_github/useful_functions_module.r ----

# nolint: commented_code_linter
# useful functions

## start functions ---
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

## functions to clean FHIER compliance and correspondense reports ----

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
        dplyr::mutate(vessel_official_number = trimws(!!col_name_to_trim_s)) %>%
        # dplyr::mutate({{col_name_to_trim_s}} := trimws(!!col_name_to_trim_s)) %>%
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
    dplyr::mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
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
    dplyr::mutate(dplyr::across(all_of(field_names_arr), aux_fun_for_dates, date_format)) %>%

    # dplyr::mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
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
    dplyr::mutate(was_contacted = if_else(is.na(contactdate_field_name), "no", "yes")) %>%
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

  # specific correspondence manipulations ----
  corresp_arr_contact_cnts_clean <- corresp_cleaning(csvs_clean1)

  ## specific compliance manipulations ----
  compl_arr <- csvs_clean1[2:length(csvs_clean1)]

  compl_clean <- compliance_cleaning(compl_arr)
  return(list(compl_clean, corresp_arr_contact_cnts_clean))
}

# specific correspondence manipulations ----
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

## specific compliance manipulations ----
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
      cat("\n\n# Current file:", files_to_combine_list[i], "----\n\n")
      cat(current_file, sep = "\n")
    }

    sink()
  }

separate_permits_into_3_groups <- function(my_df, permit_group_field_name = "permitgroup") {
  my_df %>%
  dplyr::mutate(permit_sa_gom =
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


# Current file: C:/Users/anna.shipunova/Documents/R_code_github/egregious_violators/db_functions.R ----

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)


#### from the main file:
library(zoo)
# library(RColorBrewer)

# ----set up----
my_paths <- set_work_dir()
current_project_name <- "egregious_violators"
current_project_path <-
  file.path(my_paths$git_r, current_project_name)


# Current file: C:/Users/anna.shipunova/Documents/R_code_github/egregious_violators/get_data_egregious_violators.R ----

library(lubridate)
library(tictoc)
library(stringr)

# get data for egregious violators
# Download from FHIER first:
# Home / Correspondence
# and
# Reports / FHIER Compliance Report

csv_names_list_22_23 = c("Correspondence__08_01_2023.csv",
                         r"(FHIER_Compliance_2022__08_01_2023.csv)",
                         r"(FHIER_Compliance_2023__08_01_2023.csv)")

data_file_date <- today()

## get csv data into variables ----
all_inputs <- my_paths$inputs
my_paths$inputs <- file.path(my_paths$inputs, "from_Fhier")

temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_22_23)

compl_clean <- temp_var[[1]]
corresp_contact_cnts_clean0 <- temp_var[[2]]

# get vessels, permits and participants info from the db ----

# get_vessels with permits and participants ----
vessel_permit_where_part <-
  "
    p.permit_status <> 'REVOKED'
      AND p.top IN ( 'CHG', 'HCHG', 'HRCG', 'RCG', 'CHS',
                     'SC', 'CDW' )
      AND ( p.expiration_date >= ( sysdate - ( 365 / 2 ) )
            OR p.end_date >= ( sysdate - ( 365 / 2 ) ) )
      AND nvl(p.end_date, p.expiration_date) IS NOT NULL
"

vessel_permit_fields_part <-
  "   v.sero_home_port_city,
      v.sero_home_port_county,
      v.sero_home_port_state,
      v.sero_official_number,
      v.coast_guard_nbr,
      v.event_id,
      v.hull_id_nbr,
      v.owner_id,
      v.state_reg_nbr,
      v.status v_status,
      v.supplier_vessel_id,
      v.ue,
      v.vessel_id v_vessel_id,
      v.vessel_name,
      p.effective_date,
      p.end_date,
      p.entity_id,
      p.expiration_date,
      p.new_owner,
      p.permit,
      p.permit_status,
      p.prior_owner,
      p.vessel_alt_num,
      p.vessel_id p_vessel_id
"

vessels_permits_from_part <-
"FROM
       srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov p,
  safis.vessels@secapxdv_dblk.sfsc.noaa.gov v
"

vessels_permits_query <-
  str_glue(
  "SELECT
  {vessel_permit_fields_part}
  {vessels_permits_from_part}
  WHERE
    ( p.vessel_id = sero_official_number
  OR
    p.vessel_id = state_reg_nbr
  OR
    p.vessel_id = coast_guard_nbr )
  AND
  {vessel_permit_where_part}
  ")

vessels_permits_participants_query <-
  paste0(
  "SELECT
  v_p.*,

  f_p.first_name,
  f_p.middle_name,
  f_p.last_name,
  f_p.name_suffix,
  f_p.address_1,
  f_p.address_2,
  f_p.state,
  f_p.postal_code,
  f_p.phone_nbr,
  f_p.email,
  f_p.license_nbr,
  f_p.participant_id,
  f_p.permit_id,
  f_p.status f_p_status
FROM
       safis.full_participant@secapxdv_dblk.sfsc.noaa.gov f_p
  JOIN (",
  vessels_permits_query,
  ") v_p
  ON ( to_char(license_nbr) = to_char(entity_id) )"
  )

vessels_permits_participants_file_path <-
  file.path(all_inputs,
            current_project_name,
            "vessels_permits_participants.rds")

vessels_permits_participants_fun <-
  function(vessels_permits_participants) {
    return(dbGetQuery(con,
                      vessels_permits_participants))
  }

vessels_permits_participants <-
  read_rds_or_run(
    vessels_permits_participants_file_path,
    vessels_permits_participants_query,
    vessels_permits_participants_fun
  )

dim(vessels_permits_participants)
# [1] 31942    38

# Current file: C:/Users/anna.shipunova/Documents/R_code_github/egregious_violators/egregious_violators.R ----

# see read.me

# Get common functions
source("~/R_code_github/useful_functions_module.r")

# Preparing compliance info ----

## add permit_expired column ----
compl_clean_w_permit_exp <-
  compl_clean |>
  # if permit group expiration is more than a month from data_file_date than "no"
  dplyr::mutate(permit_expired = case_when(permitgroupexpiration > (data_file_date + 30) ~ "no",
                                    .default = "yes"))

## add year_month column ----
number_of_weeks_for_non_compliancy = 27
days_in_27_weeks <- number_of_weeks_for_non_compliancy*7

half_year_ago <-
  data_file_date - days_in_27_weeks

compl_clean_w_permit_exp_last_27w <-
  compl_clean_w_permit_exp |>
  # convert
  dplyr::mutate(year_month = as.yearmon(week_start)) |>
  # keep entries for the last 28 weeks
  filter(year_month >= as.yearmon(half_year_ago))

dim(compl_clean_w_permit_exp)

dim(compl_clean_w_permit_exp_last_27w)
# [1] 74169    23

## Have only SA permits, exclude those with Gulf permits ----
compl_clean_sa <-
  compl_clean_w_permit_exp_last_27w |>
  filter(!grepl("RCG|HRCG|CHG|HCHG", permitgroup))

## Not "compliant_" only ----
compl_clean_sa_non_compl <-
  compl_clean_sa |>
  filter(compliant_ == 'NO')

dim(compl_clean_sa_non_compl)
# [1] 12454    23

compl_clean_sa_non_compl |>
  count_uniq_by_column() |> head(1)
# vessel_official_number 1328

## filter for egregious ----
### check if there is no "compliant_ == YES" since half_year_ago ----

last_week_start <- data_file_date - 6

compl_clean_sa_non_c_not_exp <-
  compl_clean_sa |>
  # not compliant
  filter(tolower(compliant_) == "no") |>
  # in the last 27 week
  dplyr::filter(week_start >= half_year_ago) |>
  # before the last week (a report's grace period)
  dplyr::filter(week_start < last_week_start) |>
  # not expired
  dplyr::filter(tolower(permit_expired) == "no")

dim(compl_clean_sa_non_c_not_exp)

# last month (3-4 weeks) is often not taken in the account yet, e.i. there are less weeks in the compliant report than 27.
grace_period_weeks <- 4

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
  filter(total_weeks >= (number_of_weeks_for_non_compliancy - grace_period_weeks)) |>
  # ...non compliant
  filter(compl_weeks_amnt == total_weeks)

dim(compl_clean_sa_all_weeks_non_c_short)
# [1] 128   4

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

compl_clean_sa_all_weeks_non_c <-
  compl_clean_sa_non_c_not_exp |>
  select(all_of(need_cols_names)) |>
  inner_join(compl_clean_sa_all_weeks_non_c_short) |>
# Joining with `by = join_by(vessel_official_number)`
  distinct()

dim(compl_clean_sa_all_weeks_non_c)
# 128

## check the last report date ----
# ids only
compl_clean_sa_all_weeks_non_c_short_vesl_ids <-
  compl_clean_sa_all_weeks_non_c_short |>
  select(vessel_official_number) |>
  distinct()

dim(compl_clean_sa_all_weeks_non_c_short_vesl_ids)
# [1] 128   1

# the info from the full compliance info for theses ids, not only not-compliant
compl_clean_sa |>
  filter(
    vessel_official_number %in% compl_clean_sa_all_weeks_non_c_short_vesl_ids$vessel_official_number
  ) |>
  # dim()
  # [1] 3146   23
  # for each vessel
  group_by(vessel_official_number) |>
  filter(tolower(compliant_) == "yes" &
           # not the current month
           year_month < as.yearmon(data_file_date)) |>
  # get only the latest compliant weeks
  dplyr::mutate(latest_compl = max(week_num)) |>
  filter(week_num == latest_compl) |>
  ungroup() |>
  select(
    # vessel_official_number,
    year_month,
    latest_compl) |>
  distinct() |>
  glimpse()
# $ year_month   <yearmon> Jul 2023
# $ latest_compl <int> 31

# TODO: add check for earlier weeks

# Preparing Correspondence ----

## remove 999999 ----
corresp_contact_cnts_clean <-
  corresp_contact_cnts_clean0 |>
  filter(!grepl("^99999", vessel_official_number))

data_overview(corresp_contact_cnts_clean) |>
  head(1)
# vesselofficial_number 3434

## New correspondence requirement 2023-08-09 ----

# Michelle
# It should be at least 2 contact "attempts". i.e., if they are ignoring our calls and emails then they cannot continue to go on in perpetuity without reporting and never be seen as egregious. So, at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough at this point and they need to be passed to OLE.

# at least 1 call (could be a voicemail) and also at a 2nd call (could be a voicemail) or an email. So, if we called 1x and left a voicemail and then attempted an email, then we have tried enough

# save the filter
two_attempts_filter <-
  # mopre than one contact
  quo(contact_freq > 1 &
        # at least one is a call
        any(tolower(contacttype) == "call"))

# use the filter
corresp_contact_cnts_clean_direct_cnt_2atmps <-
  corresp_contact_cnts_clean |>
  filter(!!two_attempts_filter)

dim(corresp_contact_cnts_clean)
# [1] 18629    23
dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    23

data_overview(corresp_contact_cnts_clean_direct_cnt_2atmps) |>
  head(1)
# vesselofficial_number 2968

dim(corresp_contact_cnts_clean_direct_cnt_2atmps)
# [1] 18163    22

# Combine compliance information with filtered correspondence info by vessel_official_number ----

compl_corr_to_investigation1 <-
  inner_join(
    corresp_contact_cnts_clean_direct_cnt_2atmps,
    compl_clean_sa_all_weeks_non_c,
    by = c("vessel_official_number"),
    multiple = "all",
    relationship = "many-to-many"
  )

dim(compl_corr_to_investigation1)
# [1] 558  29

# check
count_uniq_by_column(compl_corr_to_investigation1) |>
  head(1)
# vesselofficial_number 123

## output needed investigation ----
# 1) create additional columns
# 2) remove duplicated columns
# 3) remove vessels already in the know list

## 1) create additional columns ----

### list of contact dates and contact type in parentheses  ----

get_date_contacttype <-
  function(compl_corr_to_investigation1) {
    compl_corr_to_investigation1 |>
      # add a new column date__contacttype with combined contactdate and contacttype
      dplyr::mutate(date__contacttype = paste(contactdate_field_name, contacttype, sep = " ")) |>
      # use 2 columns only
      select(vessel_official_number, date__contacttype) |>
      # [1] 49903     2
      # sort
      dplyr::arrange(vessel_official_number, date__contacttype) |>
      dplyr::distinct() |>
      group_by(vessel_official_number) |>
      # for each vessel id combine all date__contacttypes separated by comma in one cell
      summarise(date__contacttypes = paste(date__contacttype, collapse = ", ")) %>%
      return()
  }

# run the function
date__contacttype_per_id <-
  get_date_contacttype(compl_corr_to_investigation1)

dim(date__contacttype_per_id)
# 123

# add permit and address info ----
### check ----
vessels_permits_participants_v_ids <-
  vessels_permits_participants |>
  select(P_VESSEL_ID) |>
  distinct()

dim(vessels_permits_participants_v_ids)
# [1] 3302    1

# how many vessels are missing from the db report
setdiff(
  date__contacttype_per_id$vessel_official_number,
  vessels_permits_participants_v_ids$P_VESSEL_ID
) |> cat(sep = "', '")
# |>
#   length()
# 6
# '1305388', '565041', 'FL0001TG', 'MI9152BZ', 'NC2851DH', 'VA1267CJ'
# (wrong license_nbr in full_participants
# or entity_id in permits,
# check manually)

# We don't need to check the reverse, there will be more vessels in the permit info we are not interested in

# clean up the report
vessels_permits_participants_space <-
  vessels_permits_participants |>
  # remove NAs
  dplyr::mutate(dplyr::across(where(is.character),
                ~ replace_na(., ""))) |>
  # trim trailing spaces, and replaces all internal whitespace with a single space.
  dplyr::mutate(dplyr::across(where(is.character),
                ~ str_squish(.)))

dim(vessels_permits_participants_space)
# [1] 31942    38

# combine info
vessels_permits_participants_short_u <-
  vessels_permits_participants_space |>
  # for each vessel
  group_by(P_VESSEL_ID) |>
  dplyr::mutate(
    sero_home_port = list(unique(
      paste(
        SERO_HOME_PORT_CITY,
        SERO_HOME_PORT_COUNTY,
        SERO_HOME_PORT_STATE
      )
    )),
    full_name = list(unique(
      paste(FIRST_NAME,
            MIDDLE_NAME,
            LAST_NAME,
            NAME_SUFFIX)
    )),
    full_address = list(unique(
        paste(ADDRESS_1,
              ADDRESS_2,
              STATE,
              POSTAL_CODE)
      ))
  ) |>
  # use only new columns and the vessel id
  select(P_VESSEL_ID,
         sero_home_port,
         full_name,
         full_address) |>
  ungroup() |>
  distinct()

dim(vessels_permits_participants_short_u)
# [1] 3302    4

# convert lists in comma separated strings
vessels_permits_participants_short_u_flat <-
  vessels_permits_participants_short_u |>
  dplyr::rowwise() |>
  dplyr::mutate_if(is.list,
            ~ paste(unlist(.),
                    collapse = ', ')) %>%
  # back to colwise
  dplyr::ungroup()

data_overview(vessels_permits_participants_short_u_flat) |>
  head(1)
# P_VESSEL_ID 3302

# clean up weird comma and space combinations
vessels_permits_participants_short_u_flat_sp <-
  vessels_permits_participants_short_u_flat |>
  dplyr::mutate(
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove whitespace at the start and end, and replaces all internal whitespace with a single space.
    ~ stringr::str_squish(.x)
  ),
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove space characters before commas
    ~ stringr::str_replace_all(.x, "\\s+,", ",")
  ),
  dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # replace 2+ commas with one
    ~ stringr::str_replace_all(.x, ",,+", ",")
  ),
  dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove commas at the end
    ~ stringr::str_replace_all(.x, ",$", "")
  ),
    dplyr::across(
    c(sero_home_port,
      full_name,
      full_address),
    # remove commas in front
    ~ stringr::str_replace_all(.x, "^,", "")
  ))

## Manually check missing addresses ----

### From FHIER ----
# REPORTS / For-hire Primary Physical Address List
fhier_addr <-
  read_csv(
    file.path(
      all_inputs,
      "..",
      r"(my_outputs\egregious_violators\For-hire Primary Physical Address List.csv)"
    ),
    # read all as characters
    col_types = cols(.default = 'c'),
    # use the same function for names, see above
    name_repair = fix_names
  )

# fewer fields
fhier_addr_short <-
  fhier_addr |>
  select(
    vessel_official_number,
    permit_holder_names,
    physical_address_1,
    physical_address_2,
    physical_city,
    physical_county,
    physical_state,
    physical_zip_code,
    phone_number,
    primary_email
  ) |>
  dplyr::mutate(
    fhier_address =
      paste(
        physical_address_1,
        physical_address_2,
        physical_city,
        physical_county,
        physical_state,
        physical_zip_code
      )
  ) |>
  select(
    -c(
      physical_address_1,
      physical_address_2,
      physical_city,
      physical_county,
      physical_state,
      physical_zip_code
    )
  )

print_df_names(vessels_permits_participants_short_u_flat_sp)

# join with the previous results from the db
fhier_addr__compl_corr <-
  right_join(
    fhier_addr_short,
    vessels_permits_participants_short_u_flat_sp,
    join_by(vessel_official_number == P_VESSEL_ID)
  )

dim(fhier_addr__compl_corr)
# [1] 3302    8

### add info from FHIER to the results ----

vessels_permits_participants_short_u_flat_sp_add <-
  vessels_permits_participants_short_u_flat_sp |>
  dplyr::left_join(
    fhier_addr__compl_corr,
    dplyr::join_by(
      P_VESSEL_ID == vessel_official_number,
      sero_home_port,
      full_name,
      full_address
    )
  ) |>
  dplyr::mutate(
    full_name =
      dplyr::case_when(
        is.na(full_name) | full_name == "UN" ~
          permit_holder_names,
        .default = full_name
      ),
    full_address =
      dplyr::case_when(
        is.na(full_address) | full_address == "UN" ~
          fhier_address,
        .default = full_address
      )
  ) |>
  dplyr::select(P_VESSEL_ID, sero_home_port, full_name, full_address) |>
  dplyr::distinct()

# combine vessels_permits and date__contacttype ----
vessels_permits_participants_date__contacttype_per_id <-
  dplyr::inner_join(
    date__contacttype_per_id,
    vessels_permits_participants_short_u_flat_sp_add,
    dplyr::join_by(vessel_official_number == P_VESSEL_ID)
  )

dim(vessels_permits_participants_date__contacttype_per_id)
# 117 5

# combine all outputs ----
compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id <-
  compl_corr_to_investigation1 |>
  dplyr::inner_join(vessels_permits_participants_date__contacttype_per_id,
                    by = "vessel_official_number")

dim(compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id)
# [1] 527  33

## 2) remove extra columns ----
# get field names
contactphonenumber_field_name <-
  find_col_name(compl_corr_to_investigation1, ".*contact", "number.*")[1]

compl_corr_to_investigation1_short <-
  compl_corr_to_investigation1_w_non_compliant_weeks_n_date__contacttype_per_id |>
  dplyr::select(
    vessel_official_number,
    name,
    permit_expired,
    permitgroup,
    permitgroupexpiration,
    contactrecipientname,
    !!contactphonenumber_field_name,
    contactemailaddress,
    date__contacttypes,
    sero_home_port,
    full_name,
    full_address
  ) |>
  combine_rows_based_on_multiple_columns_and_keep_all_unique_values("vessel_official_number")

dim(compl_corr_to_investigation1_short)
# 117   12

## 3) mark vessels already in the know list ----
# The first column (report created) indicates the vessels that we have created a case for. My advice would be not to exclude those vessels. EOs may have provided compliance assistance and/or warnings already. If that is the case and they continue to be non-compliant after that, they will want to know and we may need to reopen those cases.

# Data from the previous tab of "egregious violators for investigation"
# Download first and change the name
previous_egr_data_path <-
  file.path(
    my_paths$outputs, current_project_name,
    r"(from_web\egregious violators for investigation - 2023-01-24_to_2023-08-01.csv)"
  )

file.exists(previous_egr_data_path)
# T
vessels_to_mark <-
  readr::read_csv(previous_egr_data_path)

vessels_to_mark_ids <-
  vessels_to_mark |>
  dplyr::select(vessel_official_number)

# mark these vessels
compl_corr_to_investigation1_short_dup_marked <-
  compl_corr_to_investigation1_short |>
  dplyr::mutate(
    duplicate_w_last_time =
      case_when(
        vessel_official_number %in%
          vessels_to_mark_ids$vessel_official_number ~ "duplicate",
        .default = "new"
      )
  )

dim(compl_corr_to_investigation1_short_dup_marked)
# [1] 117  13

#### check ----
length(unique(compl_corr_to_investigation1_short_dup_marked$vessel_official_number))
# 117

data_overview(compl_corr_to_investigation1_short_dup_marked) |> head(1)
# vessel_official_number
# 117

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

readr::write_csv(compl_corr_to_investigation1_short_dup_marked,
                 result_file_path,
                 na = "")
