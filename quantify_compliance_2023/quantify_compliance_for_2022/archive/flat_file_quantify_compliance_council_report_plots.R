#### add-ons 1 ---- 

library(grid)
library(zoo)
library(gridExtra)
library(cowplot)
project_name <- "quantify_compliance"


#### Current file:  ~/R_code_github/useful_functions_module.r  ----

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
# curr_wd <- getwd()
# roracle_path <- r"(C:\Users\anna.shipunova\Software\ROracle_1.3-2\ROracle)"
# setwd(roracle_path)
# install.packages('ROracle')

# library('ROracle')
# drv <- dbDriver("Oracle")
# con <- dbConnect(drv, "USER GOES HERE", "PASSWORD GOES HERE", dbname='XXX')

# library('ROracle')
# drv <- dbDriver("Oracle")
# con <-
#   dbConnect(drv, "USER GOES HERE", "PASSWORD GOES HERE", dbname = 'XXX')
# 
# dbReadTable(con, 'DUAL')


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
  contents <- purrr::map_df(myfiles,
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
    dplyr::add_count(!!sym(vessel_id_field_name), was_contacted, name = "contact_freq") %>%
    return()
}

# Get frequencies for each column in the list
# usage:
# group_by_arr <- c("vesselofficialnumber", "contacttype")
# count_by_column_arr(my_df, group_by_arr)
count_by_column_arr <- function(my_df, group_by_arr) {
  my_df %>%
    dplyr::arrange(group_by_arr[1]) %>%
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

# csv_names_list_22_23 = c("Correspondence.csv",
                         # "FHIER_Compliance_22.csv",
                         # "FHIER_Compliance_23.csv")

# add my additional folder names to each filename
prepare_csv_names <- function(filenames) {
  add_path_corresp <- "Correspondence"
  add_path_compl <- "FHIER Compliance"

  my_list <- sapply(filenames, function(x) {
    dplyr::case_when(startsWith(my_headers_case_function(x), "correspond") ~
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
#   purrr::map_df(my_df, my_fun)
# }
#
# time_for_appl <<- benchmark(replications=rep(10^7, 3),
#                             exp1,
#                             exp2,
#                             columns = c('test', 'elapsed', 'relative')
# )
#
# purrr::map_df(my_df, function(x) length(unique(x)))
# to compare:
# time_for_appl %>% dplyr::group_by(test) %>% summarise(sum(elapsed))

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
      current_file = readr::read_lines(files_to_combine_list[i])
      cat("\n\n#### Current file:", files_to_combine_list[i], "----\n\n")
      cat(current_file, sep = "\n")
    }

    sink()
  }

write_to_1_flat_file <-
  function(flat_file_name,
           file_name_to_write) {
    # write to file
    sink(flat_file_name, append = TRUE)
    
    current_file_text = readr::read_lines(file_name_to_write)
    cat("\n\n#### Current file: ", file_name_to_write, " ----\n\n")
    cat(current_file_text, sep = "\n")
    # sink()
  }

separate_permits_into_3_groups <- function(my_df, permit_group_field_name = "permitgroup") {
  my_df %>%
  mutate(permit_sa_gom =
           dplyr::case_when(
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

# Usage:
# select(-all_of(names(empty_cols)))

empty_cols <-
  function(my_df) {
    my_df |>
      purrr::map_df(function(x) {
        browser()
        if (length(unique(x)) == 1) {
          return(unique(x))
        }
      }) %>%
    return()
  }

create_dir_if_not <-
  function(curr_dir_name) {
    if (!dir.exists(curr_dir_name)) {
      dir.create(curr_dir_name)
    }
  }


#### add-ons 2 ---- 

my_paths <- set_work_dir()


#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_functions.R  ----

# quantify_compliance_functions

text_sizes <- list(
  geom_text_size = 7,
  plot_title_text_size = 10,
  axis_title_text_size = 9,
  axis_text_x_size = 15,
  axis_text_y_size = 15,
  plot_caption_text_size = 12,
  ### common axes for Months ----
  y_left_fontsize = 10
)

get_non_compl_week_counts_percent <- function(my_df, vessel_id_col_name) {
  # browser()
    my_df %>%
    # how many non_compliant weeks per vessel this month
    dplyr::count(year_month, !!sym(vessel_id_col_name),
          name = "nc_weeks_per_vessl_m") %>%
    # nc weeks per month
    dplyr::count(year_month, nc_weeks_per_vessl_m,
          name = "occurence_in_month") %>%
    # turn amount of nc weeks into headers, to have one row per year_month
    tidyr::pivot_wider(names_from = nc_weeks_per_vessl_m,
                # number of vessels
                values_from = occurence_in_month,
                values_fill = 0) %>%
    # sum nc by month to get Total
    mutate(total_nc_vsl_per_month = rowSums(.[2:6])) %>%
    # turn to have num of weeks per month in a row
    tidyr::pivot_longer(-c(year_month, total_nc_vsl_per_month),
                 names_to = "non_compl_weeks",
                 values_to = "non_compl_in_month") %>%
    # count percentage
    mutate(percent_nc = round(
      100 * as.integer(non_compl_in_month) / total_nc_vsl_per_month,
      digits = 2
    )) %>%
    return()
}

perc_plots_by_month <-
  function(my_df, current_year_month) {
    # browser()
    # month_title = current_year_month
    total_nc_vsl_per_month <-
      my_df %>%
      filter(year_month == current_year_month) %>%
      select(total_nc_vsl_per_month) %>%
      unique()

    # month_title = paste0(current_year_month, " Total non-compliant vessels: ", total_nc_vsl_per_month[[1]])
    month_title = paste0(current_year_month, ": ", total_nc_vsl_per_month[[1]], " total nc vsls")

    my_df %>%
      filter(year_month == current_year_month) %>%
      ggplot(aes(non_compl_weeks, percent_nc)) +
      geom_col(fill = "lightblue") +
      geom_text(aes(label = paste0(percent_nc, "%")),
                position = position_dodge(width = 0.9)
                # ,
                # vjust = -0.5
                ) +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 9)
            ) +
      ylim(0, 100) +
      labs(title = month_title,
           # x = "",
           x = "Num of nc weeks",
           y = "") %>%
      # TODO: axes text
      return()
  }

make_year_permit_label <- function(curr_year_permit) {
  curr_year_permit %>%
    stringr::str_replace("_dual", " + dual") %>%
    stringr::str_replace("_", " ") %>%
    toupper() %>%
    return()
}

make_one_plot_compl_vs_non_compl <-
  function(my_df,
           current_title = "",
           is_compliant = "is_compliant",
           percent = "percent",
           no_legend = FALSE,
           percent_label_pos = 0.5,
           default_percent_labels = TRUE,
           geom_text_size = text_sizes[["geom_text_size"]]
           ) {
    # browser()
    one_plot <-
      my_df %>%
      ggplot(aes(x = !!sym(is_compliant),
                 y = !!sym(percent),
                 fill = !!sym(is_compliant))) +
      geom_col() +
      theme(axis.text.x = 
              element_text(size = text_sizes[["axis_text_x_size"]]),
            axis.text.y = 
              element_text(size = text_sizes[["axis_text_y_size"]])) +
      # no x and y titles for individual plots
      labs(title = current_title,
           x = "",
           y = "") +
      scale_fill_manual(
        # use custom colors
        values =
          c(
            "compliant" = "skyblue1",
            "non_compliant" = "#0570B0"
          ),
        # Legend title
        name = "Is compliant?",
        labels = c("Yes", "No")
      ) +
      # manual x axes ticks labels
      scale_x_discrete(labels = c("Yes", "No")) +
      # scale_y_continuous(limits = c(0, 100), labels = scales::percent)
      # Y axes between 0 and 100
      ylim(0, 100)
    # +
    # scale_y_continuous(labels = scales::label_percent(scale = 1))

    label_percent <- purrr::map(my_df$perc_c_nc,
                          ~ paste0(round(.x, 1), "%"))
                   
    # Add percent numbers on the bars
    if (default_percent_labels) {
      one_plot <-
        one_plot +
        geom_text(aes(label =
                        paste0(round(!!sym(
                          percent
                        ), 1), "%")),
                  # in the middle of the bar
                  position =
                    position_stack(vjust = percent_label_pos),
                  size = geom_text_size)
      
    } else {
      one_plot <-
        one_plot + annotate("text",
                            x = 1:2,
                            y = 20,
                            label = label_percent)
    }
    
    
    
    # to use with grid arrange multiple plots
    if (no_legend) {
      one_plot <- one_plot +
        theme(legend.position = "none")
    }
    
    return(one_plot)
  }

# percent buckets
get_p_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_n_compl_rank =
        dplyr::case_when(
          !!sym(field_name) < 25 ~ '0<= & <25%',
          25 <= !!sym(field_name) &
            !!sym(field_name) < 50 ~ '25<= & <50%',
          50 <= !!sym(field_name) &
            !!sym(field_name) < 75 ~ '50<= & <75%',
          75 <= !!sym(field_name) ~ '75<= & <=100%'
        )
    ) %>%
    return()
}

# percent buckets by 50%
get_2_buckets <- function(my_df, field_name) {
  my_df %>%
    dplyr::mutate(
      percent_non_compl_2_buckets =
        dplyr::case_when(
          # nc weeks 
          !!sym(field_name) < 50 ~ '< 50%',
          50 <= !!sym(field_name) ~ '>= 50%'
        )
    ) %>%
    return()
}


#### Current file:  ~/R_code_github/quantify_compliance/get_data.R  ----

# this file is called from quantify_compliance.R

library(tictoc)

project_dir_name <- "FHIER Compliance"

# Download files from FHIER / Reports / FHIER COMPLIANCE REPORT

# get data from csvs ----
get_data_from_FHIER_csvs <- function() {
  filenames = c(
    "FHIERCompliance_10_2023.csv",
    "FHIER_Compliance_2023__05_31_2023.csv"
  )

  ## ---- get csv data into variables ----
  csv_names_list <- prepare_csv_names(filenames)

  # View(csv_names_list)
  # read all csv files
  csv_contents <- load_csv_names(my_paths, csv_names_list)
  # browser()
  # unify headers, trim vesselofficialnumber, just in case
  csvs_clean1 <- clean_all_csvs(csv_contents)
#  str(csvs_clean1)
  # browser()
  compl_clean <- compliance_cleaning(csvs_clean1)

  return(compl_clean)
}

get_compliance_error_definitions <- function() {
  err_desc_filenames = c(file.path(project_dir_name, "Compliance_Error_Types_03_29_2023.csv"))

  err_desc_csv_contents <-
    load_csv_names(my_paths, err_desc_filenames)

  err_desc_clean_headers_csv_content <-
    clean_headers(err_desc_csv_contents[[1]])
  err_desc <-
    change_to_dates(err_desc_clean_headers_csv_content,
                    "last_updated",
                    "%m/%d/%Y %I:%M:%S %p")

  return(err_desc)
}

get_permit_data_from_PIMS_csv <- function() {
  permit_names_list = r"(other\Permits_2023-03-29_1611_active.csv)"

  active_permits_from_pims_raw <-
    load_csv_names(my_paths, permit_names_list)
  # View(active_permits_from_pims[[1]])
  # dplyr::glimpse(active_permits_from_pims_raw[[1]])

  # clean_headers
  active_permits_from_pims_temp1 <-
    active_permits_from_pims_raw[[1]] %>%
    clean_headers()

  # separate columns
  active_permits_from_pims_temp2 <-
    active_permits_from_pims_temp1 %>%
    separate_wider_delim(permit__,
                         "-",
                         names = c("permit_code", "permit_num"),
                         too_many = "merge") %>%
    separate_wider_regex(
      cols = vessel_or_dealer,
      patterns = c(
        vessel_official_number = "[A-Za-z0-9]+",
        " */* ",
        vessel_name = "[A-Za-z0-9]+"
      ),
      too_few = "align_start"
    )

  # correct dates format

  # get a list of field names ends with "_date"
  ends_with_date_fields <-
    grep("_date", names(active_permits_from_pims_temp2), value = TRUE)

  # convert to date
  active_permits_from_pims <-
    change_fields_arr_to_dates(active_permits_from_pims_temp2,
                               ends_with_date_fields,
                               "%m/%d/%Y")

  # test
  active_permits_from_pims %>%
    select(status_date) %>%
    dplyr::arrange(desc(status_date)) %>% unique() %>% head()
  # correct
  # str(active_permits_from_pims)

  return(active_permits_from_pims)
}

get_data_from_csv <- function() {

# uncomment to run
compl_clean <- get_data_from_FHIER_csvs()
# View(compl_clean)
dim(compl_clean)
# 208893     21

## get compliance error definitions from csvs ----
err_desc <- get_compliance_error_definitions()

## get permit data from PIMS csv ----

active_permits_from_pims <- get_permit_data_from_PIMS_csv()

compl_clean1 <- additional_clean_up(compl_clean)

return(compl_clean1)
}

additional_clean_up <- function(compl_clean) {

  # ---- separate SA and GOM permits ----
  compl_clean_sa_vs_gom <-
    separate_permits_into_3_groups(compl_clean)

  # View(compl_clean_sa_vs_gom)

  # ---- add columns for month and quarter ----
  compl_clean_sa_vs_gom_m <-
    compl_clean_sa_vs_gom %>%
    # add month
    mutate(year_month = as.yearmon(week_start)) %>%
    # add quarter
    mutate(year_quarter = as.yearqtr(week_start))

  # ---- convert report numbers to numeric ----
  compl_clean_sa_vs_gom_m_int <-
    compl_clean_sa_vs_gom_m %>%
    mutate(
      captainreports__ = as.integer(captainreports__),
      negativereports__ = as.integer(negativereports__),
      gom_permitteddeclarations__ = as.integer(gom_permitteddeclarations__)
    )

  # add year_permit column ----
  compl_clean_sa_vs_gom_m_int_c <-
    compl_clean_sa_vs_gom_m_int %>%
    mutate(
      year_permit =
        dplyr::case_when(
          year == "2022" & (permit_sa_gom == "gom_only"
                            | permit_sa_gom =="dual") ~
            paste(year, "gom_dual"),
          year == "2022" & permit_sa_gom == "sa_only" ~
            paste(year, "sa_only"),
          year == "2023" & (permit_sa_gom %in% c("sa_only", "dual")) ~
            paste(year, "sa_dual")
        )
    )


  return(compl_clean_sa_vs_gom_m_int_c)
}

# get data from db ----
get_permit_data_from_db <- function() {
  # run once
  con <- connect_to_secpr()

  permit_query <-
    "SELECT DISTINCT
  permit,
  top,
  permit_status,
  vessel_id,
  vessel_alt_num,
  effective_date,
  expiration_date,
  end_date,
  top_name
FROM
  srh.mv_sero_fh_permits_his@secapxdv_dblk.sfsc.noaa.gov
WHERE
  effective_date > TO_DATE('01-JAN-20')
"

  permit_db_data = ROracle::dbGetQuery(con,
                                       permit_query)

  ROracle::dbDisconnect(con)

  return(permit_db_data)
}

get_compl_err_data_from_db <- function() {
  # run once
  con <- connect_to_secpr()

  compl_err_query <-
    "SELECT
  *
FROM
       srh.srfh_vessel_comp_err@secapxdv_dblk.sfsc.noaa.gov
  INNER JOIN srh.srfh_vessel_comp@secapxdv_dblk.sfsc.noaa.gov
  USING ( srh_vessel_comp_id )
WHERE
  comp_year > '2021'
"
# common fields
#   SRH_VESSEL_COMP_ID
# CREATED_DT
# CREATED_USER_ID
# LU_DT
# LU_USER_ID

    compl_err_db_data_0 = ROracle::dbGetQuery(con,
                                       compl_err_query)

    compl_err_db_data_1 <-
      compl_err_db_data_0 %>%
      # remove duplicated columns
      select(-c(CREATED_DT,
                CREATED_USER_ID,
                LU_DT,
                LU_USER_ID))

  ROracle::dbDisconnect(con)

  return(compl_err_db_data_1)
}

get_data_from_db <- function() {

## get permit data from db ----
# to run
permit_db_data <- get_permit_data_from_db()

# str(permit_db_data)
# 37187
# old csv 23888

# get compliance err data from db ----

# uses an inner_join, keeps only entries with compl errors.
# To get all use FULL OUTER JOIN

tic("get_compl_err_data_from_db()")
compl_err_db_data_raw <- get_compl_err_data_from_db()
toc()

# get_compl_err_data_from_db(): 47.5 sec elapsed
# get_compl_err_data_from_db(): 22.23 sec elapsed

# test for unique() fields
all_names_len <- names(compl_err_db_data_raw) %>% length()
uniq_names_len <-
  names(compl_err_db_data_raw) %>% unique() %>% length()
identical(all_names_len, uniq_names_len)

# names(compl_err_db_data_raw) %>%
  # unique() %>%
#   # 42
  # 38
  # length()
# 46
# 38

compl_err_db_data <- clean_headers(compl_err_db_data_raw)
names(compl_err_db_data)

# dim(compl_err_db_data)
# [1] 87925    15
# [1] 44662    38 2021+

# override comments ----
compl_err_db_data_raw %>% select(OVERRIDE_CMT, COMP_OVERRIDE_CMT) %>% unique()
}

if (exists("get_data_from_param")) {
  if (get_data_from_param == "db") {
    get_data_from_db()
  }
} else {
  compl_clean_sa_vs_gom_m_int <- get_data_from_csv()
}


#### Current file:  ~\R_code_github\get_data\get_data_from_fhier\metric_tracking_no_srhs.R  ----

source(file.path(my_paths$git_r,
                 "get_data",
                 "get_data_from_fhier",
                 "get_metrics_tracking.R"))


source(file.path(my_paths$git_r,
                 "get_data",
                 "get_data_from_fhier",
                 "get_srhs_vessels.R"))

## exclude srhs vessels from metric traking ----
fhier_reports_metrics_tracking_not_srhs_ids <-
  purrr::map_df(
    fhier_reports_metrics_tracking_list,
    ~ .x |>
      filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__)
  ) |>
  select(vessel_official_number) |>
  dplyr::distinct()

dim(fhier_reports_metrics_tracking_not_srhs_ids)
# [1] 2981    1
# [1] 4063    1

# browser()
fhier_reports_metrics_tracking_not_srhs_ids_list <-
  purrr::map(
    fhier_reports_metrics_tracking_list,
    ~ .x |>
      filter(!vessel_official_number %in% srhs_vessels_2022_info$uscg__) |>
      select(vessel_official_number) |>
      dplyr::distinct()
  )

# check
map(fhier_reports_metrics_tracking_list, dim)
# [[1]]
# [1] 3634   13
# 
# [[2]]
# [1] 3460   13

map(fhier_reports_metrics_tracking_not_srhs_ids_list, dim)
# [[1]]
# [1] 3571    1
# 
# [[2]]
# [1] 3399    1
plot_file_path <-
  file.path(my_paths$outputs, "quantify_compliance", today())
create_dir_if_not(plot_file_path)

# remove ids not in fhier_reports_metrics_tracking_not_srhs_ids ----
compl_clean_sa_vs_gom_m_int_1 <-
  compl_clean_sa_vs_gom_m_int |>
  filter(
    vessel_official_number %in% fhier_reports_metrics_tracking_not_srhs_ids$vessel_official_number
  )

# remove 2023 gom_only ----
remove_23_gom <- function(my_df) {
  my_df |>
    filter(!(year == "2023" & permit_sa_gom == "gom_only")) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered <-
  # from get_data
  remove_23_gom(compl_clean_sa_vs_gom_m_int_1)

# save vsl count for future checks ----
count_all_vessels <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 4017 vessels
count_all_vessels[1]
# 3776
# [1] "2023-11-01"
# 3790
# today()

count_not_gom23_vessels <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# 3887 vessels
count_not_gom23_vessels[1]
# 3658
# 3669

vessels_compl_or_not_per_y_r_all <-
  compl_clean_sa_vs_gom_m_int_1 %>%
  select(vessel_official_number,
         compliant_,
         year,
         permit_sa_gom) %>%
  unique() %>%
  dplyr::count(compliant_, year, permit_sa_gom)

vessels_compl_or_not_per_y_r_not_gom23 <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  select(vessel_official_number, compliant_, year_permit) %>%
  unique() %>%
  dplyr::count(compliant_, year_permit) %>%
  dplyr::arrange(year_permit, compliant_)
# vessels
#  NO         2022 gom_dual   304
#  YES        2022 gom_dual  1482
#  NO         2022 sa_only   1289
#  YES        2022 sa_only   1617
#  NO         2023 sa_dual   1628
#  YES        2023 sa_dual   2125

# metrics
# vessels_compl_or_not_per_y_r_not_gom23
# 1 NO         2022 gom_dual   290
# 2 YES        2022 gom_dual  1298
# 3 NO         2022 sa_only   1263
# 4 YES        2022 sa_only   1602
# 5 NO         2023 sa_dual   1615
# 6 YES        2023 sa_dual   2111

#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_year.R  ----

# by Year: ----
## year add total ----
# (both compl. and not, a vsl can be in both)

add_total_cnt_in_gr <- function(my_df, group_by_col) {
  my_df %>%
    # group by per year and permit
    dplyr::group_by_at(group_by_col) %>%
    # cnt distinct vessels in each group
    dplyr::mutate(total_vsl_y =
                    dplyr::n_distinct(vessel_official_number)) %>%
    dplyr::ungroup() %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered, "year_permit")

# check
compl_clean_sa_vs_gom_m_int_filtered_tot %>%
  select(year_permit, total_vsl_y) %>%
  unique()
#   year_permit   tota_vsl_m
#   <chr>              <int>
# 1 2022 sa_only        2178
# 2 2022 gom_dual       1495
# 3 2023 sa_dual        2236

# 1 2022 sa_only         2145
# 2 2022 gom_dual        1304
# 3 2023 sa_dual         2220

## expired or not? ----
end_of_2022 <- as.Date("12/31/2022", format = "%m/%d/%Y")

expired_or_not <- function(my_df) {
  my_df %>%
    # get difference in days
    dplyr::mutate(exp_w_end_diff_y =
                    as.numeric(as.Date(permitgroupexpiration) -
                                 end_of_2022)) %>%
    # create a column
    dplyr::mutate(
      perm_exp_y =
        dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                         exp_w_end_diff_y > 0 ~ "active")
    ) %>%
    return()
}

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_tot)

## count expiration by year, permit ----
count_expiration_by <- function(my_df, group_by_var) {
  my_df %>%
    dplyr::group_by_at(group_by_var) %>%
    # count distinct vessels per group
    dplyr::mutate(exp_y_tot_cnt = n_distinct(vessel_official_number)) %>%
    return()
}

group_by_var <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y,
                      group_by_var)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

## get compl_counts ----
### get compl, no compl, or both per year ----

get_compl_by <- function(my_df, group_by_for_compl) {
  my_df %>%
    dplyr::group_by_at(group_by_for_compl) %>%
    # can unique, because we are looking at vessels, not weeks
    unique() %>%
    # more columns, a column per vessel
    tidyr::pivot_wider(
      names_from = vessel_official_number,
      values_from = compliant_,
      # make it "NO_YES" if both
      values_fn = ~ paste0(sort(.x), collapse = "_")
    ) %>%
    dplyr::ungroup() %>%
    return()
}

group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt_short,
               group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide)
# [1]    6 3662

### count compl, no compl, or both per year, permit, active status ----

count_by_cols <- function(my_df,
                          cols_names) {
  my_df %>%
    # turn back to a longer format, vessel ids in one column
    tidyr::pivot_longer(
      # all other columns are vessel ids, use them as names
      cols = !any_of(cols_names),
      values_to = "is_compl_or_both",
      names_to = "vessel_official_number"
    ) %>%
    return()
}

cols_names <-
  c("year_permit", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide,
                cols_names)

### get cnts for compl, no compl, or both per month with exp ----
cnts_for_compl <-
  function(my_df, group_by_cols, cols_to_cnt) {
    my_df %>%
      dplyr::group_by_at(group_by_cols) %>%
      unique() %>%
      # exclude vessel id
      dplyr::select(-vessel_official_number) %>%
      # count grouped by onther columns
      dplyr::add_count(!!!syms(cols_to_cnt),
                       name = "compl_or_not_cnt") %>%
      unique() %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols <- c("year_permit", "perm_exp_y")
cols_to_cnt <- c("year_permit", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] 22  6

#### check counts ----
# print_df_names(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt)
# [1] "year_permit, total_vsl_y, perm_exp_y, exp_y_tot_cnt, is_compl_or_both, compl_or_not_cnt"

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
  # remove NAs
  dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
  dplyr::select(year_permit, total_vsl_y, compl_or_not_cnt, is_compl_or_both) %>%
  dplyr::group_by(year_permit) %>%
  # get sums
  dplyr::mutate(sum_cnts = sum(compl_or_not_cnt)) %>%
  dplyr::filter(!total_vsl_y == sum_cnts) %>%
  dim()
# 0 OK

### One vessel in 2 groups ----
# The number should be the same as the total number we got earlier. It is not, which means One vessel is in 2 perm_exp_y groups, has both expired and not expired permit in 2022.

### check if a vessel is compliant and not at the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  dim()
# 0 - OK

# check if a vessel permit is expired and not in the same time
compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dim()
# 0

### check total_vsl_y vs. sum_cnts (should be equal, see dbl FL7825PU above) ----
compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1617 YES
# 2       1289 NO

# 1       1602 YES
# 2       1263 NO

# 1       1666 YES       
# 2       1163 NO        


compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  # dplyr::group_by(compliant_, perm_exp_y) %>%
  # dplyr::group_by(compliant_) %>%
  dplyr::group_by(perm_exp_y) %>%
  # 1707 + 472
  # [1] 2179
  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1442 YES        active
# 2        887 NO         active
# 3        402 NO         expired
# 4        175 YES        expired
# YES: 1442+175
# [1] 1617
# NO: 887+402
# [1] 1289
# 1617+1289
# 2906

# today()
# [1] "2023-06-19"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1707 YES        active
# 2       1707 NO         active
# 3        472 NO         expired
# 4        472 YES        expired
# 1707 + 472
# 2179

# [1] "2023-08-26"
#   tota_vsl_m compliant_ perm_exp_y
#        <int> <chr>      <chr>
# 1       1694 YES        active
# 2       1694 NO         active
# 3        451 NO         expired
# 4        451 YES        expired
# 1694+451 = 2145

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration

add_total_cnts <-
  function(my_df, group_by_compl_cols, group_by_exp_cols) {
    my_df %>%
      # remove NAs
      dplyr::filter(stats::complete.cases(is_compl_or_both)) %>%
      dplyr::mutate(
        compl_or_not =
          dplyr::case_when(is_compl_or_both == "YES" ~
                             "compliant",
                           .default = "non_compliant")
      ) %>%
      dplyr::group_by_at(group_by_compl_cols) %>%
      # add counts by compliant
      dplyr::mutate(cnt_y_p_c = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      # add counts by permit expiration
      dplyr::group_by_at(group_by_exp_cols) %>%
      dplyr::mutate(cnt_y_p_e = sum(compl_or_not_cnt)) %>%
      dplyr::ungroup() %>%
      return()
  }

group_by_cols1 <- c("year_permit", "compl_or_not")
group_by_cols2 <- c("year_permit", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt,
    group_by_cols1,
    group_by_cols2
  )

# check cnts
# compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt %>%
#   # remove NAs
#   filter(year_permit == '2022 gom_dual' & perm_exp_y == 'expired') %>% View()

## add percents of total ----
add_percents_of_total <-
  function(my_df, select_cols) {
    my_df %>%
      dplyr::select(all_of(select_cols)) %>%
      unique() %>%
      dplyr::mutate(perc_c_nc = cnt_y_p_c * 100 / total_vsl_y) %>%
      return()
  }

select_cols <- c(
  "year_permit",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc)
# [1] 11  7

## plots (red/green) for compl vs. non compl vessels per year ----

title_permits <- data.frame(
  # title = c("SA Only", "GOM + Dual", "2023: SA + Dual"),
  title = c("2022: SA Only", "2022: GOM + Dual", "2023: SA + Dual"),
  year_permit = c("2022 sa_only",
                  "2022 gom_dual",
                  "2023 sa_dual"),
  second_part = c("Permitted Vessels",
                  "Permitted Vessels",
                  "Permitted Vessels")
)

# "Permitted SEFHIER Vessels"

gg_all_c_vs_nc_plots <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_permit)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # 1st figure title: "SA Only Permitted Vessels (Total Permitted: 2178; Expired Permits: 472)"
    # 2nd figure title: "GOM + Dual Permitted Vessels (Total Permitted: 1495; Expired Permits: 303)"

    curr_title_permit <-
      title_permits %>%
      filter(year_permit == curr_year_permit)

    current_title <-
      paste(curr_title_permit$title,
             curr_title_permit$second_part)

    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc",
                                       geom_text_size = 7)

    return(one_plot)

  })


# repeat with no legend
gg_all_c_vs_nc_plots_no_legend <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc$year_permit %>%
  unique() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_short_wide_long_cnt_tot_y_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    # See function definition F2
    y_r_title <-
      make_year_permit_label(curr_year_permit)

    total_vsls <- unique(curr_df$total_vsl_y)

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      unique()

    # 1st figure title: "SA Only Permitted Vessels (Total Permitted: 2178; Expired Permits: 472)"
    # 2nd figure title: "GOM + Dual Permitted Vessels (Total Permitted: 1495; Expired Permits: 303)"

    curr_title_permit <-
      title_permits %>%
      filter(year_permit == curr_year_permit)

    current_title <-
      paste(curr_title_permit$title,
             curr_title_permit$second_part)

    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      unique() %>%
      # See function definition F2
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc",
                                       geom_text_size = 7,
                                       no_legend = T)

    return(one_plot)

  })

# 2023 plot
# gg_all_c_vs_nc_plots[[3]]

# 2022
# sa
gg_all_c_vs_nc_plots[[1]]

# gom
gg_all_c_vs_nc_plots_no_legend[[2]]

main_title <- "Percent Compliant vs. Noncompliant SEFHIER Vessels"

is_compliant_legend <- legend_for_grid_arrange(gg_all_c_vs_nc_plots[[1]])

# combine plots for 2022
grid.arrange(gg_all_c_vs_nc_plots_no_legend[[2]],
             gg_all_c_vs_nc_plots_no_legend[[1]],
             right = is_compliant_legend,
             nrow = 1)

# sa
grid.arrange(gg_all_c_vs_nc_plots[[1]],
             top = main_title)

# gom
grid.arrange(gg_all_c_vs_nc_plots[[2]],
             top = main_title)

# 2023 sa+dual
grid.arrange(gg_all_c_vs_nc_plots[[3]],
             top = main_title)

# Non compliant only ----

# start with the new data with expiration by year
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
weeks_per_vsl_permit_year_compl_cnt <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_exp_y_cnt %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   compliant_,
                   name = "weeks_per_vessel_per_compl") %>%
  dplyr::add_count(year_permit,
                   vessel_official_number,
                   name = "total_weeks_per_vessel") %>%
  dplyr::ungroup()

## test 1a ----
weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(vessel_official_number == "1000042" &
                  year == "2022") %>%
  dplyr::select(year,
                compliant_,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()
#   year  compliant_ weeks_per_vessel_per_compl total_weeks_per_vessel
# 1 2022 YES 50 52
# 2 2022 NO 2 52

nc_2022_sa_only_test <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(year_permit == "2022 sa_only",
                compliant_ == "NO") %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique()

head(nc_2022_sa_only_test)
# weeks_per_vessel_per…¹ total_weeks_per_vessel
# 1 VA9236AV 52 52
# 2 VA6784AD 24 24
# 3 VA4480ZY 44 44
# 4 SC9207BX 26 50
# 5 SC8907DF 14 40
# 6 SC8298DH 45 45

weeks_per_vsl_permit_year_compl_cnt %>%
  dplyr::filter(
    year_permit == "2022 sa_only",
    compliant_ == "YES",
    vessel_official_number == "SC8907DF"
  ) %>%
  dplyr::select(vessel_official_number,
                weeks_per_vessel_per_compl,
                total_weeks_per_vessel) %>%
  unique() %>%
  dplyr::glimpse()
# 26  40

## 1b) percent of compl/non-compl per total weeks each vsl was present ----
count_weeks_per_vsl_permit_year_compl_p <-
  weeks_per_vsl_permit_year_compl_cnt %>%
  mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(count_weeks_per_vsl_permit_year_compl_p)
# [1] 185251     32

# test
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only", year == "2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# [1] 2178
# 2145

count_weeks_per_vsl_permit_year_compl_p %>%
  filter(permit_sa_gom == "sa_only",
         year == "2022",
         compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  # unique() %>%
  # 1289    Non compliant vsl
  dim()
# [1] 26466 non compliant weeks
# [1] 25662     1

### test 1b ----
count_weeks_per_vsl_permit_year_compl_p %>%
  filter(vessel_official_number == "1020822",
         year == "2022") %>%
  select(
    year,
    permit_sa_gom,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ compliant_                 <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl <int> 33, 19
# $ total_weeks_per_vessel     <int> 52, 52
# $ percent_compl              <dbl> 63.46154, 36.53846

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

count_weeks_per_vsl_permit_year_n_compl_p_short <-
  count_weeks_per_vsl_permit_year_compl_p %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_permit,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

# str(count_weeks_per_vsl_permit_year_n_compl_p_short)
# tibble [3,221 × 7] (S3: tbl_df/tbl/data.frame)
# $ weeks_per_vessel_per_compl: int [1:3221] 52 24 44 26 14 45 5 41 52 27 ...
# $ total_weeks_per_vessel    : int [1:3221] 52 24 44 50 40 45 41 45 52 52 ...
# $ percent_compl             : num [1:3221] 100 100 100 52 35 ...

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts <-
  get_p_buckets(count_weeks_per_vsl_permit_year_n_compl_p_short,
                "percent_compl")

### test 2 ----
# count in one bucket
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::count(percent_compl, year_permit,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 634
# 615

# 3) count how many in each bucket ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts %>%
  dplyr::add_count(year_permit,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

### test 3 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(year_permit,
                percent_n_compl_rank,
                cnt_v_in_bucket) %>%
  unique() %>%
  dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_y_r") %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  str()
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 399 172 85 633
# $ total_per_y_r       : int [1:4] 1289 1289 1289 1289

# $ year_permit         : chr [1:4] "2022 sa_only" "2022 sa_only" "2022 sa_only" "2022 sa_only"
# $ percent_n_compl_rank: chr [1:4] "0<= & <25%" "25<= & <50%" "50<= & <75%" "75<= & <=100%"
# $ cnt_v_in_bucket     : int [1:4] 398 168 82 615
# $ total_per_y_r       : int [1:4] 1263 1263 1263 1263

# "2022 sa_only"
# 633+85+172+399
# [1] 1289

# $ cnt_v_in_bucket     : int [1:4] 399 171 85 634
# 399 + 171 + 85 + 634
# 1289
# correct

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b %>%
  # cnt vessels per year, permit and compliance
  dplyr::add_count(year_permit,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
  dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
#   percent_n_compl_rank perc_vsls_per_y_r_b
#   <chr>                              <dbl>
# 1 0<= & <25%                         31.0
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.59
# 4 75<= & <=100%                      49.1
# 1 0<= & <25%                         31.5
# 2 25<= & <50%                        13.3
# 3 50<= & <75%                         6.49
# 4 75<= & <=100%                      48.7

# 5) blue plots by year ----

# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# print_df_names(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc)

# "2022: % Non-Compliant GOM + Dual Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this]  "(Total Non-Compliant = 304 Vessels; Active Permits = 1192 Vessels)"
# "2022: % Non-Compliant SA Only Permitted Vessels Missing >25%, <=25-49.9%, <=50-74.9%, <75% of their reports"
# [subtitle this] "(Total Non-Compliant = 1289 Vessels; Active Permits = 1707 Vessels)"
# For plot 4:
# "2023: SA + Dual Permitted SEFHIER Vessels (Total Permitted: 2235 ; Total Noncompliant: 1628; Expired Permits: 1)"

blue_year_plot_titles <-
  data.frame(
    year_permit = c("2022 sa_only",
                    "2022 gom_dual",
                    "2023 sa_dual"),
    first_part = c(
      "SA Only Permitted Vessels\n(",
      "GOM + Dual Permitted Vessels\n(",
      "2023: SA + Dual Permitted SEFHIER Vessels\n(Total Permitted = 2235 Vessels; "
    )
  )

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc <-
  count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc$year_permit %>%
  unique() %>%
  sort() %>%
  # repeat for each year_permit
  purrr::map(function(curr_year_permit) {
    # browser()
    curr_df <-
      count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_permit == curr_year_permit)

    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # See the function definition F2
    curr_title_y_p <- make_year_permit_label(curr_year_permit)

    curr_blue_year_plot_title <-
      blue_year_plot_titles %>%
      filter(year_permit == curr_year_permit)

    y_p_title <-
      paste0(
        curr_blue_year_plot_title$first_part,
        "Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        # " Vessels; Acitve permits = ",
        # active_permits$exp_y_tot_cnt,
        # "; Expired permits: ",
        # expired_permits$exp_y_tot_cnt,
        " Vessels)"
      )

    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "% nc vsls per year & permit") +
      # text on bars
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5),
                size = text_sizes[["geom_text_size"]]
) +
      # y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 12))

    return(one_plot)
  })

gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[2]]

## plot 2022 ----
ndash <- "\u2013"
super_title <- paste0(
  "2022: % Non-Compliant Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)

# footnote = textGrob(
#   "X axes is % of missing reports for non-compliant vessels",
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -1.5,
#   just = "right",
#   x = 0.01, y = 0.99,
#   vjust = 1
# )

### common y axes ----
yleft <- textGrob("% per permit region",
                  # rotate
                  rot = 90,
                  gp = gpar(fontsize = 10))

p <-
  list(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[1:2])[[1]] %>%
  # remove individual x and y labels for each plot
  purrr::map(~ .x + labs(x = NULL, y = NULL))

plot_perc_22 <- gridExtra::grid.arrange(
  grobs = p,
  left = yleft,
  top = super_title)

## SA23 ----

super_title <- "% of non-compliant vessels (2023)"


gridExtra::grid.arrange(gg_count_weeks_per_vsl_permit_year_compl_p_short_cuts_cnt_in_b_tot_perc[[3]],
                        top = super_title
                        )
                        # ,
                        # bottom = footnote


#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_month.R  ----

# Per month, region ----
# super_title_per_m = "% non-compliant weeks per month for non-compliant vessels by permit type (2022)"

# by Month: ----
## add tot cnts per month, permit ----

compl_clean_sa_vs_gom_m_int_filtered_tot_m <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  dplyr::group_by(year_permit, year_month) %>%
  # count distinct vessels per group
  dplyr::mutate(total_vsl_m = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

# View(compl_clean_sa_vs_gom_m_int_filtered)

### test tot month ----
compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  dplyr::filter(year == "2022") %>%
  dplyr::select(year_permit, year_month, total_vsl_m) %>%
  dplyr::arrange(year_month, year_permit) %>%
  unique() %>%
  tail()
# numbers are as before, ok
# 1 2022 gom_dual Oct 2022          1144
# 2 2022 sa_only  Oct 2022          1695
# 3 2022 gom_dual Nov 2022          1138
# 4 2022 sa_only  Nov 2022          1656
# 5 2022 gom_dual Dec 2022          1123
# 6 2022 sa_only  Dec 2022          1647


## add the difference between expiration and week_start----

# If we use a week_end, than a vessel which ends near the end of year will have its last week expired.
compl_clean_sa_vs_gom_m_int_c_exp_diff <-
  compl_clean_sa_vs_gom_m_int_filtered_tot_m %>%
  # add a column with difference in days
  dplyr::mutate(exp_w_end_diff =
                  as.numeric(as.Date(permitgroupexpiration) - week_start + 1))

## expired or not? ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff %>%
  # add a column
  dplyr::mutate(perm_exp_m =
                  dplyr::case_when(exp_w_end_diff < 0 ~ "expired",
                            exp_w_end_diff >= 0 ~ "active"))

## Keep active only ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
  filter(perm_exp_m == "active")

dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d)
# [1] 185251     28

dim(compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp)
# [1] 185199     28

## expired: count if vessel is expired or not by year, permit and month  ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_not_exp %>%
  dplyr::group_by(year_permit, year_month, perm_exp_m) %>%
  # add a column counting distinct vessels per group
  dplyr::mutate(exp_m_tot_cnt = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

# check
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  dplyr::filter(year == "2022") %>%
  dplyr::select(year_permit,
                year_month,
                perm_exp_m,
                exp_m_tot_cnt,
                total_vsl_m) %>%
  unique() %>%
  dplyr::arrange(year_permit, year_month) %>%
  tail() |>
  dplyr::glimpse()
# year_permit  year_month perm_exp_m exp_m_tot_cnt total_vsl_m
#   <chr>        <yearmon>  <chr>              <int>       <int>
# 1 2022 sa_only Oct 2022   active              1721        1722
# 2 2022 sa_only Oct 2022   expired                1        1722
# 3 2022 sa_only Nov 2022   active              1676        1677
# 4 2022 sa_only Nov 2022   expired                1        1677
# 5 2022 sa_only Dec 2022   active              1656        1657
# 6 2022 sa_only Dec 2022   expired                1        1657

# compare with the text for tot month above
# rm exp
# $ year_permit   <chr> "2022 sa_only", "2022 sa_only", "2022 sa_only", "2022 sa_only", "…
# $ year_month    <yearmon> Jul 2022, Aug 2022, Sep 2022, Oct 2022, Nov 2022, Dec 2022
# $ perm_exp_m    <chr> "active", "active", "active", "active", "active", "active"
# $ exp_m_tot_cnt <int> 1745, 1755, 1708, 1694, 1655, 1646
# $ total_vsl_m   <int> 1746, 1756, 1709, 1695, 1656, 1647

# from now on use exp_m_tot_cnt instead of total_vsl_m

#### how many are expired ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
  filter(perm_exp_m == "expired") |>
  select(perm_exp_m, exp_m_tot_cnt) |>
  dplyr::distinct()
# 1 expired                1
# 0

compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt |>
  # filter(perm_exp_m == "expired" &
  #          !year_month == "Dec 2022") |>
  # dplyr::glimpse()
  filter(vessel_official_number == "1000164" &
           year_month == "Nov 2022") |>
  dim()
# 0

#### check if expired and active permit is in the same month
# compl_clean_sa_vs_gom_m_int_c_exp_diff_d |>
#   dplyr::group_by(vessel_official_number, year_month) |>
#   mutate(active_or_expired = paste(sort(unique(perm_exp_m)),
#                                    collapse = " & ")) |>
#   filter(grepl("&", active_or_expired)) |>
#   dim()
  # 0

## cnt distinct total vessels per year, permit, month, compl ----
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt %>%
  dplyr::group_by(year_permit, year_month, compliant_) %>%
  # add a column
  dplyr::mutate(cnt_vsl_m_compl = n_distinct(vessel_official_number)) %>%
  dplyr::ungroup()

### test tot cnts per month ----
# tic("test tot cnts per month")
compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  dplyr::select(
    year_permit,
    year_month,
    perm_exp_m,
    exp_m_tot_cnt,
    total_vsl_m,
    compliant_,
    cnt_vsl_m_compl
  ) %>%
  unique() %>%
  dplyr::filter(year_month == "Jan 2022") %>%
  dplyr::glimpse()
# toc()
# $ year_month      <yearmon> Jan 2022, Jan 2022, Jan 2022, Jan 2022
# $ perm_exp_m      <chr> "active", "active", "active", "active"
# $ exp_m_tot_cnt   <int> 1635, 1635, 1192, 1192
# $ total_vsl_m     <int> 1635, 1635, 1192, 1192
# $ compliant_      <chr> "YES", "NO", "YES", "NO"
# $ cnt_vsl_m_compl <int> 1057, 703, 1173, 45
# 1057 + 703 = 1760 is more than total. Some vessels can be both in a month, if compliance differs by week. For this analysis I used vessels having at least one week in the month  non-compliant.
# If we are going to use "yes only" than redo "yes, no, no_yes" division as for a year above.
# $ cnt_vsl_m_compl <int> 1052, 688, 1004, 42

## add counts of weeks per vessel by month, compl ----
count_weeks_per_vsl_permit_year_compl_month <-
  compl_clean_sa_vs_gom_m_int_c_exp_diff_d_cnt_cnt_compl %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            compliant_,
            name = "weeks_per_vessel_per_compl_m") %>%
  ungroup %>%
  dplyr::add_count(year_permit,
            year_month,
            vessel_official_number,
            name = "total_weeks_per_vessel_per_compl_m")

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  # select(year_permit, year_month, perm_exp_m, exp_m_tot_cnt, total_vsl_m, compliant_, cnt_vsl_m_compl) %>%
  # unique() %>%
  filter(year_month == "Dec 2022") %>%
  dplyr::glimpse()
# Rows: 11,031
# $ compliant_                         <chr> "YES", "NO", "YES", "YES",…
# $ total_vsl_m                        <int> 1657, 1657, 1657, 1657, 16…
# $ perm_exp_m                         <chr> "active", "active", "activ…
# $ exp_m_tot_cnt                      <int> 1656, 1656, 1656, 1656, 16…
# $ cnt_vsl_m_compl                    <int> 1282, 434, 1282, 1282, 434…
# $ weeks_per_vessel_per_compl_m       <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 4, 4, 4,…

# test
count_weeks_per_vsl_permit_year_compl_month %>%
  filter(year_permit == "2022 sa_only" &
           compliant_ == "NO") %>%
  select(vessel_official_number,
         compliant_,
         year_month,
         weeks_per_vessel_per_compl_m) %>%
  unique() %>%
  dplyr::glimpse()
# $ vessel_official_number       <chr> "VA9236AV", "VA6784AD", "VA4480…
# $ compliant_                   <chr> "NO", "NO", "NO", "NO", "NO", "…
# $ year_month                   <yearmon> Dec 2022, Dec 2022, Dec 202…
# $ weeks_per_vessel_per_compl_m <int> 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 4…

## 1) Month: percent compl weeks per vsl per month ----

count_weeks_per_vsl_permit_year_compl_m_p <-
  count_weeks_per_vsl_permit_year_compl_month %>%
  dplyr::mutate(percent_compl_m =
                  weeks_per_vessel_per_compl_m * 100 / total_weeks_per_vessel_per_compl_m)

### test 1, by month ----
count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(year_month == "Dec 2022") %>%
  filter(vessel_official_number == "NJ8126HN") %>%
  select(
    vessel_official_number,
    year_month,
    compliant_,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m
  ) %>%
  unique() %>%
  dplyr::arrange(year_month) %>%
  dplyr::glimpse()
# $ compliant_                         <chr> "YES", "NO"
# $ weeks_per_vessel_per_compl_m       <int> 1, 3
# $ total_weeks_per_vessel_per_compl_m <int> 4, 4
# $ percent_compl_m                    <dbl> 25, 75

## 2a) Month: Only non-compl and fewer cols ----
# View(count_weeks_per_vsl_permit_year_compl_m_p)
count_weeks_per_vsl_permit_year_compl_m_p_nc <-
  count_weeks_per_vsl_permit_year_compl_m_p %>%
  filter(compliant_ == "NO") %>%
  select(
    year_permit,
    year_month,
    vessel_official_number,
    perm_exp_m,
    exp_m_tot_cnt,
    cnt_vsl_m_compl,
    # total_vsl_m,
    weeks_per_vessel_per_compl_m,
    total_weeks_per_vessel_per_compl_m,
    percent_compl_m,
    compliant_
  ) %>%
  unique()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc)
## 2b) Month: get percentage "buckets" ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b <-
  # Use F2 to see the function definition
  get_p_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)

### check 2, by month ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  filter(percent_n_compl_rank == "75<= & <=100%") %>%
  filter(year_permit == "2022 sa_only" &
           vessel_official_number == "VA9236AV") %>%
  dplyr::add_count(percent_compl_m, year_permit,
                   name = "amount_of_occurences") %>%
  # sort in the descending order
  dplyr::arrange(desc(percent_compl_m)) %>%
  # sum
  dplyr::add_count(wt = amount_of_occurences) %>%
  dplyr::glimpse()
# $ amount_of_occurences         <int> 12, 12, 12, 12, 12, 12, 12, 12…
# $ n                            <int> 144, 144, 144, 144, 144, 144, …

### add 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 <-
  # Use F2 to see the function definition
  get_2_buckets(count_weeks_per_vsl_permit_year_compl_m_p_nc,
                "percent_compl_m")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b2)

## 3) Month: count how many in each bucket ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_n_compl_rank,
                   name = "cnt_v_in_bucket")

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b)
# [1] 11489    12
# [1] 11477    12

### 2 buckets ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b2 %>%
  dplyr::add_count(year_permit,
                   year_month,
                   percent_non_compl_2_buckets,
                   name = "cnt_v_in_bucket2")

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
# check by counting in a different way
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  dplyr::group_by(year_permit,
                  year_month,
                  percent_n_compl_rank) %>%
  dplyr::mutate(cnt_v_in_bucket1 =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::filter(!(cnt_v_in_bucket == cnt_v_in_bucket1)) %>%
  dplyr::ungroup() |> 
  dim()
# 0 - correct, no difference

### tests 3, by month ----
# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_tot)

test_compare_with <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  filter(compliant_ == "NO") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
# total 703 nc vsls in "Jan 2022 sa_only"
# tot 1635 in Jan 2022
#
# 45 nc vsls in "Jan 2022 gom_dual"
# 45 * 100 / 1192 = 3.8%

# 688

# still true?
test_res <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Jan 2022") %>%
  select(vessel_official_number) %>%
  unique() %>%
  dim()
#  703 1

test_compare_with[1] == test_res[1]
# TRUE

## 4) Month: cnt percents of (3) ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### 2 buckets ----
# print_df_names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2)
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2 %>%
  # percent vessels per year, region, bucket
  dplyr::mutate(perc_vsls_per_m_b2 = cnt_v_in_bucket2 * 100 / cnt_vsl_m_compl) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_m_b2, 0), "%"))

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p)

### test 4, by month ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  filter(year_permit == "2022 sa_only") %>%
  filter(year_month == "Dec 2022") %>%
  select(percent_n_compl_rank, perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()
# Dec 2022
# 1 25<= & <50%                         2.30
# 2 50<= & <75%                         4.15
# 3 75<= & <=100%                      93.5

# Jan 2022
#   percent_n_compl_rank perc_vsls_per_y_r_b
# 1 0<= & <25%                          4.69
# 2 25<= & <50%                         4.13
# 3 50<= & <75%                         4.13
# 4 75<= & <=100%                      87.1

# 612*100/703 == 87.05548

## 5) Month plots ----

## 5a) prepare the df for plotting ----
### keep only fields needed to plot ----

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p %>%
  select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  dplyr::distinct()

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short <-
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p |> 
    select(
    -c(
      vessel_official_number,
      weeks_per_vessel_per_compl_m,
      total_weeks_per_vessel_per_compl_m,
      percent_compl_m
    )
  ) %>%
  # can unique, because all counts by vessel are done already
  dplyr::distinct()

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)

### add column with Month name only (for plotting) ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short2 <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short %>%
  # remove a space and following digits
  dplyr::mutate(month_only = str_replace(year_month, " \\d+", ""))

# check
dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p)
# [1] 11766    15
# [1] 11489    15
# [1] 11477    15
# [1] 10732    14

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)
# [1] 107  12
# [1] 95 12

dim(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short)
# [1] 58 10

# View(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short)

### split the df by year_permit into a list ----
count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short$year_permit))

count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2 <-
  split(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short,
        as.factor(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b2_p_short$year_permit))

### get used year_permits ----
sorted_year_permits <- names(count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r) %>%
  sort()
# [1] "2022 gom_dual" "2022 sa_only"  "2023 sa_dual"

### make titles ----
get_year_permit_titles <- function(permit, year) {
  paste0("The Number of Non-Compliant Vessels Each Month That Were Compliant More Than 50% of a Month in ", year) %>%
    # "% of non-compliant ",
    #      permit,
    #      " Permitted vessels by month",
    #      " (",
    #      year,
    #      ")") %>%
    return()
}

year_permit_titles <-
  data.frame(
    super_title_gom = get_year_permit_titles("Gulf + Dual", "2022"),
    super_title_sa = get_year_permit_titles("South Atlantic Only", "2022"),
    super_title_2023 = get_year_permit_titles("South Atlantic + Dual", "2023")
  )

names(year_permit_titles) <- sorted_year_permits

### additional functions for Month plots ----
# TODO: simplify
# returns 0 or number of expired permits
get_expired_permit_numbers <- function(curr_data) {
  # browser()

  exp_filt <- curr_data %>%
    filter(perm_exp_m == "expired") %>%
    unique()

  res <- exp_filt$exp_m_tot_cnt

  # if filter(perm_exp_m == "expired") returned nothing
  if (dim(exp_filt)[1] == 0) {
    res <- 0
  }

  return(res)
}

get_one_plot_by_month <-
  function(my_df, curr_year_month) {
    # browser()
    curr_data <- my_df %>%
      filter(year_month == curr_year_month)

    curr_month_name <- unique(curr_data$month_only)

    curr_year_permit <- unique(curr_data$year_permit)

    curr_tot_v_per_m_y_r <- unique(curr_data$cnt_vsl_m_compl)

    curr_m_tot_active <- curr_data %>%
      filter(perm_exp_m == "active") %>%
      select(exp_m_tot_cnt) %>%
      unique()

    # see function definition F2
    cnt_expired <- get_expired_permit_numbers(curr_data)

    # curr_title <- paste0(
    #   curr_month_name,
    #   " (",
    #   curr_tot_v_per_m_y_r,
    #   " vsls; ",
    #   curr_m_tot_active$exp_m_tot_cnt,
    #   " act. p.; ",
    #   cnt_expired,
    #   " exp. p.)"
    # )

    # curr_title <- curr_month_name

    curr_title <- paste0(
      curr_month_name,
      " (",
      curr_tot_v_per_m_y_r,
      " total non-compliant vsls)"
      )
    one_plot <-
      ggplot(curr_data,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "skyblue") +
      labs(title = curr_title,
           # no labels for axes
           x = "",
           y = "") +
      # text on each bar
      geom_text(aes(label = perc_labels),
                # posintion - middle
                position = position_stack(vjust = 0.5)) +
      # Y axes 0 to 100
      ylim(0, 100) +
      # size of an individual plot's title
      theme(plot.title =
              element_text(size = 10))

    return(one_plot)
  }

gg_month_nc_perc <-
  sorted_year_permits %>%
  purrr::map(
    # for each year and permit pull a df from the list
    function(current_year_permit) {
      # browser()
      curr_df <-
        count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r[[current_year_permit]]

      curr_year_months <-
        curr_df %>%
        dplyr::select(year_month) %>%
        unique() %>%
        as.data.frame()

      list_of_plots <-
        curr_year_months$year_month %>%
        sort() %>%
        # run the function for each month
        # see the function definition F2
        purrr::map(~ get_one_plot_by_month(curr_df,
                                            curr_year_month = .))
      # add correct names instead of 1, 2...
      names(list_of_plots) <-
        sort(curr_year_months$year_month)

      # put the name and the plots into a list to return
      res <- list(current_year_permit, list_of_plots)
      return(res)
    })

footnote_text <- "In parenthesis are 1) # of non compliant vessels per month; 2) total active permits per month; 3) total expired permits per month;"

# footnote <- textGrob(
#   footnote_text,
#   gp = gpar(fontface = 3, fontsize = 10),
#   # justify left
#   # hjust = 0,
#   hjust = -0.5,
#   # just = "right",
#   x = 0.01,
#   y = 0.99,
#   vjust = 1
# )

### common axes for Months ----
y_left <- textGrob("% per 'bucket'",
                   # angle
                   rot = 90,
                   gp = gpar(fontsize = 10))

x_bottom <-
  textGrob("'buckets' - distibution of % of non compliant weeks per vessel",
           gp = gpar(fontsize = 10))

all_plots_w_titles_list <-
  gg_month_nc_perc %>%
  # repeat for each entry
  purrr::map(function(curr_year_reg_list) {
    # browser()
    # get a name
    curr_year_permit <- curr_year_reg_list[[1]]

    # get a title by the name
    curr_super_title <- year_permit_titles[[curr_year_permit]]

    # add a subtitle
    whole_title <- curr_super_title
    # whole_title <-
    #   paste0(curr_super_title,
    #          # new line
    #          "\n",
    #          footnote_text)

    all_plots_per_year_region <-
      gridExtra::arrangeGrob(
        grobs =
          curr_year_reg_list[[2]],
        top = whole_title,
        left = y_left,
        bottom = x_bottom,
        ncol = 3
      )

    # combine the current year_permit and the plots in a list
    res <- list(curr_year_permit,
                all_plots_per_year_region)

    return(res)
  })

# warnings()

# draw one plot to test
gridExtra::grid.arrange(all_plots_w_titles_list[[2]][[2]])

# View(all_plots_w_titles_list)

## all plots per month to files ----
# saves to PNG, PDF etc. depending on an extension in "file_full_name"
save_plots_list_to_files <-
  function(file_full_name,
           plots_list) {
    ggplot2::ggsave(
      file_full_name,
      plots_list,
      width = 30,
      height = 20,
      units = "cm"
    )
  }

# add dir
plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

all_plots_w_titles_list %>%
  # repeat for each element of the list
  purrr::map(function(curr_plot_list) {
    file_name_base <- paste0(curr_plot_list[[1]],
                             "_percent_distribution_per_month",
                             ".png")

    # file.path adds the correct concatenation
    file_full_name <- file.path(plot_file_path_m,
                                file_name_base)

    # see the function definition F2
    save_plots_list_to_files(file_full_name,
                             # plots
                             curr_plot_list[[2]])
  })


#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_line_plots.R  ----

## Month, line plots with dots ----
line_df_22_gom <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 gom_dual`

line_df_22_sa <-
  count_weeks_per_vsl_permit_year_compl_m_p_nc_b_cnt_in_b_p_short_y_r2$`2022 sa_only`

# Create a character vector named 'percent_names'.

# Use the 'paste0' function to concatenate elements together.
percent_names <- paste0(
  # Generate a sequence from 0 to 100 with a step of 10, and add "%" to each element.
  seq(0, 100, by = 10), "%"
)

dim(line_df_22_gom)
# [1] 24 10

# (re)define sizes
geom_text_size = text_sizes[["geom_text_size"]]
geom_text_size <- 5
axis_title_size <- text_sizes[["axis_text_x_size"]]
axis_title_size <- 12
  
# Define a custom R function named 'make_line_df_22_good_2_colors_plot' with an optional parameter 'my_df'.
make_line_df_22_good_2_colors_plot <- function(my_df = line_df_22_gom) {

  # Create a line plot based on the specified data frame or the default 'line_df_22_gom' data frame.
  line_plot <-

    # Use the pipe operator to chain a series of operations for 'my_df'.
    my_df |>

    # Filter rows where 'percent_non_compl_2_buckets' is "< 50%".
    filter(percent_non_compl_2_buckets == "< 50%") |>

    # Create a ggplot object for creating a line plot.
    ggplot(aes(
      # Define the x-axis as 'year_month' converted to a Date object.
      x = as.Date(year_month),
      
      # Define the y-axis as 'cnt_v_in_bucket2'.
      y = cnt_v_in_bucket2,
      
      # Define the color aesthetic using 'percent_non_compl_2_buckets'.
      color = percent_non_compl_2_buckets
    )) +

    # Add points to the plot.
    geom_point() +

    # Add lines to the plot.
    geom_line() +

    # Apply a black-and-white theme to the plot.
    theme_bw() +

    # Add text labels above the points.
    geom_text(
      aes(label = cnt_v_in_bucket2),
      vjust = -0.3,
      size = geom_text_size
    ) +

    # Add text labels below the points with a specific color.
    geom_text(
      aes(label = cnt_vsl_m_compl),
      vjust = 1.3,
      color = "skyblue1",
      size = geom_text_size
    ) +

    # Define the breaks and labels for the x-axis as well as date formatting.
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +

    # Customize the plot theme with specific settings.
    theme(
      legend.position = "none",
      plot.caption = element_text(size = text_sizes[["plot_caption_text_size"]]),
      axis.text.x = element_text(size = axis_title_size),
      axis.text.y = element_text(size = axis_title_size)
    ) +

    # Set plot labels.
    labs(
      size = "Groups of percentage",
      x = "Months (2022)",
      y = "Number of Vessels"
    ) +

    # Set the plot title.
    labs(
      title = "The Number of Non-Compliant Vessels Each Month\nThat Were Compliant More Than 50% of a Month in 2022"
    ) +

    # Set the caption for the plot.
    labs(caption = "(The blue number is a total number of non-compliant vessels per month.)");

  # Return the 'line_plot' object.
  return(line_plot)
}

line_df_22_good_plot_gom <- make_line_df_22_good_2_colors_plot()
line_df_22_good_plot_sa <-
  make_line_df_22_good_2_colors_plot(my_df = line_df_22_sa)

# Non compliant by month ----

line_plot_color = "blue"

# Define a custom R function named 'make_line_df_22_monthly_nc_plot' with optional parameters 'my_df' and 'permit_title'.
make_line_df_22_monthly_nc_plot <-
  function(my_df = line_df_22_gom,
           permit_title = "GOM + Dual") {
    # Create a line plot based on the specified data frame or the default 'line_df_22_gom' data frame.
    my_df |>
      
      # Create a ggplot object for creating a line plot.
      ggplot(aes(
        # Define the x-axis as 'year_month' converted to a Date object.
        x = as.Date(year_month),
        
        # Define the y-axis as 'cnt_vsl_m_compl'.
        y = cnt_vsl_m_compl,
        
        # Define the color aesthetic using 'line_plot_color'.
        color = line_plot_color
      )) +
      
      # Add points to the plot with a specific color and size.
      geom_point(color = line_plot_color,
                 size = 5) +
      
      # Add lines to the plot with a specific color and line width.
      geom_line(color = line_plot_color,
                linewidth = 1) +
      
      # Apply a black-and-white theme to the plot using 'theme_bw'.
      theme_bw() +
      
      # Add text labels above the points.
      geom_text(
        aes(label = cnt_vsl_m_compl),
        vjust = -1,
        hjust = -0.1,
        color = line_plot_color,
        size = geom_text_size
      ) +
      
      # Set the y-axis limits to the range of cnt_vsl_m_compl.
      # Set the lower limit by subtracting 1 from the minimum value of 'cnt_vsl_m_compl'.
      ggplot2::ylim(min(my_df$cnt_vsl_m_compl) - 1,
                    # Set the upper limit by adding 10 to the maximum value of 'cnt_vsl_m_compl'.
                    max(my_df$cnt_vsl_m_compl) + 10) +
      
    # Define the breaks and labels for the x-axis as well as date formatting.
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      
      # Customize the plot theme with specific settings.
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = axis_title_size),
        axis.text.y = element_text(size = axis_title_size)
      ) +
      
      # Set plot labels.
      labs(x = "Months (2022)",
           y = "Number of Vessels") +
      
      # Set the plot title using 'str_glue' to incorporate the 'permit_title' parameter.
      labs(
        title = str_glue(
          "The Number of Non-Compliant {permit_title} Permitted Vessels Each Month in 2022"
        )
      )
  }

line_df_22_gom_monthly_nc_plot <- make_line_df_22_monthly_nc_plot()

line_df_22_sa_monthly_nc_plot <- 
  make_line_df_22_monthly_nc_plot(my_df = line_df_22_sa,
           permit_title = "SA only")

line_df_22_sa_monthly_nc_plot

# Non compliant by month percent of total ----

# Split the data frame 'count_weeks_per_vsl_permit_year_compl_m_p' into a list of data frames based on 'year_permit'.
count_weeks_per_vsl_permit_year_compl_m_p_y_r <- split(
  # The data frame to be split.
  count_weeks_per_vsl_permit_year_compl_m_p,
  # The factor variable used for splitting.
  as.factor(count_weeks_per_vsl_permit_year_compl_m_p$year_permit)
)

count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short <-

  # Use the 'map' function to apply a function to each element of a list or vector.
  purrr::map(
    # The first argument to 'map' is a list or vector.
    count_weeks_per_vsl_permit_year_compl_m_p_y_r,
    
    # The second argument to 'map' is an anonymous function that operates on each element of the list.
    function(df_by_permit_year) {
      
      # The 'df_by_permit_year' variable represents a data frame for a specific permit and year.
      
      df_by_permit_year |>
      
        # Select specific columns from the data frame.
        dplyr::select(year_month,
               total_vsl_m,
               cnt_vsl_m_compl,
               compliant_) |>
      
        # Remove duplicate rows from the data frame based on all columns.
        dplyr::distinct() %>%
        
        # Return the resulting data frame. This is the final step in the data transformation.
        return()
    }
  )

map(count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short, dim)
# $`2022 gom_dual`
# [1] 24  4
# 
# $`2022 sa_only`
# [1] 24  4
# 
# $`2023 sa_dual`
# [1] 10  4


count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short_percent <-
  count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short |>
  # Use the 'map' function to apply a function to each element of a list or vector.
  purrr::map(# The first argument to 'map' is a list or vector.
    # The second argument to 'map' is an anonymous function that operates on each element of the list.
    function(df_by_permit_year) {
      # The 'df_by_permit_year' variable represents a data frame for a specific permit and year.
      df_by_permit_year |>
        dplyr::group_by(year_month) |>
        # Use the 'mutate' function to add a new column 'percent_of_total' to the data frame.
        # Calculate the 'percent_of_total' by computing the percentage of 'cnt_vsl_m_compl'
        # relative to 'total_vsl_m' for each group of 'year_month'.
        dplyr::mutate(percent_of_total = 100 * cnt_vsl_m_compl / total_vsl_m) |>
        dplyr::ungroup()
    })

line_df_monthly_nc_percent_plot <-
  names(count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short_percent) |>
  # Use the 'map' function to apply a function to each element of a list or vector.
  purrr::map(# The first argument to 'map' is a list or vector.
    # The second argument to 'map' is an anonymous function that operates on each element of the list.
    function(permit_year) {
      # The 'df_by_permit_year' variable represents a data frame for a specific permit and year.
      df_by_permit_year_non_compl <-
        count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short_percent[[permit_year]] |>
        # Use the 'filter' function to filter rows where 'compliant_' is equal to "NO".        
        dplyr::filter(compliant_ == "NO")
      
      df_by_permit_year_non_compl |>
        # Create a ggplot object for creating a line plot.
        ggplot2::ggplot(aes(
          # Define the x-axis as 'year_month' converted to a Date object.
          x = as.Date(year_month),
          
          # Define the y-axis as 'percent_of_total'.
          y = percent_of_total,
          
          # Define the color aesthetic for the plot using 'line_plot_color'.
          color = line_plot_color
        )) +
        
        # Add points to the plot with a specific color and size.
        ggplot2::geom_point(color = line_plot_color,
                   size = 4) +
        
        # Add a line to the plot with a specific color and line width.
        ggplot2::geom_line(color = line_plot_color,
                  linewidth = 1) +
        
        # Apply a black-and-white theme to the plot.
        ggplot2::theme_bw() +
        
        # Add text labels above the points using 'geom_text'.
        ggplot2::geom_text(
          ggplot2::aes(# The label for each point is the rounded 'percent_of_total' with a "%" symbol.
            label = paste0(round(percent_of_total, 1), "%")),
          # for SA:
          vjust = -1,
          hjust = -0.1,
          # for GOM:
          # vjust = -1, 
          # hjust = -0.5,
          color = line_plot_color,
          size = 5
        ) +
        
        # Define the breaks and labels for the x-axis as well as date formatting ("%b" means short Month).
        ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
        
        # Customize the plot theme with specific settings and predefined size.
        ggplot2::theme(
          legend.position = "none",
          axis.title = ggplot2::element_text(size = text_sizes[["axis_title_text_size"]] + 7),
          axis.text.x = ggplot2::element_text(size = text_sizes[["axis_text_x_size"]] - 2),
          axis.text.y = ggplot2::element_text(size = text_sizes[["axis_text_y_size"]] - 2)
        ) +
        # Set plot titles and axis labels.
        ggplot2::labs(title = permit_year,
             x = "Months (2022)",
             y = "Proportion of Non-Compliant Vessels") +
        
        # Expand the x-axis limits to the end of the year 2022 to show numbers on the right of Dec.
        ggplot2::expand_limits(x = as.Date("12/31/22", "%m/%d/%y")) +
        
        # Use the 'ylim' function to define the limits of the y-axis.

        ggplot2::ylim(
          
        # Calculate the lower limit by rounding up the minimum value of 'percent_of_total' and subtracting 1.
        #   min(ceiling(
        #   df_by_permit_year_non_compl$percent_of_total
        # )) - 1,
        0,        
        # Calculate the upper limit by rounding down the maximum value of 'percent_of_total' and adding 1.
        max(floor(
          df_by_permit_year_non_compl$percent_of_total
        )) + 1
        )
    })

    # print_df_names(count_weeks_per_vsl_permit_year_compl_m_p_c_cnts_short_percent$`2022 sa_only`)
# labs(title = "The Percent of Non-Compliant GOM + Dual Permitted Vessels Each Month in 2022") +
# dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
# as.Date(dates, "%m/%d/%y")

# sa
line_df_monthly_nc_percent_plot[[2]]
# gom
line_df_monthly_nc_percent_plot[[1]]

# save to files ----

plot_file_path_m <-
  file.path(plot_file_path, "per_month")
create_dir_if_not(plot_file_path_m)

plot_file_path_lines <-
  file.path(plot_file_path, "line_plots")
create_dir_if_not(plot_file_path_lines)

# file_full_name <- file.path(plot_file_path_lines,
                            # "gom_2022_mostly_right.png")

# save_plot_to_file(file.path(plot_file_path_m, "sa_nc_month_percent.png"),
                  # line_df_monthly_nc_percent_plot[[2]])

file_full_name <- file.path(plot_file_path_lines,
                            "sa_nc_month_percent.png")
plot_name <- line_df_monthly_nc_percent_plot[[2]]                     
ggplot2::ggsave(
  file_full_name,
  plot_name,
  width = 23,
  height = 23,
  units = "cm"
)


# see the function definition F2
# save_plots_list_to_files(file_full_name,
#                          # plots
#                          test_plot)

# test_df |> 
#   filter(year_month == "Jan 2022") |> 
#   View()

# Pipe the 'count_weeks_per_vsl_permit_year_compl_m_p_nc' data frame into the following operations.
count_weeks_per_vsl_permit_year_compl_m_p_nc |>

  # Filter rows where 'year_month' is "Jun 2022," 'year_permit' is "2022 gom_dual," and 'percent_compl_m' is less than 50.
  filter(year_month == "Jun 2022" &
           year_permit == "2022 gom_dual" &
           percent_compl_m < 50) |>

  # Summarize the data by counting the distinct 'vessel_official_number' values.
  summarise(n_distinct(vessel_official_number))
# print_df_names(test_df)

max_min_text <- "{cnt_v_in_bucket2} v / {cnt_vsl_m_compl} tot nc v"

min_max_val <-
  # Group 'test_df' by 'percent_non_compl_2_buckets'.
  line_df_22_sa |>
  dplyr::group_by(percent_non_compl_2_buckets) |>

  # Calculate the maximum and minimum values of 'perc_vsls_per_m_b2' within each group.
  mutate(
    max_dot_y = max(perc_vsls_per_m_b2),
    min_dot_y = min(perc_vsls_per_m_b2)
  ) |>

  # Remove grouping and ungroup the data frame.
  dplyr::ungroup() |>

  # Determine the 'year_month' where the maximum and minimum values occur for '< 50%' 'percent_non_compl_2_buckets'.
  mutate(
    max_dot_month =
      dplyr::case_when(
        perc_vsls_per_m_b2 == max_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      ),
    min_dot_month =
      dplyr::case_when(
        perc_vsls_per_m_b2 == min_dot_y &
          percent_non_compl_2_buckets == "< 50%" ~ year_month
      )
  ) |>

  # Create text labels based on the presence of 'max_dot_month' and 'min_dot_month'.
  mutate(
    max_dot_text =
      dplyr::case_when(!is.na(max_dot_month) ~ str_glue(max_min_text)),
    min_dot_text =
      dplyr::case_when(!is.na(min_dot_month) ~ str_glue(max_min_text))
  )

#### Current file:  ~/R_code_github/quantify_compliance/quantify_compliance_from_fhier_vms.R  ----

# Above compliance metrics, to assess pre and post VMS requirement or vs increase in VMS ----
# compliance (just Gulf + dual permitted vessels; assess Feb 2022 (=pre-t), March 2022 (VMS implementation), and Sept 2022 (when 80% vessels had a registered VMS))

compl_clean_sa_vs_gom_m_int_filtered |>
  dplyr::select(year_month) |>
  dplyr::distinct()
# dim(compl_clean_sa_vs_gom_m_int_filtered)

# Choose 3 month (pre, in and post VMS requirement)
compl_clean_sa_vs_gom_m_int_filtered_vms <-
  compl_clean_sa_vs_gom_m_int_filtered %>%
  # Filter rows where 'year_permit' is "2022 gom_dual" and 'year_month' matches the specified values.
  dplyr::filter(year_permit == "2022 gom_dual" &
                  year_month %in% c("Feb 2022",
                                    "Mar 2022",
                                    "Sep 2022"))

dim(compl_clean_sa_vs_gom_m_int_filtered_vms)
# [1] 12677    25

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt <-
  add_total_cnt_in_gr(compl_clean_sa_vs_gom_m_int_filtered_vms, "year_month")
# dplyr::glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt %>%
  dplyr::select(year_month, total_vsl_y) %>%
  unique()
# 1 Sep 2022          1144
# 2 Mar 2022          1031
# 3 Feb 2022          1034

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp <-
  expired_or_not(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt)

# dplyr::glimpse(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp)

group_by_var <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt <-
  count_expiration_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp,
                      group_by_var)

# dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt)

## fewer fields ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short <-
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt %>%
  dplyr::select(
    vessel_official_number,
    year_permit,
    year_month,
    compliant_,
    total_vsl_y,
    perm_exp_y,
    exp_y_tot_cnt
  ) %>%
  # can unique, because already counted
  unique()

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short)
# [1] 3319    7

## get compl_counts ----
### get compl, no compl, or both per period ----
group_by_for_compl <- vars(-c("vessel_official_number", "compliant_"))

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide <-
  get_compl_by(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short,
               group_by_for_compl)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide)
# [1]    6 1264

### count compl, no compl, or both per period, permit, active status ----
cols_names <-
  c("year_permit", "year_month", "total_vsl_y", "perm_exp_y", "exp_y_tot_cnt")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long <-
  count_by_cols(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide,
                cols_names)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long)
# [1] 7554    7

### get cnts for compl, no compl, or both per month with exp ----
group_by_cols <- c("year_month", "perm_exp_y")
cols_to_cnt <- c("year_month", "perm_exp_y", "is_compl_or_both")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt <-
  cnts_for_compl(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long, group_by_cols, cols_to_cnt)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt)
# [1] 23  7

### check if a vessel is compliant and not at the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(is_compl_or_both) == dplyr::n_distinct(.$is_compl_or_both)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::ungroup() |> 
  dplyr::glimpse()
# $ year_month             <yearmon> Sep 2022, Mar 2022, Feb 2022
# $ total_vsl_y            <int> 1144, 1031, 1034
# $ perm_exp_y             <chr> "expired", "expired", "expired"
# $ exp_y_tot_cnt          <int> 62, 108, 112
# $ vessel_official_number <chr> "657209", "657209", "657209"
# $ is_compl_or_both       <chr> "YES", "NO_YES", "NO"
# $ shared                 <lgl> TRUE, TRUE, TRUE

  # dim()
# 3
# TODO: fix

### check if a vessel permit is expired and not in the same time ----
compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long %>%
  dplyr::filter(!is.na(is_compl_or_both)) %>%
  dplyr::group_by(vessel_official_number) %>%
  dplyr::mutate(shared =
                  dplyr::n_distinct(perm_exp_y) == dplyr::n_distinct(.$perm_exp_y)) %>%
  dplyr::filter(shared == TRUE) %>%
  dplyr::arrange(vessel_official_number) %>%
  dplyr::ungroup() |> 
  dim()
# 0 ok

### check total_vsl_y vs. sum_cnts ----
compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::group_by(compliant_) %>%
  dplyr::mutate(tota_vsl_m =
                  dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_) %>%
  unique() %>%
  head()
# 1       1249 YES
# 2        117 NO
# 1249 + 117 = 1366
# TODO: what to compare with?

compl_clean_sa_vs_gom_m_int_filtered_vms %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::mutate(exp_w_end_diff_y =
                  as.numeric(as.Date(permitgroupexpiration) -
                               end_of_2022)) %>%
  mutate(perm_exp_y =
           dplyr::case_when(exp_w_end_diff_y <= 0 ~ "expired",
                     exp_w_end_diff_y > 0 ~ "active")) %>%
  dplyr::group_by(perm_exp_y) %>%
  dplyr::mutate(tota_vsl_m = dplyr::n_distinct(vessel_official_number)) %>%
  dplyr::ungroup() %>%
  dplyr::select(tota_vsl_m, compliant_, perm_exp_y) %>%
  unique() %>%
  head()
# 1       1140 YES        active
# 2        119 YES        expired
# 3       1140 NO         active
# 4        119 NO         expired
# 1140 + 119
# 1259

## add total cnts ----
# active vs expired per year, permit, compl, permit expiration
group_by_compl_cols <- c("year_month", "compl_or_not")
group_by_exp_cols <- c("year_month", "perm_exp_y")

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot <-
  add_total_cnts(
    compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt,
    group_by_compl_cols,
    group_by_exp_cols
  )

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot)
# [1] 17 10

## add percents of total ----
select_cols <- c(
  "year_month",
  "total_vsl_y",
  "perm_exp_y",
  "compl_or_not",
  "cnt_y_p_c",
  "cnt_y_p_e"
)

compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc <-
  add_percents_of_total(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot,
                        select_cols)

dim(compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc)
# [1] 12  7

# plots VMS:
# Create a new data frame 'gg_all_c_vs_nc_plots_vms' by performing a series of operations.
gg_all_c_vs_nc_plots_vms <-

  # Start with the 'year_month' column of 'compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc'.
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc$year_month %>%

  # Get unique 'year_month' values.
  unique() %>%

  # Sort the 'year_month' values in ascending order.
  sort() %>%

  # Use purrr::map to perform the following operations for each 'year_month'.
  purrr::map(function(curr_year_month) {

    # Create a data frame 'curr_df' by filtering rows with 'year_month' matching 'curr_year_month'.
    curr_df <-
      compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt_short_wide_long_cnt_tot_perc %>%
      dplyr::filter(year_month == curr_year_month)

    # Generate a title for the year and permit label using 'make_year_permit_label' function (See its definition by F2).
    y_r_title <-
      make_year_permit_label(curr_year_month)

    # Extract unique 'total_vsl_y' values.
    total_vsls <- unique(curr_df$total_vsl_y)

    # Extract unique active permits.
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(cnt_y_p_e) %>%
      dplyr::distinct()

    # Extract unique expired permits.
    expired_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "expired") %>%
      dplyr::select(cnt_y_p_e) %>%
      dplyr::distinct()

    # Create a title for the current plot.
    current_title <-
      paste0(
        "GOM + Dual",
        " ",
        curr_year_month
      )

    # Create a plot for compliance vs. non-compliance using 'make_one_plot_compl_vs_non_compl' function.
    one_plot <-
      curr_df %>%
      dplyr::select(compl_or_not, perc_c_nc) %>%
      dplyr::distinct() %>%
      make_one_plot_compl_vs_non_compl(current_title,
                                       is_compliant = "compl_or_not",
                                       percent = "perc_c_nc",
                                       default_percent_labels = FALSE)

    # Return the generated plot for this 'year_month'.
    return(one_plot)
  })

main_title_vms <- "Percent Compliant vs. Noncompliant SEFHIER Vessels (VMS)"

# combine plots for 2022
gg_arranged_plots_vms <-
  grid.arrange(
    gg_all_c_vs_nc_plots_vms[[1]],
    gg_all_c_vs_nc_plots_vms[[2]],
    gg_all_c_vs_nc_plots_vms[[3]],
    top = main_title_vms
  )
# class(gg_all_c_vs_nc_plots_vms)

vms_plot_file_path <-
  file.path(plot_file_path, "vms")
create_dir_if_not(vms_plot_file_path)

## save VMS green and red plots ----
save_plots_list_to_files(file.path(vms_plot_file_path,
                                   "vms_3_months.png"),
                         gg_arranged_plots_vms)

# Non compliant only ----
# 1) count percents - a given vsl non_compl per counted weeks total ----
## 1a) how many weeks each vessel was present ----
# Create a new data frame 'weeks_per_vsl_year_month_vms_compl_cnt' by performing a series of operations.
weeks_per_vsl_year_month_vms_compl_cnt <-
  
  # Start with the 'compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt' data frame.
  compl_clean_sa_vs_gom_m_int_filtered_vms_cnt_exp_cnt |>
  
  # Add a count column 'weeks_per_vessel_per_compl' that counts the number of weeks per vessel per compliance status.
  dplyr::add_count(year_month, 
                   vessel_official_number, 
                   compliant_, 
                   name = "weeks_per_vessel_per_compl") %>%
  
  # Add a count column 'total_weeks_per_vessel' that counts the total number of weeks per vessel.
  dplyr::add_count(year_month, 
                   vessel_official_number, 
                   name = "total_weeks_per_vessel") %>%
  
  # Remove grouping and ungroup the data frame.
  dplyr::ungroup()

dim(weeks_per_vsl_year_month_vms_compl_cnt)
# [1] 12677    31

## 1b) percent of compl/non-compl per total weeks each vsl was present ----

weeks_per_vsl_year_month_vms_compl_cnt_perc <-

  # Start with the 'weeks_per_vsl_year_month_vms_compl_cnt' data frame.
  weeks_per_vsl_year_month_vms_compl_cnt %>%

  # Mutate to calculate the 'percent_compl' column, which represents the percentage of compliance per vessel.
  dplyr::mutate(percent_compl =
           weeks_per_vessel_per_compl * 100 / total_weeks_per_vessel)

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc)
# [1] 12677    32

# check
weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  filter(vessel_official_number == "FL3327TJ") %>%
  select(
    year_month,
    compliant_,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique() %>%
  dplyr::glimpse()
# $ year_month                 <yearmon> Sep 2022, Sep 2022
# $ compliant_                 <chr> "NO", "YES"
# $ weeks_per_vessel_per_compl <int> 1, 3
# $ total_weeks_per_vessel     <int> 4, 4
# $ percent_compl              <dbl> 25, 75

# 2) split nc percentage into 4 buckets ----
## 2a Only non-compl and fewer cols ----

weeks_per_vsl_year_month_vms_compl_cnt_perc_short <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc %>%
  dplyr::filter(compliant_ == "NO") %>%
  dplyr::select(
    year_month,
    vessel_official_number,
    perm_exp_y,
    exp_y_tot_cnt,
    weeks_per_vessel_per_compl,
    total_weeks_per_vessel,
    percent_compl
  ) %>%
  unique()

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short)
# [1] 159   7

## 2b) get percentage "buckets" ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_y_p)

# See the function definition F2
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts <-
  get_p_buckets(weeks_per_vsl_year_month_vms_compl_cnt_perc_short,
                "percent_compl")

dim(weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts)
# [1] 159   8

### test 2 ----
# count in one bucket
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
  dplyr::filter(percent_n_compl_rank == "75<= & <=100%") %>%
  dplyr::filter(year_month == "Mar 2022") %>%
  dplyr::count(percent_compl, year_month,
               name = "amount_of_occurences") %>%
  dplyr::arrange(desc(percent_compl)) %>%
  # dplyr::glimpse()
  # $ percent_compl        <dbl> 100, 75
  # $ year_month           <yearmon> Mar 2022, Mar 2022
  # $ amount_of_occurences <int> 18, 2

  # sum amount_of_occurences
  dplyr::count(wt = amount_of_occurences)
# 55 all
# 20 March

# 3) count how many in each bucket ----

  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b <-
    weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts %>%
    dplyr::add_count(year_month,
                     percent_n_compl_rank,
                     name = "cnt_v_in_bucket")

### test 3 ----
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
    # dplyr::filter(year_permit == "2022 sa_only") %>%
    dplyr::select(year_month,
                  percent_n_compl_rank,
                  cnt_v_in_bucket) %>%
    unique() %>%
    dplyr::add_count(wt = cnt_v_in_bucket, name = "total_per_period") %>%
    dplyr::arrange(percent_n_compl_rank) %>%
    dplyr::glimpse()
# $ year_month           <yearmon> Sep 2022, Mar 2022, Feb 2022, Sep 2022, Mar 202…
# $ percent_n_compl_rank <chr> "25<= & <50%", "25<= & <50%", "25<= & <50%", "50<= …
# $ cnt_v_in_bucket      <int> 26, 32, 26, 6, 8, 6, 15, 20, 20
# $ total_per_period     <int> 159, 159, 159, 159, 159, 159, 159, 159, 159

# 4) cnt percents of (3) ----
# View(count_weeks_per_vsl_permit_year_n_compl_p_short_cuts_cnt_in_b)

weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b %>%
  # cnt vessels per period and compliance
  dplyr::add_count(year_month,
                   name = "vsls_per_y_r") %>%
  dplyr::mutate(perc_vsls_per_y_r_b = cnt_v_in_bucket * 100 / vsls_per_y_r) %>%
  dplyr::mutate(perc_labels = paste0(round(perc_vsls_per_y_r_b, 0), "%"))

### check 4 ----
weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
  # dplyr::filter(year_permit == "2022 sa_only") %>%
  dplyr::select(percent_n_compl_rank,
                perc_vsls_per_y_r_b) %>%
  unique() %>%
  dplyr::arrange(percent_n_compl_rank) %>%
  head()

# 1 25<= & <50%                         55.3
# 2 25<= & <50%                         53.3
# 3 25<= & <50%                         50
# 4 50<= & <75%                         12.8
# 5 50<= & <75%                         13.3
# 6 50<= & <75%                         11.5

# 5) blue plots by year ----

blue_year_plot_titles <-
  data.frame(
    first_part = c(
      "GOM + Dual Permitted Vessels\n("
    )
  )

# Create a list of plots 'gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc' for each 'year_month'.
gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc <-

  # Start with the 'year_month' column of 'weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc'.
  weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc$year_month %>%

  # Get unique 'year_month' values.
  unique() %>%

  # Sort the 'year_month' values in ascending order.
  sort() %>%

  # Use purrr::map to create a plot for each 'year_month'.
  purrr::map(function(curr_year_month) {

    # Create a data frame 'curr_df' by filtering rows with 'year_month' matching 'curr_year_month'.
    curr_df <-
      weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc %>%
      dplyr::filter(year_month == curr_year_month)

    # Extract a data frame 'total_non_compl_df' with relevant columns.
    total_non_compl_df <-
      curr_df %>%
      dplyr::select(perc_vsls_per_y_r_b,
                    percent_n_compl_rank,
                    perc_labels,
                    vsls_per_y_r) %>%
      unique()

    # Extract active permits.
    active_permits <- curr_df %>%
      dplyr::filter(perm_exp_y == "active") %>%
      dplyr::select(exp_y_tot_cnt)

    # Extract expired permits.
    expired_permits <- curr_df %>%
      filter(perm_exp_y == "expired") %>%
      dplyr::select(exp_y_tot_cnt)

    # Create a title for the plot.
    y_p_title <-
      paste0(
        curr_year_month,
        " (Total Non-Compliant = ",
        total_non_compl_df$vsls_per_y_r,
        " Vessels)"
      )

    # Create a bar plot 'one_plot' using ggplot2.
    one_plot <-
      ggplot(total_non_compl_df,
             aes(x = percent_n_compl_rank,
                 y = perc_vsls_per_y_r_b)) +
      geom_col(fill = "deepskyblue") +
      labs(title = y_p_title,
           x = "",
           y = "") +
      # Add labels to the bars.
      geom_text(aes(label = perc_labels),
                position = position_stack(vjust = 0.5)) +
      # Set the y-axis limits from 0 to 100.
      ylim(0, 100) +
      # Adjust the size of the plot's title.
      theme(plot.title =
              element_text(size = 10))

    # Return the generated plot for this 'year_month'.
    return(one_plot)
  })


# main_blue_title <- "% non compliant vessels per period"
ndash <- "\u2013"
main_blue_title <- paste0(
  "% Non-Compliant (VMS) Vessels Missing <25%, 25%", ndash, "49.9%, 50%", ndash, "74.9%, >=75% of their reports"
)


grid.arrange(gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[1]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[2]],
             gg_weeks_per_vsl_year_month_vms_compl_cnt_perc_short_cuts_cnt_in_b_perc[[3]],
             top = main_blue_title)

