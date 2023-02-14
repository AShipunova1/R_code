# useful functions
# usage example:
# get_compl_and_corresp_data <- function() {
#   my_paths <- set_work_dir()
#   csv_names_list = list(file.path(add_path_corresp,  "Correspondence21_23.csv"),
#                         file.path(add_path_compl, "FHIER_Compliance_22.csv"),
#                         file.path(add_path_compl, "FHIER_Compliance_23.csv"))
#   # Load all csv files to data frames
#   csv_contents_egr <- load_csv_names(my_paths, csv_names_list)

#   # unify headers and trim vessel official numbers across all the dfs
#   csvs_clean1 <- clean_all_csvs(csv_contents_egr)

#   # For correspondence only:
#   corresp_arr <- csvs_clean1[[1]]
#   # add a column with contact frequency per vessel official number
#   corresp_arr_contact_cnts <- add_count_contacts(corresp_arr)
#   # change all dates classes from char to POSIXct
#   corresp_arr_contact_cnts %>%
#     change_to_dates("createdon", "%m/%d/%Y %H:%M") %>%
#     change_to_dates("contactdate", "%m/%d/%Y %I:%M %p") ->
#     corresp_arr_contact_cnts_clean

#   # For compliance info only:
#   compl_arr <- list(csvs_clean1[[2]], csvs_clean1[[3]])
#   # combine all compliance data frames into one
#   compl <- compl_arr
#   if (!is.data.frame(compl_arr)) {
#     compl <- join_same_kind_csvs(compl_arr)
#   }

#   compl %>%
#     # split week column (52: 12/26/2022 - 01/01/2023) into 3 columns with proper classes, week_num (week order number), week_start and week_end
#     clean_weeks() %>%
#     # change dates classes from char to POSIXct
#     change_to_dates("permitgroupexpiration", "%m/%d/%Y") ->
#     compl_clean

#   return(list(my_paths, compl_clean, corresp_arr_contact_cnts_clean))
# }

# # get csv data into variables
# temp_var <- get_compl_and_corresp_data()
# my_paths <- temp_var[[1]]
# compl_clean <- temp_var[[2]]
# corresp_contact_cnts_clean <- temp_var[[3]]

source("~/GitHub/R_code/start_module.R")
# functions to clean FHIER compliance and correspondense reports
cleen_weeks <- function(my_df) {
  my_df %>%
    separate_wider_delim(week, ":", names = c("week_num", "week_rest")) %>%
    separate_wider_delim(week_rest, " - ", names = c("week_start", "week_end")) ->
    temp_df

  my_df$week_num <- as.integer(trimws(temp_df$week_num))
  my_df$week_start <- as.Date(trimws(temp_df$week_start), "%m/%d/%Y")
  my_df$week_end <- as.Date(trimws(temp_df$week_end), "%m/%d/%Y")

  return(my_df)
}

trim_all_vessel_ids <- function(csvs_clean) {
  for (i in seq_along(csvs_clean)){
    id_name <- grep("vessel.*number", names(csvs_clean[[i]]), value = TRUE)
    # id_name
    trimmed_ids <- lapply(csvs_clean[[i]][id_name], trimws)

    csvs_clean[[i]] %<>% mutate(vesselofficialnumber = unlist(trimmed_ids))
  }
  return(csvs_clean)
  # gives incorrect output:
#   chr [1:6262] "c(\"VA9236AV\", \"VA7344AW\", \"VA6784AD\", \"VA3571BF\", \"VA1460CJ\", \"VA1267CJ\", \"VA0830CF\", \"TX9645BW\"| __truncated__ ...

}

trim_all_vessel_ids_simple <- function(csvs_clean) {
    for (i in seq_along(csvs_clean)){
    csvs_clean[[i]]$vesselofficialnumber <-
      trimws(csvs_clean[[i]]$vesselofficialnumber)
  }
  return(csvs_clean)
}

clean_all_csvs <- function(csvs) {
  csvs_clean0 <- lapply(csvs, clean_headers)

  csvs_clean1 <- trim_all_vessel_ids_simple(csvs_clean0)

  # str(csvs_clean1) %>% print()

  return(csvs_clean1)
}

join_same_kind_csvs <- function(csvs_list_2_plus) {
  return(bind_rows(csvs_list_2_plus))
}

join_all_csvs <- function(corresp_arr, compl_arr) {
  # dim(corresp_arr) %>% print()
  corresp <- corresp_arr
  if (!is.data.frame(corresp_arr)) {
    corresp <- join_same_kind_csvs(corresp_arr)
  }

  # dim(corresp) %>% print()
  # dim(compl_arr) %>% print()

  compl <- compl_arr
  if (!is.data.frame(compl_arr)) {
    compl <- join_same_kind_csvs(compl_arr)
  }

  # dim(compl) %>% print()
  # corresp <- join_same_kind_csvs(csvs_clean1[[1]], csvs_clean1[[2]])
  # compl <- rbind(csvs_clean1[[3]], csvs_clean1[[4]])

  compl %>%
    full_join(corresp,
              by = c("vesselofficialnumber"),
              multiple = "all") ->
    data_join_all

  return(data_join_all)
}

# change_classes <- function(my_csv_df) {
#   all_data_df_cleen <-
#     my_csv_df %>%
#     cleen_weeks() %>%
#     mutate(permitgroupexpiration =
#              as.Date(permitgroupexpiration, "%m/%d/%Y")) %>%
#     mutate(createdon = as.POSIXct(createdon, format = "%m/%d/%Y %H:%M")) %>%
#     mutate(contactdate = as.POSIXct(contactdate,
#                                     format = "%m/%d/%Y %I:%M %p",
#                                     tz = "America/New_York"))

#   return(all_data_df_cleen)
# }

change_to_dates <- function(my_df, field_name, date_format) {
  my_df %>%
    mutate({{field_name}} := as.POSIXct(pull(my_df[field_name]),
    format = date_format)) %>%
    return()
}

# E.g. group_by_arr <- c("vesselofficialnumber", "contacttype")
count_by_column_list <- function(my_df, group_by_list) {
  my_df %>%
    arrange(vesselofficialnumber) %>%
    group_by_at(group_by_arr) %>%
    summarise(my_freq = n()) %>%
    return()
}

add_count_contacts <- function(all_data_df_cleen) {
  all_data_df_cleen %>%
    mutate(was_contacted = if_else(is.na(contactdate), "no", "yes")) %>%
    add_count(vesselofficialnumber, was_contacted, name = "contact_freq") %>%
    return()
}
