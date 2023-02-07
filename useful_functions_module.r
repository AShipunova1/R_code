# useful functions
# usage:
# main <- function() {
#   source("~/GitHub/R_code/start_module.R")
#   my_paths <- set_work_dir()
#   csv_names_list = list("Correspondence2022.csv", 
#                         "Correspondence2023.csv",
#                         "Compliance2022.csv",
#                         "Compliance2023.csv")
#   csv_contents <- load_csv_names(my_paths, csv_names_list)
#   all_data_df <- join_all_csvs(csv_contents)
#   all_data_df_cleen <- change_classes(all_data_df)
# }

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
}

join_all_csvs <- function(csvs) {
  csvs_clean0 <- lapply(csvs, clean_headers)

  csvs_clean1 <- trim_all_vessel_ids(csvs_clean0)

  print(csvs_clean1)

  corresp <- rbind(csvs_clean1[[1]], csvs_clean1[[2]])
  compl <- rbind(csvs_clean1[[3]], csvs_clean1[[4]])

  compl %>%
    full_join(corresp,
              by = c("vesselofficialnumber")) ->
    data_join_all

  return(data_join_all)
}

change_classes <- function(my_csv_df) {
  all_data_df_cleen <-
    my_csv_df %>%
    cleen_weeks() %>%
    mutate(permitgroupexpiration =
             as.Date(permitgroupexpiration, "%m/%d/%Y")) %>%
    mutate(createdon = as.POSIXct(createdon, format = "%m/%d/%Y %H:%M")) %>%
    mutate(contactdate = as.POSIXct(contactdate,
                                    format = "%m/%d/%Y %I:%M %p",
                                    tz = "America/New_York"))

  return(all_data_df_cleen)

}

