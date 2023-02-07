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

join_all_csvs <- function(csvs) {
  csvs_clean1 <- lapply(csvs, clean_headers)
  for (i in seq_along(csvs_clean1)){
    csvs_clean1[[i]]$vesselofficialnumber <-
      trimws(csvs_clean1[[i]]$vesselofficialnumber)
  }

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

