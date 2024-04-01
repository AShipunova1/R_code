# using functions
source("reselenium_download.R")

my_year <- "2023"

remote_driver$getStatus()
# remDr$server$stop()
# remote_driver$close()

login_into_fhier()
# open_menu_item("Reports")

open_menu_item("Correspondence")

curr_start_date = str_glue("01/01/{my_year}")
curr_end_date = str_glue("12/31/{my_year}")

choose_correspondence_dates(curr_start_date,
                            curr_end_date)

download_start_time <- download_table()

file_name_pattern = "^Correspondence.*csv"
correspondence_from_fhier <- read_file(file_name_pattern)

glimpse(correspondence_from_fhier)

