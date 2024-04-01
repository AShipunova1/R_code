source("reselenium_download.R")

# go to PIMS search ----
remote_driver$navigate("https://appscloud.fisheries.noaa.gov/suite/sites/pims-search")

correct_page_title <-
  (remote_driver$getTitle() == "SEARCH - PIMS Search")
correct_page_title
# T

# go to Permits ----
Sys.sleep(10)
permits_menu_item <-
  remote_driver$findElement("xpath",
                            "//p[contains(text(), 'Permits')]")

permits_menu_item$clickElement()

download_button <-
  remote_driver$findElement(
    "xpath",
    "//span[contains(text(), 'Export to Excel - Exports the currently filtered list of records as an Excel file.')]"
  )

download_start_time <- Sys.time()
download_button$clickElement()

# read the downloaded file ----

file_name_pattern = "^Permits .*xlsx"
permits_from_pims <- read_new_file(file_name_pattern)
glimpse(permits_from_pims)

## clean up the file ----
# mv out first 3 rows,
# rename columns as in X4,

permits_from_pims_3_r <-
  slice(permits_from_pims, 3)

permits_from_pims_clean0 <-
  tail(permits_from_pims, -3)

# glimpse(permits_from_pims_clean0)

names(permits_from_pims_clean0) <- permits_from_pims_clean0[1,]

permits_from_pims_clean <-
  tail(permits_from_pims_clean0, -1)

# View(permits_from_pims_clean)
