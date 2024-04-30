# using functions
source("reselenium_download.R")

my_year <- "2024"

remote_driver$getStatus()
# remDr$server$stop()
# remote_driver$close()

login_into_fhier()

open_menu_item("Maintenance")

# get the sc link

all_table_rows <- remote_driver$findElements("tag name", "tr")

# all_table_rows[[1]]$findChildElement
# /html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[12]/td[2]

# my_tr <- all_table_rows[[12]]

# all_td <- all_table_rows[[1]]$findChildElements("xpath",
#                             "//td[contains(text(), 'SC VESSELS REPORTING VIA VESL')]")

# all_td <- all_table_rows[[12]]$findElement("xpath",
#                             "//*[contains(text(), 'SC VESSELS REPORTING VIA VESL')]")

  # col_names <- remDr$findElements("xpath", paste(table_xpath, "/div/div/div[2]/div[1]/div[2]/div[2]/div/div", sep = ""))
  # col_names <- vapply(col_names, function(x) stringr::str_split(x$getElementAttribute('innerHTML')[[1]], "<")[[1]][1], character(1))

length(all_table_rows)
# 24

# all_td |> length()

# all_td[[1]]$getElementTagName()
# all_td[[1]]$getElementText()
# [1] "SC VESSELS REPORTING VIA VESL"

# all_td[[1]]$getElementAttribute("xpath")

get_row_idx <- function() {
  idx <- 1
  for (my_tr in all_table_rows) {
    # browser()
    print(idx)

    idx <- idx + 1

    if (grepl("SC VESSELS REPORTING VIA VESL", my_tr$getElementText()) == FALSE) {
      next
    }
    else {
      return(idx - 1)
    }
  }
}

my_idx <- get_row_idx()

all_table_rows[[my_idx]]$getElementAttribute('innerHTML')
a_tag <- all_table_rows[[my_idx]]$findChildElement("tag name", "a")
a_tag$clickElement()

download_start_time <- download_table()

file_name_pattern = "^Report 1.*csv"
my_path <- find_the_downloaded_file(file_name_pattern)
sc_maintenance_from_fhier <- read_csv(my_path)

glimpse(sc_maintenance_from_fhier)
