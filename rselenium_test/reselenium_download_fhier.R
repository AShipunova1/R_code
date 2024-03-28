# setup ----
require("RSelenium")
# require("rstudioapi")
# require("XML")
require("tidyverse")
require("tictoc")

Sys.setenv(TZ = Sys.timezone())
curr_tz <- Sys.timezone()

remDr <- rsDriver(browser = "firefox",
               chromever = NULL,
               port = 4444L)

remote_driver <- remDr[["client"]]

my_year <- "2022"

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

# go to FHIER ----
login_into_fhier <- function() {

  remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")

  print(remote_driver$getTitle() == "South East For-hire Electronic Reporting program")
  # T

  # login ----
  P101_USERNAME <-
    remote_driver$findElement(using = "id", "P101_USERNAME")
  P101_USERNAME$clearElement()
  P101_USERNAME$sendKeysToElement(list(rstudioapi::askForSecret("fhier_username")))

  P101_PASSWORD <-
    remote_driver$findElement(using = "id", "P101_PASSWORD")
  P101_PASSWORD$clearElement()
  P101_PASSWORD$sendKeysToElement(list(rstudioapi::askForSecret("fhier_password")))

  login_button <-
    remote_driver$findElement(using = "id",
                              value = "B705187520008063447")

  login_button$clickElement()
}

login_into_fhier()

# open compliance ----
open_menu_item <- function(page_name) {

  menu_links <-
    remote_driver$findElements(using = "tag name",
                               value = "a")

  # View(menu_links)
  for (element in menu_links) {
    if (element$getElementText() == page_name) {
      menu_report <- element
    }
  }

  menu_report$clickElement()
}

open_menu_item("Reports")

# open FHIER COMPLIANCE REPORT ----

# element_text = "FHIER COMPLIANCE REPORT"

my_td = remote_driver$findElement(using = "xpath",
                                  value = "/html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]")

# my_td$getElementAttribute("text")

my_td$clickElement()

## choose year ----
# <select id="P300_COMP_YEAR" name="P300_COMP_YEAR" class="selectlist apex-item-select" style="font-family:monospace; font-size:12px" size="1"><option value="2024" selected="selected">2024</option>
# <option value="2023">2023</option>
# <option value="2022">2022</option>
# </select>
remote_driver$findElement("id", 'P300_COMP_YEAR')$clickElement()

# /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div[1]/div[2]/div[1]/div/div[2]/select/option[2]
# 2023
remote_driver$findElement("xpath", "//option[2]")$clickElement()

# 2022
# remote_driver$findElement("xpath", "//option[3]")$clickElement()

# click search to load the year
remote_driver$findElement("xpath", "//*[@id='B717215823519513218']")$clickElement()

# download ----
get_all_buttons <-
  function() {
    buttons_elements <-
      remote_driver$findElements(using = "tag name",
                                 value = "button")
    return(buttons_elements)
  }

buttons_elements <- get_all_buttons()
map(buttons_elements, ~ print(.x$getElementText()))

download_table <- function() {
  # there 3 layers, so we need to find buttons 3 time to load the last Download button

  # 1
  buttons_elements <- get_all_buttons()
  for (element in buttons_elements) {
    if (element$getElementText() == "Actions") {
      action_button <- element
    }
  }

  action_button$clickElement()

  # 2
  buttons_elements <- get_all_buttons()
  # map(buttons_elements, ~ print(.x$getElementText()))
  for (element in buttons_elements) {
    if (element$getElementText() == "Download") {
      download_button1 <- element
    }
  }

  download_button1$isElementDisplayed()

  download_button1$clickElement()

  # 3
  buttons_elements <- get_all_buttons()
  # map(buttons_elements, ~ print(.x$getElementText()))
  for (element in buttons_elements) {
    if (element$getElementText() == "Download") {
      download_button <- element
    }
  }

  download_button$isElementDisplayed()

  download_button$clickElement()
  download_start_time <- Sys.time()
  return(download_start_time)
}

# read_in ----
## find the downloaded file ----

find_the_downloaded_file <-
  function(file_name_pattern) {
    download_folder <- file.path(r"(~\..\Downloads)")
    # download_folder <- file.path(r"(C:\Users\anna.shipunova\Downloads)")

    downloaded_compl_files <-
      list.files(download_folder,
                 full.names = T,
                 pattern = file_name_pattern)

    glimpse(downloaded_compl_files)

    files_info <-
      file.info(downloaded_compl_files)

    # View(files_info)

    # newest_time <- max(files_info$mtime) |> as.POSIXct(curr_tz)
    # download_start_time |> as.POSIXct(curr_tz)

    if (!newest_time > download_start_time) {
      Sys.sleep(10)
    }

    newest_file_path <-
      files_info |>
      filter(mtime == newest_time) |>
      rownames()

    fhier_file_downloaded <-
      read_csv(newest_file_path)

    return(fhier_file_downloaded)
  }
file_name_pattern = "^FHIER Compliance.*csv"

