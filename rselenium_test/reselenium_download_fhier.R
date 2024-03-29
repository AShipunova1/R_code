# setup ----
require("RSelenium")
# require("rstudioapi")
# require("XML")
require("tidyverse")
require("tictoc")
require("openxlsx")

Sys.setenv(TZ = Sys.timezone())
curr_tz <- Sys.timezone()

# closes itself if ran from the function
# start_browser <- function() {
extraCap = list("moz:firefoxOptions" = list(args = list('--headless')))
# add bellow to rsDriver
# , extraCapabilities = extraCap

remDr <- rsDriver(browser = "firefox",
                  chromever = NULL,
                  port = 4444L
                  )

remote_driver <- remDr[["client"]]

  # return(remote_driver)
# }

# remote_driver$close
# my_year <- "2022"

# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

# go to FHIER ----
login_into_fhier <- function() {

  # remote_driver <- start_browser()
  remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")

  # print(remote_driver$getTitle() == "South East For-hire Electronic Reporting program")
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

  # return(remote_driver)
}

# remote_driver <- login_into_fhier()

# glimpse(remote_driver)
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

# open_menu_item("Reports")

# element_text = "FHIER COMPLIANCE REPORT"

choose_correspondence_dates <-
  function(curr_start_date = "01/01/2023",
           curr_end_date = "12/31/2023") {

    start_date <-
      remote_driver$findElement(using = "xpath",
                                value = "//*[@id='P225_START_DT']")

    start_date$clickElement()
    # start_date$sendKeysToActiveElement(list(curr_start_date, key = "enter"))
    start_date$sendKeysToElement(list(curr_start_date, key = "enter"))

    end_date <-
      remote_driver$findElement(using = "xpath",
                                value = "//*[@id='P225_END_DT']")
    end_date$clickElement()
    end_date$sendKeysToElement(list(curr_end_date, key = "enter"))

    go_button <-
      remote_driver$findElement(using = "xpath",
                                value = "//*[@id='P225_GO']")

    go_button$isElementDisplayed()
    go_button$clickElement()

  }

choose_compliance_year <-
  function(my_year) {
    remote_driver$findElement("id", 'P300_COMP_YEAR')$clickElement()

    remote_driver$findElement("xpath", "//option[2]")$clickElement()

    # 2022
    # remote_driver$findElement("xpath", "//option[3]")$clickElement()

    # click search to load the year
    remote_driver$findElement("xpath", "//*[@id='B717215823519513218']")$clickElement()
  }

# download ----
get_all_buttons <-
  function() {
    buttons_elements <-
      remote_driver$findElements(using = "tag name",
                                 value = "button")
    return(buttons_elements)
  }

# buttons_elements <- get_all_buttons()
# map(buttons_elements, ~ print(.x$getElementText()))

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
      download_button2 <- element
    }
  }

  download_button2$isElementDisplayed()

  download_button2$clickElement()
  download_start_time <- Sys.time()
  return(download_start_time)
}

# download_start_time <- download_table()

## find the downloaded file ----

file_name_pattern <- "Permit"
find_the_downloaded_file <-
  function(file_name_pattern) {
    download_folder <- file.path(r"(~\..\Downloads)")

    downloaded_files <-
      list.files(download_folder,
                 full.names = T,
                 pattern = file_name_pattern)

    # glimpse(downloaded_files)

    files_info <-
      file.info(downloaded_files)

    # glimpse(files_info)

    newest_time <- max(files_info$mtime) |> as.POSIXct(curr_tz)

    if (!newest_time > download_start_time) {
      Sys.sleep(10)
    }

    newest_file_path <-
      files_info |>
      filter(mtime == newest_time) |>
      rownames()

    return(newest_file_path)
  }

read_new_file <- function(file_name_pattern) {
  browser()
  newest_file_path <-
    find_the_downloaded_file(file_name_pattern)

  if (length(newest_file_path) > 0) {

    if (grepl("xlsx$", file_name_pattern))
    {
      my_file_downloaded <-
        read.xlsx(newest_file_path,
                  colNames = TRUE)
    }
    else if (grepl("csv$", file_name_pattern))
    {
      my_file_downloaded <-
        read_csv(newest_file_path)
    }
    else {
      print(str_glue("Don't know how to read {file_name_pattern}"))
    }

    return(my_file_downloaded)
  }
}

# file_name_pattern = "^FHIER Compliance.*csv"

# file_name_pattern = "^Correspondence.*csv"
# correspondence_from_fhier <- read_new_file(file_name_pattern)
