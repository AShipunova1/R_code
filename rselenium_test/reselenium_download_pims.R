# setup ----
require("RSelenium")
# require("rstudioapi")
# require("XML")
require("tidyverse")
require("tictoc")

Sys.setenv(TZ = Sys.timezone())
curr_tz <- Sys.timezone()

start_browser <- function() {
    remDr <- rsDriver(browser = "firefox",
                      chromever = NULL,
                      port = 4444L)

    remote_driver <- remDr[["client"]]

    return(remote_driver)
  }

remote_driver <- start_browser()
# remote_driver$close
# my_year <- "2022"

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

# go to PIMS search ----
remote_driver$navigate("https://appscloud.fisheries.noaa.gov/suite/sites/pims-search")

correct_page_title <-
  (remote_driver$getTitle() == "SEARCH - PIMS Search")
# T

# choose applications

my_text <- "Applications"
appl_xpath <- str_glue("//*[contains(text(), '{my_text}')]")

# tagName("body")).getText()

appl_el <- remote_driver$findElements("xpath", appl_xpath)

appl_el[[1]]$getElementTagName()
appl_el[[2]]$getElementTagName()


# /html/body/div[1]/div[1]/main/div/div/div[1]/div/div/div[1]/div[1]/div/div/div/div[2]/div/p
# <p class="ParagraphText---richtext_paragraph ParagraphText---default_direction ParagraphText---center ParagraphText---inAccentBackground elements---global_p" data-testid="ParagraphText-paragraph">Applications</p>






# go to FHIER ----
login_into_fhier <- function() {



  # TODO: click_ok_button for certificate
  # <input id="loginButton_0" name="callback_2" type="submit" role="button" index="0" value="ACCEPT" class="btn btn-lg btn-primary">


  # click warning page
  warning_page_accept <-
    remote_driver$findElement(using = "id", "loginButton_0")

  warning_page_accept$clickElement()


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

# login_into_fhier()

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
  function(start_date = "01/01/2023",
           end_date = "12/31/2023") {

    start_date <-
      remote_driver$findElement(using = "xpath",
                                value = "//*[@id='P225_START_DT']")

    start_date$clickElement()
    start_date$sendKeysToElement(list(start_date, key = "enter"))

    end_date <-
      remote_driver$findElement(using = "xpath",
                                value = "//*[@id='P225_END_DT']")
    end_date$clickElement()
    end_date$sendKeysToElement(list(end_date, key = "enter"))

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
      download_button <- element
    }
  }

  download_button$isElementDisplayed()

  download_button$clickElement()
  download_start_time <- Sys.time()
  return(download_start_time)
}

## find the downloaded file ----

find_the_downloaded_file <-
  function(file_name_pattern) {
    download_folder <- file.path(r"(~\..\Downloads)")
    # download_folder <- file.path(r"(C:\Users\anna.shipunova\Downloads)")

    downloaded_compl_files <-
      list.files(download_folder,
                 full.names = T,
                 pattern = file_name_pattern)

    # glimpse(downloaded_compl_files)

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

    return(newest_file_path)
  }

# file_name_pattern = "^Correspondence.*csv"
#
# file_name_pattern = "^FHIER Compliance.*csv"

read_file <- function(file_name_pattern) {
  newest_file_path_compl <-
    find_the_downloaded_file(file_name_pattern)

  if (length(newest_file_path_compl) > 0) {
    fhier_file_downloaded_compl <-
      read_csv(newest_file_path_compl)
  }
}

  # file_name_pattern_corr = "^Correspondence.*csv"
  # newest_file_path_corr <-
  #   find_the_downloaded_file(file_name_pattern_corr)
  #
  # if (length(newest_file_path_corr) > 0) {
  #   fhier_file_downloaded_corr <-
  #     read_csv(newest_file_path_corr)
  # }

