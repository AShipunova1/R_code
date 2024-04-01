source("reselenium_download.R")

# pims search
# go past warning
# login
# go through combinations of application status
# //*[@id="37099fe221b530cddc0f9d1b0260154d_value_span"]
# and beginning of application number
# //*[@id="fa6d6d69431b1c7cbd9b5b14252ee01c"]
# check if there are few enough rows to download (check the button is not grayed out)
# download


# go to PIMS search ----
remote_driver$navigate("https://appscloud.fisheries.noaa.gov/suite/sites/pims-search")

# TODO: login
# click ok
# click accept
# active_el <- remote_driver$getActiveElement()
# active_el$getElementTagName()

correct_page_title <-
  (remote_driver$getTitle() == "SEARCH - PIMS Search")
correct_page_title
# T

# choose applications
my_text <- "Applications"
appl_xpath <- str_glue("//p[contains(text(), '{my_text}')]")

Sys.sleep(5)
appl_el <- remote_driver$findElement("xpath",
                                     appl_xpath)

# appl_el$getElementTagName()
# p
appl_el$clickElement()
Sys.sleep(5)

# download_button <-
#   remote_driver$findElement(
#     "xpath",
#     "//span[contains(text(), 'Export to Excel - Exports the currently filtered list of records as an Excel file.')]"
#   )
#
# download_button$clickElement()
#
# download_start_time <- Sys.time()
#
# download_start_time <- download_table()

# applications status ----

# add wait and check

applications_status_menu <-
  remote_driver$findElement("xpath",
"//div[2]/div/div/div[2]/div[2]/div/div/div/div/div/div[2]/div/div/div")

applications_status_menu$clickElement()

applications_status_input <-
  remote_driver$findElement("xpath",
                            "//div[6]/div/div/div/div/input")

# applications_status_input$sendKeysToElement(list(key = 'down_arrow'))
# applications_status_input$sendKeysToElement(list(key = 'enter'))
# applications_status_input$sendKeysToElement(list(key = 'escape'))

# applications_status_input$getClass()
#
# applications_status_input$getElementAttribute("text")
# applications_status_input$getElementTagName()
# [1] "input"

# applications_status_input$getElementText()
# applications_status_input$getElementValueOfCssProperty("class")
# applications_status_input$getWindowSize()
# applications_status_input$getElementAttribute("text")

# see the page source
# rr <- remote_driver$getPageSource()
# glimpse(rr)
# XML::htmlParse(rr[[1]])

# <li id="4867239fd45b39d95022a6b343ebac8c_list_0" role="option" class="MenuWidgetItem---active MenuWidgetItem---default_direction MenuWidgetItem---active_not_selected" tabindex="-1"><div>Application Expired</div></li>


applications_status_uls <-
  remote_driver$findElements("tag name", "ul")

# length(applications_status_uls)
# 1

ul_li_list <- applications_status_uls[[1]]$findChildElements("tag name", "li")
length(ul_li_list)
# 19

el_text <- ul_li_list[[3]]$getElementText()
# [1] "Application Started"

all_li_texts <-
  map(ul_li_list, \(el)
      el$getElementText())

glimpse(all_li_texts)

# applications_status_input$getElementText()

map(ul_li_list, \(el) {
  browser()
  el_text <- el$getElementText()
  el$clickElement()
  applications_status_input$sendKeysToElement(list(key = 'escape'))
  # check if loaded
  app_status_cell1 <- remote_driver$findElement(
    "xpath",
    "/html/body/div[1]/div[1]/main/div/div/div[1]/div/div/div[2]/div[2]/div/div[2]/div[1]/table/tbody/tr[1]/td[3]/div/p"
  )
  trimws(app_status_cell1$getElementText()[[1]]) == el_text[[1]]
    # "Application Ready"


  })

# while (length(applications_status_menu) == 0) {
#   Sys.sleep(5)
#   applications_status_menu <-
#     remote_driver$findElements("class",
#                                "MultipleDropdownWidget---dropdown_caret")
#
# }

# applications_status_menu |>
  # map(~.x$getElementTagName())
# svg

# getAttribute("aria-expanded")
applications_status_menu[[1]]$clickElement()
# svg

# menu elements from application status -----
applications_status_menu_elements <-
  remote_driver$findElements("class",
"MultipleDropdownWidget---dropdown_value")

applications_status_menu_elements |> length()
# class="MultipleDropdownWidget---dropdown_value
# aria-activedescendant=""

applications_status_menu_elements[[1]]$getElementAttribute("aria-activedescendant")

applications_status_menu_elements[[1]]$findElements("xpath",                                                         "aria-activedescendant")

# aa <- remote_driver$screenshot()

# glimpse(aa)

# class="MultipleDropdownWidget---dropdown_caret"
# element = driver.findElement(sectionheader);
# if (!element.getAttribute("aria-expanded").equals("true")) {
#     element.click();
# }

# applications_status_menu1 <- remote_driver$findElement("xpath",
# "//*[contains('Application Status')]")

applications_status_menu[[1]]$getElementTagName()
# svg
# applications_status_menu[[1]]$getElementValueOfCssProperty("id")
# applications_status_menu[[1]]$getElementValueOfCssProperty("class")
# [1] "APPLICATION STATUS\nAny"

applications_status_menu[[1]]$findChildElements("xpath",
                                                "//*[@id='P225_START_DT']")

outer_html1 <- applications_status_menu[[1]]$getElementAttribute("outerHTML")

outer_html2 <- applications_status_menu[[2]]$getElementAttribute("outerHTML")

outer_html2
table <- read_html(applications_status_menu[[1]]$getElementAttribute("outerHTML")[[1]]) # get html
# And use rvest to extract the lines of the table:

meta_html <- table |> html_nodes(tag = "//tr[@class='base ng-scope']")

table_text <- html_text(table)



# table %>% html_elements()
  # html_nodes(xpath = "//tr[@class='base ng-scope']")

applications_status_menu[[1]]$highlightElement()

# "aria-activedescendant"
# read the page with menu ----
page_with_menu <- remote_driver$getPageSource()[[1]]
doc <- xml2::read_html(page_with_menu)

# page_with_menu <- htmlParse(remote_driver$getPageSource()[[1]])

glimpse(doc)

page_with_menu_1 <-
  page_with_menu |> str_split("><")

glimpse(page_with_menu_1)

# bodyText = remote_driver('body').text

grep("Closed", page_with_menu, value = T) |> glimpse()

# <div class="MultipleDropdownWidget---dropdown_value MultipleDropdownWidget---inSideBySideItem MultipleDropdownWidget---has_inline_label" id="c0ef4b68cec0347a3f2ab5421f02fa7d_value" role="combobox" aria-autocomplete="list" tabindex="0" aria-controls="c0ef4b68cec0347a3f2ab5421f02fa7d_list" aria-expanded="false" aria-activedescendant="" aria-labelledby="c0ef4b68cec0347a3f2ab5421f02fa7d_inlineLabel" aria-describedby="c0ef4b68cec0347a3f2ab5421f02fa7d_placeholder"><span><span class="MultipleDropdownWidget---inline_label" aria-hidden="true" id="c0ef4b68cec0347a3f2ab5421f02fa7d_inlineLabel">Application Status</span><span id="c0ef4b68cec0347a3f2ab5421f02fa7d_placeholder" class="MultipleDropdownWidget---accessibilityhidden">Any</span><span id="c0ef4b68cec0347a3f2ab5421f02fa7d_value_span" class="MultipleDropdownWidget---value_display" data-placeholder="Any"></span><svg focusable="false" tabindex="-1" class="MultipleDropdownWidget---dropdown_caret" width="320" height="512" viewBox="0 0 320 512" xmlns="http://www.w3.org/2000/svg" aria-hidden="true"><path d="M31.3 192h257.3c17.8 0 26.7 21.5 14.1 34.1L174.1 354.8c-7.8 7.8-20.5 7.8-28.3 0L17.2 226.1C4.6 213.5 13.5 192 31.3 192z"></path></svg></span></div>

# map(1:16,
#     \(curr_num) {
#       # setElementAttribute(attributeName, value)
#
#       applications_status_menu$setElementAttribute("aria-activedescendant", "37099fe221b530cddc0f9d1b0260154d_list_3")
#     })



# 37099fe221b530cddc0f9d1b0260154d_list_3
# //*[@id="wrapper_37099fe221b530cddc0f9d1b0260154d"]/div
# //*[@id="37099fe221b530cddc0f9d1b0260154d_value"]
# <div class="MultipleDropdownWidget---dropdown_value MultipleDropdownWidget---inSideBySideItem MultipleDropdownWidget---has_inline_label" id="37099fe221b530cddc0f9d1b0260154d_value" role="combobox" aria-autocomplete="list" tabindex="0" aria-controls="37099fe221b530cddc0f9d1b0260154d_list" aria-expanded="false" aria-activedescendant="" aria-labelledby="37099fe221b530cddc0f9d1b0260154d_inlineLabel"><span><span class="MultipleDropdownWidget---inline_label" aria-hidden="true" id="37099fe221b530cddc0f9d1b0260154d_inlineLabel">Application Status</span><span id="37099fe221b530cddc0f9d1b0260154d_value_span" class="MultipleDropdownWidget---value_display" data-placeholder="Any"></span><svg focusable="false" tabindex="-1" class="MultipleDropdownWidget---dropdown_caret" width="320" height="512" viewBox="0 0 320 512" xmlns="http://www.w3.org/2000/svg" aria-hidden="true"><path d="M31.3 192h257.3c17.8 0 26.7 21.5 14.1 34.1L174.1 354.8c-7.8 7.8-20.5 7.8-28.3 0L17.2 226.1C4.6 213.5 13.5 192 31.3 192z"></path></svg></span></div>



# # go to FHIER
# login_into_fhier <- function() {
#
#
#
#   # TODO: click_ok_button for certificate
#   # <input id="loginButton_0" name="callback_2" type="submit" role="button" index="0" value="ACCEPT" class="btn btn-lg btn-primary">
#
#
#   # click warning page
#   warning_page_accept <-
#     remote_driver$findElement(using = "id", "loginButton_0")
#
#   warning_page_accept$clickElement()
#
#
#   # login
#   P101_USERNAME <-
#     remote_driver$findElement(using = "id", "P101_USERNAME")
#   P101_USERNAME$clearElement()
#   P101_USERNAME$sendKeysToElement(list(rstudioapi::askForSecret("fhier_username")))
#
#   P101_PASSWORD <-
#     remote_driver$findElement(using = "id", "P101_PASSWORD")
#   P101_PASSWORD$clearElement()
#   P101_PASSWORD$sendKeysToElement(list(rstudioapi::askForSecret("fhier_password")))
#
#   login_button <-
#     remote_driver$findElement(using = "id",
#                               value = "B705187520008063447")
#
#   login_button$clickElement()
# }
#
# # login_into_fhier()
#
# open_menu_item <- function(page_name) {
#
#   menu_links <-
#     remote_driver$findElements(using = "tag name",
#                                value = "a")
#
#   # View(menu_links)
#   for (element in menu_links) {
#     if (element$getElementText() == page_name) {
#       menu_report <- element
#     }
#   }
#
#   menu_report$clickElement()
# }
#
# # open_menu_item("Reports")
#
# # element_text = "FHIER COMPLIANCE REPORT"
#
# choose_correspondence_dates <-
#   function(start_date = "01/01/2023",
#            end_date = "12/31/2023") {
#
#     start_date <-
#       remote_driver$findElement(using = "xpath",
#                                 value = "//*[@id='P225_START_DT']")
#
#     start_date$clickElement()
#     start_date$sendKeysToElement(list(start_date, key = "enter"))
#
#     end_date <-
#       remote_driver$findElement(using = "xpath",
#                                 value = "//*[@id='P225_END_DT']")
#     end_date$clickElement()
#     end_date$sendKeysToElement(list(end_date, key = "enter"))
#
#     go_button <-
#       remote_driver$findElement(using = "xpath",
#                                 value = "//*[@id='P225_GO']")
#
#     go_button$isElementDisplayed()
#     go_button$clickElement()
#
#   }
#
# choose_compliance_year <-
#   function(my_year) {
#     remote_driver$findElement("id", 'P300_COMP_YEAR')$clickElement()
#
#     remote_driver$findElement("xpath", "//option[2]")$clickElement()
#
#     # 2022
#     # remote_driver$findElement("xpath", "//option[3]")$clickElement()
#
#     # click search to load the year
#     remote_driver$findElement("xpath", "//*[@id='B717215823519513218']")$clickElement()
#   }
#
# # download
# get_all_buttons <-
#   function() {
#     buttons_elements <-
#       remote_driver$findElements(using = "tag name",
#                                  value = "button")
#     return(buttons_elements)
#   }
#
# # buttons_elements <- get_all_buttons()
# # map(buttons_elements, ~ print(.x$getElementText()))
#
# download_table <- function() {
#   # there 3 layers, so we need to find buttons 3 time to load the last Download button
#
#   # 1
#   buttons_elements <- get_all_buttons()
#   for (element in buttons_elements) {
#     if (element$getElementText() == "Actions") {
#       action_button <- element
#     }
#   }
#
#   action_button$clickElement()
#
#   # 2
#   buttons_elements <- get_all_buttons()
#   # map(buttons_elements, ~ print(.x$getElementText()))
#   for (element in buttons_elements) {
#     if (element$getElementText() == "Download") {
#       download_button1 <- element
#     }
#   }
#
#   download_button1$isElementDisplayed()
#
#   download_button1$clickElement()
#
#   # 3
#   buttons_elements <- get_all_buttons()
#   # map(buttons_elements, ~ print(.x$getElementText()))
#   for (element in buttons_elements) {
#     if (element$getElementText() == "Download") {
#       download_button <- element
#     }
#   }
#
#   download_button$isElementDisplayed()
#
#   download_button$clickElement()
#   download_start_time <- Sys.time()
#   return(download_start_time)
# }
#
# ## find the downloaded file
#
# find_the_downloaded_file <-
#   function(file_name_pattern) {
#     download_folder <- file.path(r"(~\..\Downloads)")
#     # download_folder <- file.path(r"(C:\Users\anna.shipunova\Downloads)")
#
#     downloaded_compl_files <-
#       list.files(download_folder,
#                  full.names = T,
#                  pattern = file_name_pattern)
#
#     # glimpse(downloaded_compl_files)
#
#     files_info <-
#       file.info(downloaded_compl_files)
#
#     # View(files_info)
#
#     # newest_time <- max(files_info$mtime) |> as.POSIXct(curr_tz)
#     # download_start_time |> as.POSIXct(curr_tz)
#
#     if (!newest_time > download_start_time) {
#       Sys.sleep(10)
#     }
#
#     newest_file_path <-
#       files_info |>
#       filter(mtime == newest_time) |>
#       rownames()
#
#     return(newest_file_path)
#   }
#
# # file_name_pattern = "^Correspondence.*csv"
# #
# # file_name_pattern = "^FHIER Compliance.*csv"
#
# read_file <- function(file_name_pattern) {
#   newest_file_path_compl <-
#     find_the_downloaded_file(file_name_pattern)
#
#   if (length(newest_file_path_compl) > 0) {
#     fhier_file_downloaded_compl <-
#       read_csv(newest_file_path_compl)
#   }
# }
#
#   # file_name_pattern_corr = "^Correspondence.*csv"
#   # newest_file_path_corr <-
#   #   find_the_downloaded_file(file_name_pattern_corr)
#   #
#   # if (length(newest_file_path_corr) > 0) {
#   #   fhier_file_downloaded_corr <-
#   #     read_csv(newest_file_path_corr)
#   # }
#
