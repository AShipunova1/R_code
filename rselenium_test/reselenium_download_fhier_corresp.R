# using functions

source("reselenium_download_fhier.R")

remote_driver <- start_browser()
my_year <- "2023"

login_into_fhier()

open_menu_item("Correspondence")

start_date = str_glue("01/01/{my_year}")
end_date = str_glue("12/31/{my_year}")

choose_correspondence_dates(start_date,
                            end_date)


# # setup ----
# require("RSelenium")
# require("rstudioapi")
# require("XML")
# require("tidyverse")
# require("tictoc")
#
# remDr <- rsDriver(browser = "firefox",
#                chromever = NULL,
#                port = 4444L)
#
# remote_driver <- remDr[["client"]]
#
# my_year <- "2022"
#
# # go to FHIER ----
# remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")
#
# # remote_driver$setImplicitWaitTimeout(3000)
# # findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)
#
# fhier_username <- rstudioapi::askForSecret("fhier_username")
# fhier_password <- rstudioapi::askForSecret("fhier_password")
#
# remote_driver$getTitle() == "South East For-hire Electronic Reporting program"
# # T
#
# # login ----
# P101_USERNAME <- remote_driver$findElement(using = "id", "P101_USERNAME")
# P101_USERNAME$clearElement()
# P101_USERNAME$sendKeysToElement(list(fhier_username))
# # P101_USERNAME$sendKeysToElement(list(rstudioapi::askForSecret("fhier_username")))
#
# P101_PASSWORD <- remote_driver$findElement(using = "id", "P101_PASSWORD")
# P101_PASSWORD$clearElement()
# P101_PASSWORD$sendKeysToElement(list(fhier_password))
# # P101_PASSWORD$sendKeysToElement(list(rstudioapi::askForSecret("fhier_password")))
#
# login_button <-
#   remote_driver$findElement(using = "id",
#                             value = "B705187520008063447")
#
# login_button$clickElement()
#
# # # open FHIER Correspondence ----
# # /html/body/form/div[1]/div[1]/div/ul/li[4]/div[2]/a
# menu_links <-
#   remote_driver$findElements(using = "tag name",
#                             value = "a")
#
# # View(menu_links)
# for (element in menu_links) {
#   if (element$getElementText() == "Correspondence") {
#     menu_correspondence <- element
#   }
# }
#
# menu_correspondence$clickElement()
#
# # fill dates ----
# start_date <-
#   remote_driver$findElement(using = "xpath",
#                             value = "//*[@id='P225_START_DT']")
#
# start_date$clickElement()
# start_date$sendKeysToElement(list("01/01/2023", key = "enter"))
#
# end_date <-
#     remote_driver$findElement(using = "xpath",
#                             value = "//*[@id='P225_END_DT']")
# end_date$clickElement()
# end_date$sendKeysToElement(list("12/31/2023", key = "enter"))
#
# # /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div/div/div[5]/button/span[2]
# # <button class="t-Button t-Button--icon t-Button--iconLeft t-Button--large t-Button--hot" onclick="apex.submit({request:'P225_GO'});" type="button" id="P225_GO"><span class="t-Icon t-Icon--left " aria-hidden="true"></span><span class="t-Button-label">Go</span><span class="t-Icon t-Icon--right " aria-hidden="true"></span></button>
#
# go_button <-
#   remote_driver$findElement(using = "xpath",
#                             value = "//*[@id='P225_GO']")
#
# # go_button$isElementDisplayed()
# go_button$clickElement()
#
# # download_by_action_button(action_button_id)
# # Selenium message:Unable to locate element: //button[@id='B717215823519513218']
# # <button id="R728253330585395881_actions_button" class="a-Button a-IRR-button a-IRR-button--actions js-menuButton" type="button" data-menu="R728253330585395881_actions_menu" aria-haspopup="menu" aria-expanded="false">Actions<span class="a-Icon icon-menu-drop-down"></span></button>
# # //*[@id="R728253330585395881_actions_menu_14i"]
# # <button type="button" id="R728253330585395881_actions_menu_14i" role="menuitem" class="a-Menu-label">Download</button>
# #
#
# # download ----
# buttons_elements <-
#   remote_driver$findElements(using = "tag name",
#                              value = "button")
#
# # View(buttons_elements)
# for (element in buttons_elements) {
#   if (element$getElementText() == "Download") {
#     download_button <- element
#   }
# }
#
# download_button$isElementDisplayed()
#
# download_button$clickElement()
# # /html/body/div[6]/div[3]/div/button[2]
# # <button type="button" class="ui-button--hot ui-button ui-corner-all ui-widget">Download</button>
#
#
# # read_in ----
# ## find the downloaded file ----
# download_folder <- file.path(r"(C:\Users\anna.shipunova\Downloads)")
#
# downloaded_corresp_files <-
#   list.files(download_folder,
#              full.names = T,
#              pattern = "^Correspondence.*csv")
#
# # glimpse(downloaded_compl_files)
#
# files_info <-
#   file.info(downloaded_corresp_files)
#
# newest_time <- max(files_info$mtime)
#
# newest_file_path <-
#   files_info |>
#   filter(mtime == newest_time) |>
#   rownames()
#
# fhier_corresp_downloaded <-
#   read_csv(newest_file_path)
#
