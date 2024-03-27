# setup ----
require("RSelenium")
require("rstudioapi")
require("XML")
require("tidyverse")
require("tictoc")

remDr <- rsDriver(browser = "firefox",
               chromever = NULL,
               port = 4444L)

remote_driver <- remDr[["client"]]

my_year <- "2022"

# go to FHIER ----
remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

fhier_username <- rstudioapi::askForSecret("fhier_username")
fhier_password <- rstudioapi::askForSecret("fhier_password")

remote_driver$getTitle() == "South East For-hire Electronic Reporting program"
# T

# login ----
P101_USERNAME <- remote_driver$findElement(using = "id", "P101_USERNAME")
P101_USERNAME$clearElement()
P101_USERNAME$sendKeysToElement(list(fhier_username))
# P101_USERNAME$sendKeysToElement(list(rstudioapi::askForSecret("fhier_username")))

P101_PASSWORD <- remote_driver$findElement(using = "id", "P101_PASSWORD")
P101_PASSWORD$clearElement()
P101_PASSWORD$sendKeysToElement(list(fhier_password))
# P101_PASSWORD$sendKeysToElement(list(rstudioapi::askForSecret("fhier_password")))

login_button <-
  remote_driver$findElement(using = "id",
                            value = "B705187520008063447")

login_button$clickElement()

# # open FHIER Correspondence ----
# /html/body/form/div[1]/div[1]/div/ul/li[4]/div[2]/a
menu_links <-
  remote_driver$findElements(using = "tag name",
                            value = "a")

# View(menu_links)
for (element in menu_links) {
  if (element$getElementText() == "Correspondence") {
    menu_correspondence <- element
  }
}

menu_correspondence$clickElement()

# fill dates ----
start_date <-
  remote_driver$findElement(using = "xpath",
                            value = "//*[@id='P225_START_DT']")

start_date$clickElement()
start_date$sendKeysToElement(list("01/01/2023", key = "enter"))

end_date <-
    remote_driver$findElement(using = "xpath",
                            value = "//*[@id='P225_END_DT']")
end_date$clickElement()
end_date$sendKeysToElement(list("12/31/2023", key = "enter"))

# /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div/div/div[5]/button/span[2]
# <button class="t-Button t-Button--icon t-Button--iconLeft t-Button--large t-Button--hot" onclick="apex.submit({request:'P225_GO'});" type="button" id="P225_GO"><span class="t-Icon t-Icon--left " aria-hidden="true"></span><span class="t-Button-label">Go</span><span class="t-Icon t-Icon--right " aria-hidden="true"></span></button>

go_button <-
  remote_driver$findElement(using = "xpath",
                            value = "//*[@id='P225_GO']")

# go_button$isElementDisplayed()
go_button$clickElement()

download_by_action_button()
# Selenium message:Unable to locate element: //button[@id='B717215823519513218']

# ## choose year ----
# # <select id="P300_COMP_YEAR" name="P300_COMP_YEAR" class="selectlist apex-item-select" style="font-family:monospace; font-size:12px" size="1"><option value="2024" selected="selected">2024</option>
# # <option value="2023">2023</option>
# # <option value="2022">2022</option>
# # </select>
# remote_driver$findElement("id", 'P300_COMP_YEAR')$clickElement()
#
# # /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div[1]/div[2]/div[1]/div/div[2]/select/option[2]
# # 2023
# remote_driver$findElement("xpath", "//option[2]")$clickElement()
#
# # 2022
# # remote_driver$findElement("xpath", "//option[3]")$clickElement()
#
# # click search to load the year
# remote_driver$findElement("xpath", "//*[@id='B717215823519513218']")$clickElement()
#
# # download ----
#
# action_button <-
#   remote_driver$findElement(using = "xpath",
#                              "//button[@id='B717215823519513218']")
#
# action_button$clickElement()
#
# action_button_menu <-
#   remote_driver$findElement(using = "xpath",
#                             "//button[@id='R717219435042513225_actions_button']/span")
#
# action_button_menu$clickElement()
#
# action_menu_download <-
#   remote_driver$findElement(using = "xpath",
#                             value = "//*[@id='R717219435042513225_actions_menu_14i']")
#
# # to be sure it is loaded
# action_menu_download$isElementDisplayed()
# action_menu_download$clickElement()
#
# download_button <-
#     remote_driver$findElement(using = "xpath",
#                               value = "/html/body/div[5]/div[3]/div/button[2]")
#
# download_button$isElementDisplayed()
# download_button$clickElement()
#
# # read_in ----
# ## find the downloaded file ----
# download_folder <- file.path(r"(C:\Users\anna.shipunova\Downloads)")
#
# downloaded_compl_files <-
#   list.files(download_folder,
#              full.names = T,
#              pattern = "^FHIER Compliance.*csv")
#
# # glimpse(downloaded_compl_files)
#
# files_info <-
#   file.info(downloaded_compl_files)
#
# newest_time <- max(files_info$mtime)
#
# newest_file_path <-
#   files_info |>
#   filter(mtime == newest_time) |>
#   rownames()
#
# fhier_compl_downloaded <-
#   read_csv(newest_file_path)
