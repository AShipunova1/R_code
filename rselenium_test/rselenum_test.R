# setup ----
require("RSelenium")
require("rstudioapi")
require("XML")
require("tidyverse")
require("tictoc")
require("parallel")

# ?RSelenium
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L,
# browserName = "firefox")

remDr <- rsDriver(browser = "firefox",
               chromever = NULL,
               port = 4444L)

# remDr <- rsDriver()

remote_driver <- remDr[["client"]]

# check the next page number ----
get_page_numbers <- function() {
  page_numbers <-
    remote_driver$findElement(using = "xpath",
                              value = "//div[@id='R717219435042513225_data_panel']/div[2]/ul/li[2]/span")

  # str(page_numbers)

  next_page_number <-
    page_numbers$getElementText()[[1]] |> str_split_i(" - ", -1)

  return(next_page_number)
}

# go to FHIER ----
remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")

get_one_element <- function(tag_name, element_text) {
  all_tds <-
    remote_driver$findElements(using = "tag name",
                               value = tag_name)

  # View(menu_links)
  for (element in all_tds) {
    if (element$getElementText() == element_text) {
      return(element)
    }
  }
}

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

fhier_username <- rstudioapi::askForSecret("fhier_username")
fhier_password <- rstudioapi::askForSecret("fhier_password")

remote_driver$getTitle()

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

# open compliance ----

menu_links <-
  remote_driver$findElements(using = "tag name",
                            value = "a")

# View(menu_links)
for (element in menu_links) {
  if (element$getElementText() == "Reports") {
    menu_report <- element
  }
}

menu_report$clickElement()

# open FHIER COMPLIANCE REPORT ----

tag_name = "td"
element_text = "FHIER COMPLIANCE REPORT"

my_td = remote_driver$findElement(using = "xpath",
                                  value = "/html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]")

my_td$getElementAttribute("text")

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
remote_driver$findElement("xpath", "//option[3]")$clickElement()

# click search
remote_driver$findElement("xpath", "//*[@id='B717215823519513218']")$clickElement()

## action button ----
action_button <-
  remote_driver$findElement(using = "xpath",
                             "//button[@id='B717215823519513218']")

action_button$clickElement()

action_button_menu <-
  remote_driver$findElement(using = "xpath",
                            "//button[@id='R717219435042513225_actions_button']/span")

action_button_menu$clickElement()

pages_num_el <-
  remote_driver$findElement(using = "xpath",
                            "//*[@id='R717219435042513225_actions_menu_3i']")

pages_num_el$clickElement()

pages_num_el_1000 <-
  remote_driver$findElement(using = "xpath",
                            "//*[@id='R717219435042513225_actions_menu_3_0_c8i']")

get_page_numbers() == "1,000"
# T

## click more pages ----

there_is_more <-
  function() {
    more_pages <-
      remote_driver$findElement(using = "xpath",
                                value = "//div[@id='R717219435042513225_data_panel']/div[2]/ul/li[3]/button/span")

    more_pages$clickElement()

  }

# new_table_2023 <- new_table
# dim(new_table_2023)
# 148502

# same in a loop ----
page_number <- 0
new_table <- tibble()
dim(new_table)

# check_if_loaded <- function(curr_page_num) {
#   browser()
#   new_page_num <- there_is_more()
#   new_page_num == curr_page_num
#
#   # webElem <- NULL
#   # while (is.null(webElem)) {
#   #   webElem <-
#   #     tryCatch({
#   #       remote_driver$findElement(using = 'name', value = "P300_IS_COMP_OVERRIDE")
#   #     },
#   #     error = function(e) {
#   #       NULL
#   #     })
#   #   #loop until element with name <value> is found in <webpage url>
#   # }
# }

# repeat {
#   new_page_number <- get_page_numbers()
#   if (!new_page_number == old_page_number) {
#     # browser()
#     there_is_more()
#     # check_if_loaded()
#
#     tables <- htmlParse(remote_driver$getPageSource()[[1]])
#     table1 <- readHTMLTable(tables, header = T)
#     # dim(table1[[2]])
#     new_table <- rbind(new_table, table1[[2]])
#     # View(new_table)
#     old_page_number <- new_page_number
#   } else {
#     break
#   }
# }

tic("get_all_rows")
old_page_number <- page_number
# first run
new_page_number <- get_page_numbers()
while (new_page_number != old_page_number) {
  # browser()
  if (length(
    remote_driver$findElements(using = "xpath",
                               value = "//div[@id='R717219435042513225_data_panel']/div[2]/ul/li[3]/button/span")
  ) == 0) {
    Sys.sleep(10)
  } else {
    there_is_more()
  }
  # check_if_loaded()
  Sys.sleep(5)
  tables <- htmlParse(remote_driver$getPageSource()[[1]])
  table1 <- readHTMLTable(tables, header = T)
  # dim(table1[[2]])
  new_table <- rbind(new_table, table1[[2]])
  # View(new_table)
  old_page_number <- new_page_number
  new_page_number <- get_page_numbers()
}
toc()
# 2022 get_all_rows: 946.72 sec elapsed
# 15.77867 min

# print out ----
dim(new_table)
# [1] 145048     18
# why less than
# > new_page_number
# [1] "145,524"

# getwd()

write_csv(
  new_table,
  file.path(
    r"(~\R_files_local\my_outputs\rselenium_test)",
    "fhier_compliance_2022.csv"
  )
)

# test ----
fhier_compl_downloaded <-
  read_csv(r"(C:\Users\anna.shipunova\Downloads\FHIER Compliance(1).csv)")

# download try ----

action_button <-
  remote_driver$findElement(using = "xpath",
                             "//button[@id='B717215823519513218']")

action_button$clickElement()

action_button_menu <-
  remote_driver$findElement(using = "xpath",
                            "//button[@id='R717219435042513225_actions_button']/span")

action_button_menu$clickElement()

action_menu_download <-
  remote_driver$findElement(using = "xpath",
                            value = "//*[@id='R717219435042513225_actions_menu_14i']")

action_menu_download$isElementDisplayed()
action_menu_download$clickElement()

download_button <-
    remote_driver$findElement(using = "xpath",
                              value = "/html/body/div[5]/div[3]/div/button[2]")

download_button$isElementDisplayed()
download_button$clickElement()

# ===
n_cores <- detectCores()
# 16
