# setup ----
library(RSelenium)
require("rstudioapi")
library(XML)

# ?RSelenium
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L,
# browserName = "firefox")

remDr <- rsDriver(browser = "firefox",
               chromever = NULL,
               port = 4444L)

# remDr <- rsDriver()

remote_driver <- remDr[["client"]]

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

# fhier_username <- rstudioapi::askForSecret("fhier_username")
# fhier_password <- rstudioapi::askForSecret("fhier_password")

remote_driver$getTitle()

# login ----
P101_USERNAME <- remote_driver$findElement(using = "id", "P101_USERNAME")
P101_USERNAME$clearElement()
# P101_USERNAME$sendKeysToElement(list(fhier_username))
P101_USERNAME$sendKeysToElement(list(rstudioapi::askForSecret("fhier_username")))

P101_PASSWORD <- remote_driver$findElement(using = "id", "P101_PASSWORD")
P101_PASSWORD$clearElement()
P101_PASSWORD$sendKeysToElement(list(rstudioapi::askForSecret("fhier_password")))

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
# with xpath
# /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div[1]/div[2]/div[1]/div/div[2]/select/option[2]
remote_driver$findElement("id", 'P300_COMP_YEAR')$clickElement()


# $findElement("xpath", "./option[2]")$clickElement()
# remote_driver$findElement("class", '.t-Button-label')$clickElement()

# remDr$findElement('xpath', "//li[. = 'Handel']") solves it.

# remote_driver$findElement("id", 'P300_COMP_YEAR')

# select 2018
# remDr$findElement("xpath", '//*[@id="Body"]/div[1]/ul/li[3]/label')$clickElement() # in [3] is for 2018.

# next you need put cilck in update bottom in the page for load the new filter.
# remDr$findElement("xpath", '//*[@id="dnn_ctr633_Dispatch_Index_btnSubmit"]')$clickElement()

# genderElems <- remDrv$findElements("css", "#Gender option")
# genderElems[[1]]$clickElement()

## click search ----
# /html/body/form/div[1]/div[2]/div[2]/div/div/div[1]/div/div/div[2]/div[2]/div[2]/div/div/div/div/div[2]/div/button/span[1]
# html.js.flexboxlegacy.no-touch body#t_PageBody.t-PageBody.t-PageBody--hideLeft.t-PageBody--hideActions.apex-side-nav.apex-icons-fontapex.apex-theme-vita-copy.js-navExpanded.t-PageBody--leftNav form#wwvFlowForm div.t-Body div.t-Body-main div#t_Body_content.t-Body-content div.t-Body-contentInner div.container div.row div.col.col-12.apex-col-auto div#R717215418394513216.t-Region.t-Region--noPadding.t-Region--accent5.t-Region--scrollBody.t-Form--slimPadding div.t-Region-bodyWrap div.t-Region-body div.container div.row div.col.col-12.apex-col-auto div#R719635242264827707.t-ButtonRegion.t-Form--floatLeft.t-ButtonRegion--slimPadding.t-ButtonRegion--noUI.t-Form--slimPadding div.t-ButtonRegion-wrap div.t-ButtonRegion-col.t-ButtonRegion-col--content div.t-ButtonRegion-buttons button#B717215823519513218.t-Button.t-Button--icon.t-Button--iconLeft.t-Button--hot span.t-Button-label

search_button <-
  remote_driver$findElements(using = "id",
                             'B717215823519513218')
search_button[[1]]$clickElement()

## action button ----
action_button <-
  remote_driver$findElement(using = "xpath",
                             "//button[@id='R717219435042513225_actions_button']/span")

action_button$clickElement()

## download data ----

# download_button <-
#   remote_driver$findElement(using = "xpath",
#                              "//span[contains(.,'Download')]")
#
# download_button$clickElement()

## show all rows ----

# actions_menu R717219435042513225_actions_menu

pages_link <-
  remote_driver$findElement(using = "id",
                            "R717219435042513225_actions_menu_3_0_c9i")

pages_link$clickElement()

pages_link_all <-
  remote_driver$findElement(using = "xpath",
                            "//button[@id='R717219435042513225_actions_button']/span")

pages_link_all$clickElement()


# html.js.flexboxlegacy.no-touch body#t_PageBody.t-PageBody.t-PageBody--hideLeft.t-PageBody--hideActions.apex-side-nav.apex-icons-fontapex.apex-theme-vita-copy.js-navExpanded.t-PageBody--leftNav div#R717219435042513225_actions_menu.a-Menu div.a-Menu-content ul li#R717219435042513225_actions_menu_3.a-Menu-item div#R717219435042513225_actions_menu_3im.a-Menu div.a-Menu-content ul li#R717219435042513225_actions_menu_3_0_c1.a-Menu-item div.a-Menu-inner span.a-Menu-labelContainer button#R717219435042513225_actions_menu_3_0_c1i.a-Menu-label
# //*[@id="R717219435042513225_actions_menu_3_0_c1i"]
# <button type="button" id="R717219435042513225_actions_menu_3_0_c1i" role="menuitemradio" class="a-Menu-label" aria-checked="true">5</button>


doc <- htmlParse(remote_driver$getPageSource()[[1]])
readHTMLTable(doc)
