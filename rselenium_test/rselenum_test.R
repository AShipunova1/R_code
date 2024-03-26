# setup ----
library(RSelenium)
require("rstudioapi")

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

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

# username = keyring::key_list("SECPR")[1, 2]
# password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2])

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

# //*[@id="B705187520008063447"]
# <button class="t-Button t-Button--icon t-Button--iconLeft t-Button--hot" onclick="apex.submit({request:'LOGIN'});" type="button" id="B705187520008063447"><span class="t-Icon t-Icon--left fa fa-sign-in" aria-hidden="true"></span><span class="t-Button-label">Log In</span><span class="t-Icon t-Icon--right fa fa-sign-in" aria-hidden="true"></span></button>

# open compliance ----

# <a tabindex="0" role="treeitem" class="a-TreeView-label" href="f?p=162:6:15020351378971:::::" aria-level="1" aria-selected="true">Reports</a>

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

all_trs <-
    remote_driver$findElements(using = "tag name",
                               value = "tr")

class(all_trs)

# my_el <-
  all_trs[[2]]$getElementValueOfCssProperty("xpath")

# $findElements(using = "xpath",
#                        value = "td:nth-child(2)")

tag_name = "td"
element_text = "FHIER COMPLIANCE REPORT"


# compl_report_element$getElementValueOfCssProperty("id")

# parent = compl_report_element$findElement(using = "xpath",
                                          # value = "./parent::*")

# tds = remote_driver$findElement(using = "xpath",
#                                 value = "//table/body/tr/td")

# /html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]
# <td headers="LINK B706409315579859069_3"><a href="f?p=162:300:13568749866649:::RP,300,RIR::&amp;cs=3Fsanj_sz2sv87weceHdu2cw8slIPTBPXz1OEElwmHSh5d1Z-ug2CUJQgWys3QTYkzwmqM8nD-18rgfcQRge0aA"><img src="https://static.oracle.com/cdn/apex/20.2.0.00.20/magnifying_glass_white_bg.gif" alt=""></a></td>
for (one_tr in all_trs) {
  browser()

  my_td <-
    one_tr$findElement(using = "tag name",
                       value = tag_name)
  if (length(my_td) > 0) {
    my_link <- one_tr$findElement(using = "xpath",
                                  value = ".//td[1]/*")

    # tds = driver.find_element_by_xpath("//table/tr/td")
    # for td in tds:print(td.text)

    my_link$getElementSize()
    # my_link$click()
    my_link$clickElement()
  }
  # print(one_td$ )
}

# session.find_element('xpath', './/a[contains(@href, "student")]').text

# /html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]/a
my_td = remote_driver$findElement(using = "xpath",
                                value = "/html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]"
)

my_td$clickElement()

# While (iterate over row)
#      While(Iterate over column)
#            if(column.Text=='YOUR_MATCH'){
#              int voila=column.Index
#            }
#     }
# }

# <td class=" u-tL" headers="C706408587173859069 B706409315579859069_3">FHIER COMPLIANCE REPORT</td>
#\37 06407713552859068 > tbody:nth-child(1) > tr:nth-child(16) > td:nth-child(2)
# html.js.flexboxlegacy.no-touch body#t_PageBody.t-PageBody.t-PageBody--hideLeft.t-PageBody--hideActions.apex-side-nav.apex-icons-fontapex.apex-theme-vita-copy.t-PageBody--leftNav.js-navCollapsed form#wwvFlowForm div.t-Body div.t-Body-main div#t_Body_content.t-Body-content div.t-Body-contentInner div.container div.row div.col.col-12.apex-col-auto div#R706407619556859068.t-IRR-region.js-apex-region div#R706407619556859068_ir.a-IRR-container div#R706407619556859068_worksheet_region.a-IRR div#R706407619556859068_full_view.a-IRR-fullView div#R706407619556859068_content.a-IRR-content div#R706407619556859068_data_panel.a-IRR-reportView div.a-IRR-tableContainer table#706407713552859068.a-IRR-table tbody tr td.u-tL
# /html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[2]

#\37 06407713552859068 > tbody:nth-child(1) > tr:nth-child(16) > td:nth-child(1)
# /html/body/form/div[1]/div[2]/div[2]/div/div/div/div/div/div/div/div[2]/div[2]/div[6]/div[1]/table/tbody/tr[16]/td[1]
# <a href="f?p=162:300:15020351378971:::RP,300,RIR::&amp;cs=3bSTKETmKnEGrZGNPS7tQ_npWoFU1fuSwSfLkj_QAkZJwitfwrK_QvMziXxvddh_0VzFcUAuZS7LuZb4ZT8zRRw"><img src="https://static.oracle.com/cdn/apex/20.2.0.00.20/magnifying_glass_white_bg.gif" alt=""></a>
# //div[text()='FHIER COMPLIANCE REPORT']

compl_report <-
  remote_driver$findElement(using = "link text",
                            value = "FHIER COMPLIANCE REPORT")
# webElem$getElementAttribute("href")


# webElem4$sendKeysToElement(list(key="down_arrow", key="down_arrow", key="down_arrow",
#  key="enter"))
