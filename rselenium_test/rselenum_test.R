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
remote_driver$navigate("https://grunt.sefsc.noaa.gov/apex/f?p=162:LOGIN_DESKTOP:12001011577015:::::")

# remote_driver$setImplicitWaitTimeout(3000)
# findElement(remDr, using = c("xpath", "css selector", "id", "name", "tag name", "class name", "link text", "partial link text"), value, ...)

username = keyring::key_list("SECPR")[1, 2]
password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2])

fhier_username <- rstudioapi::askForSecret("fhier_username")
fhier_password <- rstudioapi::askForSecret("fhier_password")

# key_set_with_value("R-test-service", "donaldduck", password = "secret",
#                    keyring = "foobar")

keyring::keyring_list()
  key_get("SECPR", keyring::key_list("SECPR")[1, 2])

remote_driver$getTitle()
P101_USERNAME <- remote_driver$findElement(using = "id", "P101_USERNAME")
P101_USERNAME$clearElement()
P101_USERNAME$sendKeysToElement(list(fhier_username))

P101_PASSWORD <- remote_driver$findElement(using = "id", "P101_PASSWORD")
P101_PASSWORD$clearElement()
P101_PASSWORD$sendKeysToElement(list(fhier_password))

login_button <-
  remote_driver$findElement(using = "id",
                            value = "B705187520008063447")

login_button$clickElement()

# //*[@id="B705187520008063447"]
# <button class="t-Button t-Button--icon t-Button--iconLeft t-Button--hot" onclick="apex.submit({request:'LOGIN'});" type="button" id="B705187520008063447"><span class="t-Icon t-Icon--left fa fa-sign-in" aria-hidden="true"></span><span class="t-Button-label">Log In</span><span class="t-Icon t-Icon--right fa fa-sign-in" aria-hidden="true"></span></button>
