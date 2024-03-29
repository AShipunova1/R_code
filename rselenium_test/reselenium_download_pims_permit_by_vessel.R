library('XML')
source("reselenium_download_fhier.R")

# go to PIMS search ----
remote_driver$navigate("https://appscloud.fisheries.noaa.gov/suite/sites/pims-search")

correct_page_title <-
  (remote_driver$getTitle() == "SEARCH - PIMS Search")
correct_page_title
# T

# go to Permits ----
Sys.sleep(10)
permits_menu_item <-
  remote_driver$findElement("xpath",
                            "//p[contains(text(), 'Permits')]")

permits_menu_item$clickElement()

# Search by vessel ----
# $ VESSEL_OFFICIAL_NUMBER <chr> "FL8151TE", "1214856", "NC9069EA", "FL9693LY", "92321…
# <input id="14a393859d6e0ef83ad3900b7b3b91fe" type="text" class="TextInput---text TextInput---align_start TextInput---inSideBySideItem TextInput---has_clear_link" placeholder="Search Permits" value="FL8151TE">

# "//*[contains(text(), 'Any')]/../../..")

search_perm1 <-
  remote_driver$findElements("tag name",
                             "input")

search_perm1[[1]]$getElementAttribute("placeholder") == "Search Permits"
# T

search_perm <-
  remote_driver$findElements("xpath",
                             "//*[contains(text(), 'Search Permits')]")

search_perm[[1]]$clickElement()

search_perm[[1]]$getElementTagName()

search_perm[[1]]$getElementAttribute("id")
search_perm[[1]]$
FL8151TE

# get the table ----

doc <- htmlParse(remote_driver$getPageSource()[[1]])
my_table_list <- readHTMLTable(doc)

my_table_0 <- my_table_list[[1]]
my_table_names <- my_table_0 |> names()
# View(tt[[1]])
df <- my_table_0
clean_names <- function(df) {
  names(df) <-
    gsub(' Sortable column, activate to sort ascending', '', names(df))
  names(df) <- str_sub(names(df), 1, nchar(names(df))/2)
  names(df) <- tolower(names(df))
  names(df) <- gsub("\\W", "_", names(df))
  df <- rename(df, request_type = reques)
  df
}

my_table <- clean_names(my_table_0)

# View(my_table)
