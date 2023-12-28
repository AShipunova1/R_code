# states lists ----
south_east_coast_states <- c(
  "Alabama",
  "Florida",
  "Georgia",
  "Louisiana",
  "Mississippi",
  "North Carolina",
  "South Carolina",
  "Texas"
)

south_atlantic_states <-
  c(
    "Maryland",
    "Delaware",
    "West Virginia",
    "Virginia",
    "North Carolina",
    "South Carolina",
    "Georgia",
    "Florida"
  )

east_coast_states <- list(
  gom = c("Alabama",
          "Florida",
          "Louisiana",
          "Mississippi",
          "Texas"),
  sa = c(
    "Connecticut",
    "Delaware",
    "Florida",
    "Georgia",
    "Maine",
    "Maryland",
    "Massachusetts",
    "New Hampshire",
    "New Jersey",
    "New York",
    "North Carolina",
    "Pennsylvania",
    "Rhode Island",
    "South Carolina",
    "Virginia",
    "Washington DC"
  )
)

# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

sa_council_states <-
  c(
    "Florida", # should be separated by county
    "Georgia",
    "North Carolina",
    "South Carolina"
  )

# don't need, see above
gom_council_states <-
  c("Florida",
    "Alabama",
    "Mississippi",
    "Louisiana",
    "Texas")

# Florida counties by region (from the Internet) ----
# NB. "Monroe" is in borh regions
fl_counties <- list(
  "sa" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Monroe",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "gom" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

# prepare state names and abbs ----
# have to save first, to use the original once as names
my_state_abb <- state.abb
my_state_name <- state.name
names(my_state_abb) <- tolower(state.name)
names(my_state_name) <- tolower(state.abb)

# result names
result_names <- c(
  "south_east_coast_states",
  "east_coast_states",
  "sa_council_states",
  "south_atlantic_states",
  "fl_counties",
  "my_state_abb",
  "my_state_name"
)

title_message_print(result_names)
