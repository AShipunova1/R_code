# explore the SA data for fishing within Gray's Reef National Marine Sanctuary. I'm not sure if we have enough refined data to do this, but it is worth exploring.

# https://graysreef.noaa.gov/about/sanctuary/location-oceanography.html
# Sanctuary Boundaries
# Latitude (DD	Longitude (DD)
# Southwest	31.362732° N	80.921200° W
# Northwest	31.421064° N	80.921200° W
# Northeast	31.421064° N	80.828145° W
# Southeast	31.362732° N	80.828145° W
# Research Area Boundaries
# Latitude (DD	Longitude (DD)
# Southwest	31.362732° N	80.921200° W
# Northwest	31.384444° N	80.921200° W
# Northeast	31.384444° N	80.828145° W
# Southeast	31.362732° N	80.828145° W

# setup ----

# library(zoo) #date manipulations
library(sf) #Create sf object to work with coordinates
library(mapview) #View spatial objects interactively
library(leaflet)
library(tictoc) #benchmarking
# # library(htmlwidgets) # add js script to leaflets
# library(stringi) # add characters
# library(htmltools)

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()
current_project_name <- "gray_s_reef_sanctuary"
fishing_effort_location_project_name <- "fishing_effort_location"

source(
  file.path(
    my_paths$git_r,
    fishing_effort_location_project_name,
    "fishing_effort_locations_get_data.R"
  )
)

source(
  file.path(
    my_paths$git_r,
    current_project_name,
    "prep_safis_efforts_extended_gray_s_reef_sanctuary.R"
  )
)
