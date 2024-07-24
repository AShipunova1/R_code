# This script copies the file onto Google drive
# 0) Setup paths
# 1) Find if a file with this name exists on Google Drive
# 2) Save the existing one with a different name
# 3) Write the new one

library(tidyverse)
library(tools)

# Set up ----
#' Manually change values
#' Hard coded Google drive folder names, manually change here if changing in Google drive.
egr_violators_googledrive_folder_name <- "Egregious violators"
output_egr_violators_googledrive_folder_name <- "Code"
# Define the current result Google Sheets name
current_google_doc_name <- "egregious_violators_start.qmd"

## Set up Google Drive paths ----

#'
#' Get the path to the main Egregious violators folder on Google Drive
#' 
#' It is used to read the previous result and for saving the new result.
#' 
#' When asked for the authentication the first time choose the appropriate option and follow the instructions. 
#' 
#' If there is an option with your google email account (like your.name@noaa.gov) you can choose that option (usually #2) and it will confirm your access automatically.
#'
#' `n_max = 1` means we will use the first result, assuming we have only one folder with such name on Google dive.
#'

egr_violators_googledrive_folder_path <-
  googledrive::drive_find(pattern =
                            egr_violators_googledrive_folder_name,
                          type = "folder",
                          n_max = 1)
#'
#' Get the path to the output folder within the Egregious violators folder on Google Drive
#'
#' Explainations for the following code:
#'
#' - `googledrive::drive_ls(...)` lists the contents of a Google Drive folder based on the specified parameters:
#'
#'   - `path = googledrive::as_id(egr_violators_googledrive_folder_path)` specifies the path to the folder using its ID:
#'
#'   - `type = "folder"` specifies that only folders should be listed.
#'
#'   - `n_max = 1` specifies that only the first matching folder should be returned.
#'   

output_egr_violators_googledrive_folder_path <-
  googledrive::drive_ls(
    path = googledrive::as_id(egr_violators_googledrive_folder_path),
    pattern = output_egr_violators_googledrive_folder_name,
    type = "folder",
    n_max = 1
  )

# write_res_to_google_sheets <-
#   function() {

# my_current_google_doc contains information about the existing file
my_current_google_doc <-
  googledrive::drive_ls(
    path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
    pattern = current_google_doc_name,
    n_max = 1
  )

# An example of my_current_google_doc:
#   name                        id                                           drive_resource
# 1 egregious_violators_start.qmd ...--o6BpLWpb4-... <named list [36]>

# Get my_current_google_doc created date
# View(my_current_google_doc$drive_resource)
my_current_google_doc_more <-
  my_current_google_doc |>
  mutate(
    modified = map_chr(drive_resource, "modifiedTime"),
    created = map_chr(drive_resource, "createdTime")
  )

# Make a name to rename to
previous_current_google_doc_name <- 
  str_glue("{current_google_doc_name}_{my_current_google_doc_more$modified}")


# Next:
# Save a copy by renaming it
# Rename the file from "current" to the previous_current_google_doc_name with the previous date.
# In case of an error print the message and keep going.
# If there is a file with this name this code will create another one with the same name.

tryCatch({
  message("Try to rename the file")
  
  googledrive::drive_mv(
    my_current_google_doc,
    path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
    name = previous_current_google_doc_name
  )
  # E.g.
  # Original file:
  # • Egregious Violators Current
  # Has been renamed:
  # • output/egregious_violators_to_investigate_2024-06-18
  
}, error = function(cond) {
  message(paste(
    "Failed to rename this file: ",
    previous_current_google_doc_name
  ))
  
  message("Here's the original error message:")
  message(conditionMessage(cond))
  # Choose a return value in case of error
}, warning = function(cond) {
  
}, finally = {
  # message("Some other message at the end")
})

# Create a new empty spreadsheet in the Google Drive output folder to replace the renamed one
# And save its properties into current_google_doc_name_info
# In case of an error print the message and keep going.
# If there is a file with this name this code will create another one with the same name.

tryCatch({
  message("Try to create a new file")
  
  current_google_doc_name_info <-
    googledrive::drive_create(
      name = current_google_doc_name,
      path = googledrive::as_id(output_egr_violators_googledrive_folder_path),
      type = "spreadsheet"
    )
  
}, error = function(cond) {
  message(paste("Failed to create a new file: ", current_google_doc_name))
  
  message("Here's the original error message:")
  message(conditionMessage(cond))
  # Choose a return value in case of error
}, warning = function(cond) {
  
}, finally = {
  # message("Some other message at the end")
})

# Write our results into the newly created spreadsheet "Egregious Violators Current"
# into a sheet/tab with a name defined in out_file_basename
googlesheets4::write_sheet(
  compl_corr_to_investigation_short_dup_marked__permit_region__add_columns,
  ss = current_google_doc_name_info,
  sheet = out_file_basename
)

# See sheets/tabs to check
googlesheets4::sheet_properties(ss = current_google_doc_name_info)

# Remove the empty Sheet1 created automatically by googledrive::drive_create()
googlesheets4::sheet_delete(ss = current_google_doc_name_info, "Sheet1")

# Check the existing tabs again
googlesheets4::sheet_properties(ss = current_google_doc_name_info)$name
# Should be only one name now, like
# [1] "egregious_violators_to_investigate_2024-07-15"

# See in browser to check
googledrive::drive_browse(current_google_doc_name_info)

# Generate a shareable link for the new spreadsheet
current_output_file_link <- googledrive::drive_link(current_google_doc_name_info)

auxfunctions::pretty_print(current_output_file_link, "Link to the new spreadsheet:")

# The function returns the current output file link
return(current_output_file_link)

# }
