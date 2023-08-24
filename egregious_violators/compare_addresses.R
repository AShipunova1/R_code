library(tidyverse)
library(readr)

base_path <- getwd()

correctd_path <- 
  file.path(base_path,
            r"(..\..\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation__2023-01-24_to_2023-08-01_c.csv)")
file.exists(correctd_path)
corrected_csv <-
  readr::read_csv(correctd_path,
                  col_types = cols(.default = 'c'))

# "C:\Users\anna.shipunova\Documents\R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv"

aug_9_res_path <- 
  file.path(base_path,
            r"(../../R_files_local\my_outputs\egregious_violators\corrected_addr\egregious violators for investigation_Detail2-NED.csv)")

file.exists(aug_9_res_path)
# T

aug_9_csv <- readr::read_csv(aug_9_res_path,
                             col_types = cols(.default = 'c'))

