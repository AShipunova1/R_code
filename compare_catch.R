# we will need to start looking at catch in comparison to the validation survey and to what MRIP is reporting. I think the 3 of us can start working on this together, but for now - Anna if you can create some code to pull catch by species. So, you'd need to ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels). Then the total caught (numbers) for each unique species. THen we will move to comparing it to the Validation survey data and MRIP. I have validation survey data outputs, that I have not touched yet - no idea the format. I don't know how to get MRIP estimates, but we can figure it out together.

# All logbook data can be downloaded using FHIER (sometimes lags out), ROracle or SQL developer. I actually just pulled the logbook data yesterday, with ROracle, for a data request. I just put it in the google folder here (https://drive.google.com/drive/folders/1iBSiEt30DkTrzzijOb6IuZwQ1r5zvqhs), where I created some new code for the economist who requested an analysis. It's a big file, so it may not show up in there until later today or tomorrow.

# MRIP
# https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\compare_catch\FHIER_all_logbook_data.csv"
# ReportsSAFIS Catches Extended

source("~/R_code_github/useful_functions_module.r")

## ---- set up ----
my_paths <- set_work_dir()

csv_names_list <- c("compare_catch/FHIER_all_logbook_data.csv",
                "compare_catch/mrip_estim_catch_2022_2022/species_list.csv")
temp_var <- load_csv_names(my_paths, csv_names_list)
logbooks <- temp_var[[1]]
mrip_estimate <- temp_var[[2]]

str(logbooks)
str(mrip_estimate)

# ---- the breath of species caught in all logbooks (2022+) ----
# ?? Do this by region (gulf vs s atl vessels).
# ---- Then the total caught (numbers) for each unique species. ----

## ---- to get permit info use compliance reports for now ----

csv_names_list_21_23 = c("Correspondence__2_24_23.csv",
                         "FHIER_Compliance_22__02_24_23.csv",
                         "FHIER_Compliance_23__02_24_23.csv",
                         "FHIER_Compliance_21__03_01_23.csv")

## ---- get csv data into variables ----
temp_var <- get_compl_and_corresp_data(my_paths, csv_names_list_21_23)
compl_clean <- temp_var[[1]]

## check if all the vessels from the logbooks are in compl_clean

compl_ids <-
  compl_clean %>% select(vesselofficialnumber) %>% unique()

logbooks_ids <-
  logbooks  %>% select(VESSEL_OFFICIAL_NBR) %>% unique()

# check if there is permit info in logbooks
# permit_names <- grep("permit", names(logbooks), value = T, ignore.case = T)
#
# logbooks %>% select(as.factor(permit_names)) %>% unique() %>% str()

dim(logbooks_ids)
setdiff(logbooks_ids$VESSEL_OFFICIAL_NBR, compl_ids$vesselofficialnumber) %>%
  cat(sep = ', ')
  # str()
# 6

## ---- get the permit info ----

permit_info <-
  compl_clean %>% 
  filter(vesselofficialnumber %in% logbooks_ids$VESSEL_OFFICIAL_NBR) %>%
    select(vesselofficialnumber, permitgroup) %>% unique()

str(permit_info)
# 'data.frame':	1861 obs. of  2 variables:
  