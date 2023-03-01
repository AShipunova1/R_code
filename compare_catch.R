# we will need to start looking at catch in comparison to the validation survey and to what MRIP is reporting. I think the 3 of us can start working on this together, but for now - Anna if you can create some code to pull catch by species. So, you'd need to ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels). Then the total caught (numbers) for each unique species. THen we will move to comparing it to the Validation survey data and MRIP. I have validation survey data outputs, that I have not touched yet - no idea the format. I don't know how to get MRIP estimates, but we can figure it out together.

# All logbook data can be downloaded using FHIER (sometimes lags out), ROracle or SQL developer. I actually just pulled the logbook data yesterday, with ROracle, for a data request. I just put it in the google folder here (https://drive.google.com/drive/folders/1iBSiEt30DkTrzzijOb6IuZwQ1r5zvqhs), where I created some new code for the economist who requested an analysis. It's a big file, so it may not show up in there until later today or tomorrow.

# MRIP
# https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#
# "C:\Users\anna.shipunova\Documents\R_files_local\my_inputs\compare_catch\FHIER_all_logbook_data.csv"
# Reports/SAFIS Catches Extended

## ---- Workflow ----
## 1) logbooks data
### add permitgroup, scientific name (to use for MSP_CODE)
### separate gulf and sa permits
## 2) MRIP data
### download for region 6 & 7 (GoM by state, SA by state) from https://www.st.nmfs.noaa.gov/SASStoredProcess/do?#
### use SP_CODE, SUB_REG, AREA_X, TOT_CAT
### combine AREA_X by SUB_REG (sum(TOT_CAT) for each SUB_REG)
## 3) For (1) and (2) get total caught (numbers) for each unique species by permit type or region
# compare counts

## ---- set up ----

source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# download Reports / SAFIS Catches Extended for each month, one year is too slow

safis_catch <- 
  list.files(path = file.path(my_paths$inputs, "compare_catch/SAFIS CATCHES EXTENDED_2022"), 
             pattern = "*.csv",
             full.names = TRUE)  %>%
  map_df(~read_csv(.x,
                   show_col_types = FALSE) %>% 
           mutate(across(.fns = as.character))) %>%
  type_convert() %>%
  unique()

str(safis_catch)
# A tibble: 327,397 × 59

csv_names_list <- c(
  # "compare_catch/FHIER_all_logbook_data.csv",
                "compare_catch/mrip_estim_catch_2022_2022/species_list.csv",
                "compare_catch/mrip_estim_catch_2022_2022/mrip_estim_catch_year_2022_2022.csv")
temp_var <- load_csv_names(my_paths, csv_names_list)
# logbooks <- temp_var[[1]]
mrip_species_list <- temp_var[[1]]
mrip_estimate <- temp_var[[2]]

# str(logbooks)
# str(mrip_estimate)

# ---- the breath of species caught in all logbooks (2022+) ----
# ?? Do this by region (gulf vs s atl vessels).
# ---- Then the total caught (numbers) for each unique species. ----

## ---- to get permit type info use compliance reports for now ----
# use all years, to get more vessel ids with permit info
compliance_csv_names_list_21_23 = c(
  "FHIER_Compliance_23__03_01_23.csv",
  "FHIER_Compliance_22__02_24_23.csv",
  "FHIER_Compliance_21__03_01_23.csv")
full_file_names <- prepare_csv_names(compliance_csv_names_list_21_23)
csv_contents <- load_csv_names(my_paths, full_file_names)

# unify headers, trim vesselofficialnumber, just in case
csvs_clean1 <- clean_all_csvs(csv_contents)
compl_clean <- csvs_clean1[[1]]
# str(compl_clean)

## ---- check if all the vessels from the logbooks are in compl_clean ----
compl_ids <-
  compl_clean %>% select(vesselofficialnumber) %>% unique()

safis_catch_ids <-
  safis_catch %>% select(VESSEL_OFFICIAL_NBR) %>% unique()

# setdiff(safis_catch_ids$VESSEL_OFFICIAL_NBR, compl_ids$vesselofficialnumber) %>% str()
# 241 TODO: get permit type info
# intersect(compl_ids$vesselofficialnumber, safis_catch_ids$VESSEL_OFFICIAL_NBR) %>% str()
# 1599

dim(compl_ids)
# 3640
# 2850 (newer 2023 file)
# dim(safis_catch_ids)
# 1840

# setdiff(logbooks_ids$VESSEL_OFFICIAL_NBR, compl_ids$vesselofficialnumber) %>%
#   cat(sep = ', ')
  # str()
# 6

## ---- get the permit info ----

permit_info <-
  compl_clean %>% 
  filter(vesselofficialnumber %in% safis_catch_ids$VESSEL_OFFICIAL_NBR) %>%
    select(vesselofficialnumber, permitgroup) %>% unique()

str(permit_info)
# 'data.frame':	1861 obs. of  2 variables logbooks_ids
# 'data.frame':	1815 obs. of  2 variables for safis_catch_ids

## ---- separate gulf and sa permits ----
permit_info %<>%
  mutate(sa_permits_only = case_when(
    !grepl("RCG|HRCG|CHG|HCHG", permitgroup, ignore.case = TRUE) ~ "yes",
    .default = "no")
  ) %>%
  mutate(sa_permits_only = as.factor(sa_permits_only))

# str(permit_info)
# 'data.frame':	1861 obs. of  3 variables
# sa_permits_only     : Factor w/ 2 levels "no","yes"

## ---- ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels) ----

names(safis_catch)

species_vsl <-
  # combine logbook and permit info
  inner_join(safis_catch, 
             permit_info, 
             by = c("VESSEL_OFFICIAL_NBR" = "vesselofficialnumber"),
             multiple = "all") %>% 
    # select columns to use
    select(VESSEL_OFFICIAL_NBR,
           SPECIES_ITIS,
           # CATCH_SPECIES_ITIS, for logbooks
           REPORTED_QUANTITY,
           permitgroup,
           sa_permits_only
           ) 
str(species_vsl)

# species_vsl$SPECIES_ITIS %>% unique() %>% length()
# 471

## ---- catch info ----
## ---- The total caught (numbers) for each unique species ----

quantity_by_species <-
  safis_catch %>%
  select(SPECIES_ITIS, REPORTED_QUANTITY) %>% 
  group_by(SPECIES_ITIS) %>% 
# CATCH_SPECIES_ITIS: int [1:467] 
  summarise(sum(REPORTED_QUANTITY))
head(quantity_by_species, 10)

## ---- The total caught (numbers) for each unique species by permit type ----
quantity_by_species_and_permit <-
  species_vsl %>%
  select(sa_permits_only, SPECIES_ITIS, REPORTED_QUANTITY) %>% 
  group_by(SPECIES_ITIS, sa_permits_only) %>% 
  # CATCH_SPECIES_ITIS: int [1:467] for logbooks
  summarise(sum(REPORTED_QUANTITY))
head(quantity_by_species, 10)

## test
# quantity_by_species_and_permit %>% head(10)
# quantity_by_species_and_permit[6:7,]
# for logbooks
# quantity_by_species_and_permit[9:10,]

# species_vsl_sorted <-
  # species_vsl %>% arrange(SPECIES_ITIS)
# grep("...", species_vsl_sorted$SPECIES_ITIS)
# species_vsl_sorted[146:152,]

# for logbooks
# species_vsl_sorted[205:217,]

## ---- MRIP data ----
# landing	Total Harvest (A+B1)	The total number of fish removed from the fishery resource.  May be obtained by summing catch types A (CLAIM) and B1 (HARVEST).
# tot_cat	Total Catch (A+B1+B2)	The number of fish caught but not necessarily brought ashore.  May be obtained by summing catch types A (CLAIM), B1 (HARVEST), and B2 (RELEASE).

# sp_code	Species Code	Species code of fish
# sub_reg	Region	" Subregion code for region of trip
# 4   = North Atlantic (ME; NH; MA; RI; CT) 
# 5   = Mid-Atlantic (NY; NJ; DE; MD; VA) 
# 6   = South Atlantic (NC; SC; GA; EFL) 
# 7   = Gulf of Mexico (WFL; AL; MS; LA) 
# 8   = West Pacific (HI) 
# 11 = U. S. Caribbean (Puerto Rico and Virgin Islands"
# ? We need 6 & 7

# year

# mrip_estimate %>%
#   select(YEAR) %>% unique()
# YEAR
# 1 2022
options(scipen=999)

mrip_estimate_catch <-
  mrip_estimate %>%
    select(SP_CODE, SUB_REG, LANDING, TOT_CAT) %>%
    filter(SUB_REG %in% c(6, 7)) %>% 
    group_by(SP_CODE, SUB_REG) %>% 
    summarise(mrip_total_catch = sum(TOT_CAT))
head(mrip_estimate_catch, 2)
# gropd_df [298 × 3]
# fields: SP_CODE SUB_REG total_catch

## get the same ids for species between  FHIER and MRIP
# ? where to get scientific names by id
str(mrip_species_list)

# grep("scientific", names(logbooks), ignore.case = T, value = T)
# 0
# grep("scientific", names(safis_catch), ignore.case = T, value = T)
# SCIENTIFIC_NAME
# grep("scientific", names(mrip_species_list), ignore.case = T, value = T)
# SCIENTIFIC_NAME

## ---- add common species identifier to safis_catch ----
safis_catch_w_mrip <- 
  mrip_species_list %>%
  inner_join(safis_catch,
  by = "SCIENTIFIC_NAME",
  multiple = "all")

# View(species_info)
# names(safis_catch_w_mrip)
# rename sp_code to upper case for compartability with mrip
colnames(safis_catch_w_mrip)[colnames(safis_catch_w_mrip) == 'sp_code'] <- toupper('sp_code')
safis_catch_w_mrip %>%
  select(SCIENTIFIC_NAME, SPECIES_ITIS, SP_CODE) %>% unique() %>% str()
# 385

# mrip_species_list$sp_code %>% unique() %>% str()
# 1775
# safis_catch$SPECIES_ITIS %>% unique() %>% str()
# 477
# logbooks$CATCH_SPECIES_ITIS %>% unique() %>% str()
# 467

safis_catch_w_mrip %>% str()

species_vsl <-
  # combine logbook and permit info
  inner_join(safis_catch_w_mrip, 
             permit_info, 
             by = c("VESSEL_OFFICIAL_NBR" = "vesselofficialnumber"),
             multiple = "all") %>% 
  # select columns to use
  select(VESSEL_OFFICIAL_NBR,
         SPECIES_ITIS,
         SP_CODE,
         # CATCH_SPECIES_ITIS, for logbooks
         REPORTED_QUANTITY,
         permitgroup,
         sa_permits_only
  ) 
str(safis_catch_w_mrip)

quantity_by_species_and_permit_1 <-
  species_vsl %>%
  select(sa_permits_only, SP_CODE, SPECIES_ITIS, REPORTED_QUANTITY) %>% 
  group_by(SP_CODE, sa_permits_only) %>% 
  summarise(safis_total_catch = sum(REPORTED_QUANTITY))
# head(quantity_by_species_and_permit_1, 10)

# compare with mrip
# mrip_estimate_catch
head(quantity_by_species_and_permit_1, 3)
head(mrip_estimate_catch, 3)

mrip_and_safis <-
  inner_join(quantity_by_species_and_permit_1,
           mrip_estimate_catch,
           by = "SP_CODE"
           )

head(mrip_and_safis, 3)
