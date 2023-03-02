# Compare catch in survey vs logbook
# see read.me
# all data for 2022

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
# turn off the scientific notation
options(scipen=999)
source("~/R_code_github/compare_catch/get_data.R")

# ---- the breath of species caught in SEFIHIER (2022) ----
# ?? (where is the permit info) Do this by region (gulf vs s atl vessels). Or by landing?
# ---- Then the total caught (numbers) for each unique species. ----
# ?? species ids are different in MRIP in SEFHIER, need a scientific name to connect

## ---- ID the breath of species caught in all SEFHIER data. Do this by region (gulf vs s atl vessels) ----

## ---- catch info ----
## ---- The total caught (numbers) for each unique species ----
# names(fhier_species_count_by_disposition)
# [1] "tripid"               "permitregion"         "vesselofficialnumber"
# [4] "vesselname"           "tripstartdate"        "speciesitis"         
# [7] "name"                 "reportedquantity"     "disposition" 

quantity_by_species <-
  fhier_species_count_by_disposition %>%
  select(speciesitis, reportedquantity) %>% 
  group_by(speciesitis) %>% 
  summarise(sum(reportedquantity))
# head(quantity_by_species, 10)

## ---- The total caught (numbers) for each unique species by permit type ----
quantity_by_species_and_permit <-
  fhier_species_count_by_disposition %>%
  select(permitregion, speciesitis, reportedquantity) %>% 
  group_by(speciesitis, permitregion) %>% 
  summarise(sum(reportedquantity))
# head(quantity_by_species_and_permit, 10)

## ---- test ----
# quantity_by_species_and_permit %>% head(10)
# quantity_by_species_and_permit_sorted <-
#   quantity_by_species_and_permit %>% arrange(speciesitis)
# grep("...", quantity_by_species_and_permit_sorted$speciesitis)
# quantity_by_species_and_permit_sorted[8:9,]

## ---- add common species identifier to safis_catch ----
str(mrip_species_list)
safis_catch_w_mrip <- 
  mrip_species_list %>%
  inner_join(scientific_names,
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

## ---- MRIP data ----

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

## ---- compare with mrip ----
# mrip_estimate_catch
head(quantity_by_species_and_permit_1, 3)
head(mrip_estimate_catch, 3)

mrip_and_safis <-
  inner_join(quantity_by_species_and_permit_1,
           mrip_estimate_catch,
           by = "SP_CODE"
           )

head(mrip_and_safis, 3)
