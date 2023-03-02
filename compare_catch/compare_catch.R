# Compare catch in survey vs logbook
# see read.me


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
options(scipen=999)
source("~/R_code_github/compare_catch/get_data.R")

# ---- the breath of species caught in all logbooks (2022+) ----
# ?? (where is the permit info) Do this by region (gulf vs s atl vessels).
# ---- Then the total caught (numbers) for each unique species. ----
# ?? species ids are different in MRIP in SEFHIER, need a scientific name to connect


fhier_species_count_by_disposition_ids <-
  fhier_species_count_by_disposition %>% 
  select(vesselofficialnumber) %>% unique()

## ---- ID the breath of species caught in all logbooks (2022+). Do this by region (gulf vs s atl vessels) ----

# names(safis_catch)

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
# CATCH_SPECIES_ITIS: int [1:467] # for logbooks
  summarise(sum(REPORTED_QUANTITY))
# head(quantity_by_species, 10)

## ---- The total caught (numbers) for each unique species by permit type ----
quantity_by_species_and_permit <-
  species_vsl %>%
  select(sa_permits_only, SPECIES_ITIS, REPORTED_QUANTITY) %>% 
  group_by(SPECIES_ITIS, sa_permits_only) %>% 
  # CATCH_SPECIES_ITIS: int [1:467] for logbooks
  summarise(sum(REPORTED_QUANTITY))
# head(quantity_by_species_and_permit, 10)

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
## ---- fields ----
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

# check
# mrip_estimate %>%
#   select(YEAR) %>% unique()
# YEAR
# 1 2022

# turn off the scientific notation
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
