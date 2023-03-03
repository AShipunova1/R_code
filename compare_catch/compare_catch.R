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

# ---- rename all field names to upper case for comparability ----
data_list_names <- list("fhier_species_count_by_disposition", "mrip_species_list", 
                        "mrip_estimate_6_7", "scientific_names")

for(d_name in data_list_names) {
  # get an object (df) by its name
  tmp0 <- get(d_name)
  # change field names to upper case
  tmp1 <- rename_with(tmp0, toupper)
  # assign newly renamed df back to the same df name
  assign(d_name, tmp1)
}

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

## ---- add common species identifier to FHIER data ----

scientific_names_w_mrip <- 
  inner_join(mrip_species_list,
             scientific_names,
             by = "SCIENTIFIC_NAME",
             multiple = "all")

# names(scientific_names_w_mrip)
# use: SPECIES_ITIS, SP_CODE

## ---- test ----
# itis vs. mrip sp_code 
# scientific_names_w_mrip %>%
  # select(SCIENTIFIC_NAME, SPECIES_ITIS, SP_CODE) %>% unique() %>% str()
# 511

# total species in mrip
# mrip_species_list$sp_code %>% unique() %>% str()
# 1775

# total species in fhier species list
# scientific_names$SPECIES_ITIS %>% unique() %>% str()
# 736

# total species in the logbook file
# logbooks$CATCH_SPECIES_ITIS %>% unique() %>% str()
# 467

scientific_names_w_mrip %>% str()

## ---- add sp_code to FHIER data ---- 
fhier_species_count_by_disposition %<>%
  mutate(speciesitis = as.character(speciesitis))

# str(fhier_species_count_by_disposition)
fhier_species_count_by_disposition_sp_all <-
  inner_join(scientific_names_w_mrip, 
             fhier_species_count_by_disposition, 
             by = c("SPECIES_ITIS" = "speciesitis"),
             multiple = "all")

# make all field names capital
names(fhier_species_count_by_disposition_sp_all) <- toupper(names(fhier_species_count_by_disposition_sp_all))

## ---- select columns to use ----
fhier_species_count_by_disposition_sp <- 
  fhier_species_count_by_disposition_sp_all %>%
    select(VESSELOFFICIALNUMBER,
         SPECIES_ITIS,
         SP_CODE,
         PERMITREGION,
         REPORTEDQUANTITY,
         DISPOSITION
  ) 
# str(fhier_species_count_by_disposition_sp)
# 'data.frame':	291346 obs. of  6 variables:
  
quantity_by_species_and_permit_1 <-
  fhier_species_count_by_disposition_sp %>%
  select(PERMITREGION, SP_CODE, SPECIES_ITIS, REPORTEDQUANTITY) %>% 
  group_by(SP_CODE, PERMITREGION) %>% 
  summarise(safis_total_catch = sum(REPORTEDQUANTITY))
# head(quantity_by_species_and_permit_1, 10)

## ---- MRIP data ----
# use sub_reg 6 & 7 for now (SA & GOM)
mrip_estimate <- mrip_estimate_6_7

mrip_estimate_catch <-
  mrip_estimate %>%
    select(SP_CODE, SUB_REG, LANDING, TOT_CAT) %>%
    filter(SUB_REG %in% c(6, 7)) %>% 
    group_by(SP_CODE, SUB_REG) %>% 
    summarise(mrip_total_catch = sum(TOT_CAT))
head(mrip_estimate_catch, 2)
# gropd_df [298 × 3]
# fields: SP_CODE SUB_REG total_catch

# str(mrip_species_list)

# grep("scientific", names(logbooks), ignore.case = T, value = T)
# 0
# grep("scientific", names(safis_catch), ignore.case = T, value = T)
# SCIENTIFIC_NAME
# grep("scientific", names(mrip_species_list), ignore.case = T, value = T)
# SCIENTIFIC_NAME

## ---- compare with mrip ----
# mrip_estimate_catch
head(fhier_species_count_by_disposition_sp, 3)
head(quantity_by_species_and_permit_1, 3)
head(mrip_estimate_catch, 3)

mrip_estimate_catch %>%
  filter(SP_CODE == "1000000000")
# 1 1000000000       6         1652894.
# 2 1000000000       7         3042903.

fhier_species_count_by_disposition_sp %>%
  filter(SP_CODE == "1000000000")

# compare sp_code with mrip
species_in_fhier <-
  fhier_species_count_by_disposition_sp %>%
    select(SP_CODE) %>% unique()
str(species_in_fhier)
374

species_in_mrip <-
  mrip_estimate %>%
  select(SP_CODE) %>% unique()
str(species_in_mrip)
# 348

# in both
intersect(species_in_fhier, species_in_mrip) %>% str()
# 229
# in FHIER only
setdiff(species_in_fhier, species_in_mrip) %>% str()
# 145
# in MRIP only
setdiff(species_in_mrip, species_in_fhier) %>% str()
# 119

mrip_estimate_catch_1 <-
  mrip_estimate_catch %>%
    mutate(PERMITREGION = 
           case_when(SUB_REG == "6" ~ "SA",
                     SUB_REG == "7" ~ "GOM"
                    )
           ) %>%
  select(-SUB_REG)

str(mrip_estimate_catch_1)

mrip_and_fhier <-
  inner_join(quantity_by_species_and_permit_1,
             mrip_estimate_catch_1,
           by = c("SP_CODE", "PERMITREGION")
           )

head(mrip_and_fhier, 3)
mrip_and_fhier %>%
  filter(mrip_total_catch <= safis_total_catch) %>% str()
# 18 

plot(mrip_and_fhier$mrip_total_catch, 
     # type="o", 
     col="blue", ylim=c(0, 100000))

# Graph trucks with red dashed line and square points
lines(mrip_and_fhier$safis_total_catch, 
      # type="o", 
      pch = 22, 
      # lty = 2, 
      col="red")
