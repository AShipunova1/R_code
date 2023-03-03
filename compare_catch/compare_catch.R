# Compare catch in survey vs logbook
# see read.me

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

## ---- add common species identifier to FHIER data ----

scientific_names_w_mrip <- 
  inner_join(mrip_species_list,
             scientific_names,
             by = "SCIENTIFIC_NAME",
             multiple = "all")

# names(scientific_names_w_mrip)
# use: SPECIES_ITIS, SP_CODE

## ---- test itis vs. mrip sp_code  ----

# scientific_names_w_mrip %>%
#   select(SCIENTIFIC_NAME, SPECIES_ITIS, SP_CODE) %>% unique() %>% str()
# 511

# total species in mrip
# mrip_species_list$SP_CODE %>% unique() %>% str()
# 1775

# total species in fhier species list
# scientific_names$SPECIES_ITIS %>% unique() %>% str()
# 736

# total species in the logbook file
# logbooks$CATCH_SPECIES_ITIS %>% unique() %>% str()
# 467

sp_code__species_itis <- 
  scientific_names_w_mrip %>% 
  select(SP_CODE, SPECIES_ITIS) 

## ---- add sp_code to FHIER data ---- 
# convert fhier_species_count_by_disposition$SPECIESITIS to char
fhier_species_count_by_disposition %<>%
  mutate(SPECIESITIS = as.character(SPECIESITIS))

fhier_species_count_by_disposition_sp_all <-
  inner_join(sp_code__species_itis, 
             fhier_species_count_by_disposition, 
             by = c("SPECIES_ITIS" = "SPECIESITIS"),
             multiple = "all")

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

## ---- FHIER: count catch by species ----
fhier_quantity_by_species <-
  fhier_species_count_by_disposition_sp %>%
  select(SP_CODE, REPORTEDQUANTITY) %>% 
  group_by(SP_CODE) %>% 
  summarise(fhier_quantity_by_species = sum(REPORTEDQUANTITY))
# head(fhier_quantity_by_species, 10)

## ---- FHIER: count catch by species and permit ----
fhier_quantity_by_species_and_permit <-
  fhier_species_count_by_disposition_sp %>%
  select(PERMITREGION, SP_CODE, REPORTEDQUANTITY) %>% 
  group_by(SP_CODE, PERMITREGION) %>% 
  summarise(fhier_quantity_by_species_and_permit = sum(REPORTEDQUANTITY))
# head(fhier_quantity_by_species_and_permit, 10)

## ---- MRIP data ----
# use sub_reg 6 & 7 for now (SA & GOM)
mrip_estimate <- mrip_estimate_6_7

## ---- convert TOT_CAT to integers ----
# TOT_CAT : chr  "1,111,111" "11,111"
mrip_estimate %<>%
  mutate(TOT_CAT = TOT_CAT %>% 
           str_replace_all(",", "") %>% 
           as.integer()
         )

## ---- MRIP: count catch by species and region ----
# str(mrip_estimate)
mrip_estimate_catch_by_species_and_region <-
  mrip_estimate %>%
    select(SP_CODE, SUB_REG, LANDING, TOT_CAT) %>%
    group_by(SP_CODE, SUB_REG) %>% 
    summarise(mrip_estimate_catch_by_species_and_region = sum(TOT_CAT))
head(mrip_estimate_catch_by_species_and_region, 2)

## ---- MRIP: count catch by species only ----
mrip_estimate_catch_by_species <-
  mrip_estimate %>%
  select(SP_CODE, TOT_CAT) %>% 
  group_by(SP_CODE) %>% 
  summarise(mrip_estimate_catch_by_species = sum(TOT_CAT))
head(mrip_estimate_catch_by_species, 2)

## ---- compare with mrip ----
# mrip_estimate_catch
head(fhier_species_count_by_disposition_sp, 3)
head(fhier_quantity_by_species, 3)
head(mrip_estimate_catch_by_species, 3)

# compare species in fhier with mrip
species_used_in_fhier <-
  fhier_species_count_by_disposition_sp %>%
    select(SP_CODE) %>% unique()
str(species_used_in_fhier)
# 374

species_in_fhier_sp_list <-
  sp_code__species_itis %>%
  select(SP_CODE) %>% unique()
str(species_in_fhier_sp_list)
# 511

species_in_mrip <-
  mrip_estimate %>%
  select(SP_CODE) %>% unique()
str(species_in_mrip)
# 348

# in FHIER with catch info only
setdiff(species_used_in_fhier, species_in_mrip) %>% str()
# 145
# in MRIP only
setdiff(species_in_mrip, species_used_in_fhier) %>% str()
# 119
# in both
intersect(species_used_in_fhier, species_in_mrip) %>% str()
# 229

# in FHIER species list only
setdiff(species_in_fhier_sp_list, species_in_mrip) %>% str()
# 255
# both in FHIER species list and MRIP
intersect(species_in_fhier_sp_list, species_in_mrip) %>% str()
# 256

## ---- if use by region/landing ----
mrip_estimate_catch_1 <-
  mrip_estimate_catch %>%
    mutate(PERMITREGION = 
           case_when(SUB_REG == "6" ~ "SA",
                     SUB_REG == "7" ~ "GOM"
                    )
           ) %>%
  select(-SUB_REG)

str(mrip_estimate_catch_1)

## ---- combine mrip and fhier catch results by species
mrip_and_fhier <-
  full_join(fhier_quantity_by_species,
            mrip_estimate_catch_by_species,
           by = c("SP_CODE")
           )

head(mrip_and_fhier, 3)


mrip_and_fhier %>%
  filter(mrip_estimate_catch_by_species <= fhier_quantity_by_species) %>% str()
# 15 

plot(mrip_and_fhier$mrip_estimate_catch_by_species, 
     # type="o", 
     col="blue", ylim=c(0, 100000))

# Graph trucks with red dashed line and square points
lines(mrip_and_fhier$fhier_quantity_by_species, 
      # type="o", 
      pch = 22, 
      # lty = 2, 
      col="red")
