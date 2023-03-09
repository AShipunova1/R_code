# Compare catch in survey vs logbook
# see read.me

## ---- set up ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

source("~/R_code_github/compare_catch/get_data.R")

# ---- the breath of species caught in SEFIHIER (2022) ----
# ?? (where is the permit info) Do this by region (gulf vs s atl vessels). Or by landing?
# ---- Then the total caught (numbers) for each unique species. ----
# ?? species ids are different in MRIP in SEFHIER, need a scientific name to connect

## ---- ID the breath of species caught in all SEFHIER data. Do this by region (gulf vs s atl vessels) ----

# names(fhier_species_count_by_disposition)
# str(fhier_species_count_by_disposition)
# 'data.frame':	316171  obs. of  6 variables:

## ---- FHIER: count catch by species ----
fhier_quantity_by_species <-
  fhier_species_count_by_disposition %>%
  select(speciesitis, reportedquantity) %>% 
  group_by(speciesitis) %>% 
  summarise(fhier_quantity_by_species = sum(reportedquantity))
head(fhier_quantity_by_species, 10)

## ---- add common names ----

# change both columns to character
fhier_quantity_by_species <-
  mutate(fhier_quantity_by_species, speciesitis = as.character(speciesitis))
scientific_names <-
  mutate(scientific_names, species_itis = as.character(species_itis))

# names(scientific_names)
# common_name
fhier_species_count_by_disposition_com_names <-
    inner_join(fhier_quantity_by_species,
          scientific_names, 
          by = c("speciesitis" = "species_itis")
          )

# str(fhier_species_count_by_disposition_com_names)
  
# red snapper, greater amberjack, gag, and gray triggerfish
fhier_species_count_by_disposition_com_names %>%
  filter(grepl("snapper.*red", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("amberjack.*greater", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("gag", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("triggerfish.*gray", tolower(common_name)))

## ---- FHIER: count catch by species and permit ----
fhier_quantity_by_species_and_permit <-
  fhier_species_count_by_disposition %>%
  select(permitregion, speciesitis, reportedquantity) %>% 
  group_by(speciesitis, permitregion) %>% 
  summarise(fhier_quantity_by_species_and_permit = sum(reportedquantity))
# head(fhier_quantity_by_species_and_permit, 10)

## ---- MRIP data ----

## ---- convert TOT_CAT to integers ----
# TOT_CAT : chr  "1,111,111" "11,111"
glimpse(mrip_estimate)
mrip_estimate %<>%
  mutate(TOT_CAT = TOT_CAT %>% 
           str_replace_all(",", "") %>% 
           as.integer()
         )

## ---- MRIP: count catch by species and region ----
# str(mrip_estimate)
mrip_estimate_catch_by_species_and_region <-
  mrip_estimate %>%
    select(speciesitis, SUB_REG, LANDING, TOT_CAT) %>%
    group_by(speciesitis, SUB_REG) %>% 
    summarise(mrip_estimate_catch_by_species_and_region = sum(TOT_CAT))
head(mrip_estimate_catch_by_species_and_region, 2)

## ---- MRIP: count catch by species only ----
mrip_estimate_catch_by_species <-
  mrip_estimate %>%
  select(speciesitis, TOT_CAT) %>% 
  group_by(speciesitis) %>% 
  summarise(mrip_estimate_catch_by_species = sum(TOT_CAT))
head(mrip_estimate_catch_by_species, 2)

## ---- compare with mrip ----
# mrip_estimate_catch
head(fhier_species_count_by_disposition, 3)
head(fhier_quantity_by_species, 3)
head(mrip_estimate_catch_by_species, 3)

# compare species in fhier with mrip
species_used_in_fhier <-
  fhier_species_count_by_disposition %>%
  select(speciesitis) %>% unique()
str(species_used_in_fhier)
# 374

species_in_fhier_sp_list <-
  speciesitis__species_itis %>%
  select(speciesitis) %>% unique()
str(species_in_fhier_sp_list)
# 511

species_in_mrip <-
  mrip_estimate %>%
  select(speciesitis) %>% unique()
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
# mrip_estimate_catch_1 <-
#   mrip_estimate_catch %>%
#     mutate(PERMITREGION = 
#            case_when(SUB_REG == "6" ~ "SA",
#                      SUB_REG == "7" ~ "GOM"
#                     )
#            ) %>%
#   select(-SUB_REG)

# str(mrip_estimate_catch_1)

## ---- combine mrip and fhier catch results by species
mrip_and_fhier <-
  full_join(fhier_quantity_by_species,
            mrip_estimate_catch_by_species,
            by = c("speciesitis")
  )

head(mrip_and_fhier, 3)

mrip_and_fhier %>%
  filter(mrip_estimate_catch_by_species <= fhier_quantity_by_species) %>% str()
# 15 

source("~/R_code_github/compare_catch/plots.R")

## ---- get MRIP counts for federal waters only