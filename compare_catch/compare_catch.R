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
# fhier_species_count_by_disposition %>%
  # select(disposition) %>% unique()
## ---- FHIER: count catch by species ----
fhier_quantity_by_species <-
  fhier_species_count_by_disposition %>%
  select(species_itis, reported_quantity) %>% 
  group_by(species_itis) %>% 
  summarise(fhier_quantity_by_species = sum(as.integer(reported_quantity)))
head(fhier_quantity_by_species, 10)

## ---- add common names ----

# change both columns to character
fhier_quantity_by_species <-
  mutate(fhier_quantity_by_species, species_itis = as.character(species_itis))
scientific_names <-
  mutate(scientific_names, species_itis = as.character(species_itis))

# names(scientific_names)
# common_name
fhier_species_count_by_disposition_com_names <-
    inner_join(fhier_quantity_by_species,
          scientific_names, 
          by = "species_itis"
          )

str(fhier_species_count_by_disposition_com_names)
  
# red snapper, greater amberjack, gag, and gray triggerfish
fhier_species_count_by_disposition_com_names %>%
  filter(grepl("snapper.*red", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("amberjack.*greater", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("gag", tolower(common_name)))

fhier_species_count_by_disposition_com_names %>%
  filter(grepl("triggerfish.*gray", tolower(common_name)))

names(fhier_species_count_by_disposition)
## ---- FHIER: count catch by species and permit ----
fhier_quantity_by_species_and_permit <-
  fhier_species_count_by_disposition %>%
  select(permit_region, species_itis, reported_quantity) %>% 
  group_by(species_itis, permit_region) %>% 
  summarise(fhier_quantity_by_species_and_permit = sum(as.integer(reported_quantity)))
# head(fhier_quantity_by_species_and_permit, 10)

## ---- MRIP data ----

## ---- convert ab1 to integers ----
names(mrip_estimate)
mrip_estimate %<>%
  mutate(ab1 = as.integer(ab1))

## ---- MRIP: count catch by species and region ----
# str(mrip_estimate)
mrip_estimate_catch_by_species_and_region <-
  mrip_estimate %>%
    select(itis_code, sub_reg, ab1) %>%
    group_by(itis_code, sub_reg) %>% 
    summarise(mrip_estimate_catch_by_species_and_region = sum(ab1))
# head(mrip_estimate_catch_by_species_and_region, 20)

## ---- MRIP: count catch by species only ----
mrip_estimate_catch_by_species <-
  mrip_estimate %>%
  select(itis_code, ab1) %>% 
  group_by(itis_code) %>% 
  summarise(mrip_estimate_catch_by_species = sum(ab1))
# head(mrip_estimate_catch_by_species, 2)

## ---- compare fhier with mrip ----
# mrip_estimate_catch
head(fhier_species_count_by_disposition, 3)
head(fhier_quantity_by_species, 3)
head(mrip_estimate_catch_by_species, 3)

## ---- compare species in fhier with mrip ----

sp_itis_fhier <-
  grep("itis", tolower(names(fhier_species_count_by_disposition)), value = TRUE)

species_used_in_fhier <-
  fhier_species_count_by_disposition %>%
  select(all_of(sp_itis_fhier)) %>% 
  unique() %>%
  set_names(sp_itis_mrip <- "itis")

str(species_used_in_fhier)
# 458

sp_itis_mrip <-
  grep("itis", tolower(names(mrip_estimate)), value = TRUE)

species_in_mrip <-
  mrip_estimate %>%
  # select(sp_code) %>% unique()
  select(all_of(sp_itis_mrip)) %>% 
  unique() %>%
  set_names(sp_itis_mrip <- "itis")
str(species_in_mrip)
# 76 itis_code

# in FHIER with catch info only
setdiff(species_used_in_fhier, species_in_mrip) %>% str()
# 145
# 386
# in MRIP only
setdiff(species_in_mrip, species_used_in_fhier) %>% str()
# 119
# 4

# in both
intersect(species_used_in_fhier, species_in_mrip) %>% str()
# 229
# 72

## ---- if use by region/landing ----
# mrip_estimate_catch_1 <-
#   mrip_estimate_catch %>%
#     mutate(permit_region = 
#            case_when(sub_reg == "6" ~ "SA",
#                      sub_reg == "7" ~ "GOM"
#                     )
#            ) %>%
#   select(-sub_reg)

# str(mrip_estimate_catch_1)

## ---- combine mrip and fhier catch results by species
mrip_and_fhier <-
  full_join(fhier_quantity_by_species,
            mrip_estimate_catch_by_species,
            by = c("species_itis" = "itis_code")
  )

head(mrip_and_fhier, 3)

# fhier quantity is grater than mrip's
mrip_and_fhier %>%
  filter(mrip_estimate_catch_by_species <= fhier_quantity_by_species) %>% str()
# 15 

# combine mrip with fhier with common names
fhier_species_count_by_disposition_com_names

fhier_species_count_by_disposition_com_names

source("~/R_code_github/compare_catch/plots.R")


## ---- most n frequent FHIER species ----

str(fhier_quantity_by_species)

get_n_most_frequent_fhier <- function(n) {
  fhier_quantity_by_species %>%
    arrange(desc(fhier_quantity_by_species)) %>%
    head(n) %>%
    return()
}
