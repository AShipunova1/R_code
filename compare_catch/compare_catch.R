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
           by = c("SP_CODE")
           )

head(mrip_and_fhier, 3)

mrip_and_fhier %>%
  filter(mrip_estimate_catch_by_species <= fhier_quantity_by_species) %>% str()
# 15 

## ---- plot catch by species (1) ----
# Graph mrip
plot(mrip_and_fhier$SP_CODE, mrip_and_fhier$mrip_estimate_catch_by_species, 
     col="blue",
     pch = 17
     , xlim = c(8800000000, max(mrip_and_fhier$SP_CODE))
     , ylim = c(0, 100000) #20000
     ) 

# add fhier
points(mrip_and_fhier$SP_CODE,
  mrip_and_fhier$fhier_quantity_by_species,
       pch = 19,
       cex = .6,
      col="red")

## ---- add common names ----
mrip_and_fhier_w_names <- inner_join(scientific_names_w_mrip,
                                     mrip_and_fhier, 
                                     by = "SP_CODE"
)

## ---- data overview ----
# MRIP numbers are much bigger
mrip_and_fhier_w_names %>%
  filter(mrip_estimate_catch_by_species > 1000) %>% dim()
# 100000: 125  
# 10000 : 182
# 1000  : 221

# Only few FHIER species have counts > 100000
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 1000) %>% dim()
# 100000: 10
# 10000 : 35
# 1000  : 97

mrip_and_fhier_w_common_names <- 
  mrip_and_fhier_w_names %>%
  select(fhier_quantity_by_species,
         mrip_estimate_catch_by_species, 
         COMMON_NAME.x,
         SP_CODE)

data_overview(mrip_and_fhier)

# look at common names for the biggest catch
mrip_and_fhier_w_names %>%
  filter(fhier_quantity_by_species > 100000) %>% 
  select(COMMON_NAME.x, 
         # COMMON_NAME.y, 
         fhier_quantity_by_species,
         mrip_estimate_catch_by_species
         )

## ---- plot catch by species (2) ----
mrip_and_fhier %>%
  rename("fhier_cnt" = "fhier_quantity_by_species",
         "mrip_cnt" = "mrip_estimate_catch_by_species") %>%
  select(-SP_CODE) %>%
  # filter(mrip_cnt < 10000000) %>%
  filter(mrip_cnt < 20000) %>%
  boxplot()

## ---- plot catch by species (2) ----
my_colors <- c("MRIP" = "blue", "FHIER" = "red")

MIN_SP_CODE <- 8000000000
MAX_QUANTITY_BY_SPECIES <- 200000
mrip_and_fhier_short_values <-
  # mrip_and_fhier_w_names %>%
  mrip_and_fhier %>%
  mutate(SP_CODE = ifelse(SP_CODE >= MIN_SP_CODE, SP_CODE, NA)) %>%
  mutate(fhier_quantity_by_species_short = ifelse(fhier_quantity_by_species <= MAX_QUANTITY_BY_SPECIES, fhier_quantity_by_species, NA)) %>%
  mutate(mrip_estimate_catch_by_species_short = ifelse(mrip_estimate_catch_by_species <= MAX_QUANTITY_BY_SPECIES, mrip_estimate_catch_by_species, NA))
  # mutate(order = fct_reorder(as.factor(SP_CODE), COMMON_NAME.x)) %>% 
str(mrip_and_fhier_short_values)
  
counts_plot <-
  mrip_and_fhier_short_values %>%
  ggplot(aes(x = SP_CODE,
               # order,
             y = fhier_quantity_by_species_short,
             colour = "FHIER"
             ),
         size = 2,
         alpha = 0.1
         ) + 
  geom_point(aes(x = SP_CODE,
                   # order,
                 y = mrip_estimate_catch_by_species_short, 
                 colour = "MRIP"
                ),
             size = 1
             ) +
  labs(title = "catch by species",
     x = "species code", 
     # x = "common_name", 
     y = paste0("quantity by species if < ", MAX_QUANTITY_BY_SPECIES)
) +
  theme(axis.text.x = element_text(angle = 45)
        # ,
        # axis.text.y = element_blank()
        ) +
  scale_colour_manual(values = my_colors) +
  geom_point()

counts_plot
# Warning messages:
#   1: Removed 305 rows containing missing values (`geom_point()`). 
# 2: Removed 154 rows containing missing values (`geom_point()`). 