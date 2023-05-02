##| echo: false
library(zoo)
library(gridExtra)
library(grid)
# install.packages("viridis")
library(viridis)

## include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

##| echo: false
source("~/R_code_github/compare_catch/compare_catch_data_preparation.R")

## save common names and itis in a separate data frame ----
fhier_common_names <-
  fhier_logbooks_content %>%
  # names()
  select(catch_species_itis, common_name) %>%
  unique()

# add column names
names(fhier_common_names) <- c("species_itis", "common_name")

## Join Fhier and ACL ----
fhier_acl_catch_by_species_state_region_waves <-
  full_join(
    fhier_catch_by_species_state_region_waves,
    acl_estimate_catch_by_species_state_region_waves,
    by = join_by(species_itis, state, sa_gom, year, wave)
  )

## NA counts to 0 ----
# change NAs to 0 where one or another agency doesn't have counts for this species
fhier_acl_catch_by_species_state_region_waves %<>%
  mutate(
    fhier_quantity_by_4 =
      replace_na(fhier_quantity_by_4, 0),
    acl_estimate_catch_by_4 =
      replace_na(acl_estimate_catch_by_4, 0)
  )

### test join ----
# look at the first 20 entries for mackerel spanish
fhier_acl_catch_by_species_state_region_waves %>%
  filter(species_itis == test_species_itis) %>% head(20)

#| classes: test
### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----
# names(fhier_acl_catch_by_species_state_region_waves)
fhier_acl_catch_by_species_state_region_waves %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(fhier_test_cnts$mackerel_fhier_cnt)

# acl_test_cnts
# fhier_test_cnts

fhier_acl_catch_by_species_state_region_waves %>%
  filter(species_itis == test_species_itis) %>%
  group_by(species_itis, sa_gom) %>%
  summarise(mackerel_acl_cnt = sum(acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_acl_cnt) %>%
  identical(acl_test_cnts$mackerel_acl_cnt)

# grep("grouper, black", fhier_common_names$common_name, value = T, ignore.case = T)

## Councils spp. lists ----
# DOLPHINFISH and DOLPHIN are combined
# List by Michelle
sa_top <- c(
  "BASS, BLACK SEA",
  "DOLPHIN",
  "GROUPER, BLACK",
  "GROUPER, GAG",
  "GROUPER, RED",
  "GROUPER, SCAMP",
  "MACKEREL, SPANISH",
  "SNAPPER, RED",
  "TRIGGERFISH, GRAY"
)

sa_top_spp <-
  fhier_common_names %>%
  filter(common_name %in% sa_top)

# View(sa_top)
# intersect(sa_top, fhier_common_names$common_name)
# List by Michelle
gom_top <- c(
  "AMBERJACK, GREATER",
  "COBIA",
  "GROUPER, BLACK",
  "GROUPER, GAG",
  "GROUPER, RED",
  "GROUPER, SCAMP",
  "MACKEREL, KING",
  "MACKEREL, SPANISH",
  "SNAPPER, GRAY",
  "SNAPPER, RED",
  "TRIGGERFISH, GRAY"
)

gom_top_spp <-
  fhier_common_names %>%
  filter(common_name %in% gom_top)

glimpse(gom_top_spp)

## an aux function to use only a wave from year_wave
use_wave <- function(my_df) {
  my_df %>%
    separate_wider_delim(year_wave,
                         delim = "_",
                         names = c("year", "wave")) %>%
    select(-year) %>%
    return()
}

glimpse(fhier_acl_catch_by_species_state_region_waves)
# Rows: 6,327
# Columns: 7
# Rows: 5,728

#| warning: false
## Make separate data frames by region ----
fhier_acl_catch_by_species_state_region_waves_list <-
  fhier_acl_catch_by_species_state_region_waves %>%
  # split by sa_gom column
  split(as.factor(fhier_acl_catch_by_species_state_region_waves$sa_gom)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-one_of("year", "sa_gom"))))

glimpse(fhier_acl_catch_by_species_state_region_waves_list)

## Top 10 ACL spp. ----
### GOM Top 10 ACL spp. ----
gom_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  filter(sa_gom == "gom") %>%
  select(species_itis, acl_estimate_catch_by_4) %>%
  group_by(species_itis) %>%
  summarise(acl_count = sum(acl_estimate_catch_by_4)) %>%
  # sort
  arrange(desc(acl_count)) %>%
  head(12)

gom_acl_top_common_names <-
  fhier_common_names %>%
  # keep the subset only
  filter(species_itis %in% gom_acl_top_spp$species_itis)

### SA Top 10 ACL spp. ----
sa_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  filter(sa_gom == "sa") %>%
  select(species_itis, acl_estimate_catch_by_4) %>%
  group_by(species_itis) %>%
  # sum the counts by species
  summarise(acl_count = sum(acl_estimate_catch_by_4)) %>%
  # sort
  arrange(desc(acl_count)) %>%
  head(12)
# head(14) 12 fits better in one plot


sa_acl_top_common_names <-
  fhier_common_names %>%
  # keep the subset only
  filter(species_itis %in% sa_acl_top_spp$species_itis)

