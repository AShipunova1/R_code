
## 1a) SEDAR spp. lists ----
# DOLPHINFISH and DOLPHIN are combined
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
  dplyr::filter(common_name %in% sa_top)

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
  dplyr::filter(common_name %in% gom_top)

glimpse(gom_top_spp)

## an aux function to use only a wave from year_wave
use_wave <- function(my_df) {
  my_df %>%
    # split the column
    separate_wider_delim(year_wave,
                         delim = "_",
                         names = c("year", "wave")) %>%
    dplyr::select(-year) %>%
    return()
}

#| warning: false
## Separate data frames by region ----
fhier_acl_catch_by_species_state_region_waves_list <-
  fhier_acl_catch_by_species_state_region_waves %>%
  # split by sa_gom column
  split(as.factor(fhier_acl_catch_by_species_state_region_waves$sa_gom)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-one_of("year", "sa_gom"))))

glimpse(fhier_acl_catch_by_species_state_region_waves_list)

## 2b) Top 12 ACL spp. ----
# By region
### GOM Top 12 ACL spp. ----
gom_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  dplyr::filter(sa_gom == "gom") %>%
  dplyr::select(species_itis, acl_estimate_catch_by_4) %>%
  dplyr::group_by(species_itis) %>%
  summarise(acl_count = sum(acl_estimate_catch_by_4)) %>%
  # sort
  arrange(desc(acl_count)) %>%
  head(12)

gom_acl_top_common_names <-
  fhier_common_names %>%
  # keep the subset only
  dplyr::filter(species_itis %in% gom_acl_top_spp$species_itis)

### SA Top 10 ACL spp. ----
sa_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  dplyr::filter(sa_gom == "sa") %>%
  dplyr::select(species_itis, acl_estimate_catch_by_4) %>%
  dplyr::group_by(species_itis) %>%
  # sum the counts by species
  summarise(acl_count = sum(acl_estimate_catch_by_4)) %>%
  # sort
  arrange(desc(acl_count)) %>%
  head(12)
# head(14) 12 fits better in one plot

sa_acl_top_common_names <-
  fhier_common_names %>%
  # keep the subset only
  dplyr::filter(species_itis %in% sa_acl_top_spp$species_itis)

# 2) Data By wave and state ----
# str(fhier_catch_by_species_state_region_waves)
# str(acl_estimate_catch_by_species_state_region_waves)
# fhier_acl_catch_by_species_state_region_waves - has only common species

## split by state ----
fhier_acl_catch_by_species_state_region_waves_states_list <-
  fhier_acl_catch_by_species_state_region_waves %>%
  split(as.factor(fhier_acl_catch_by_species_state_region_waves$state)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-"state")))

names(fhier_acl_catch_by_species_state_region_waves_states_list[[2]])

# 3) Data By year and region ----
names(fhier_acl_catch_by_species_state_region_waves)

fhier_acl_catch_by_species_region_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::select(-c(state, wave, year)) %>%
  # dplyr::select(species_itis,
  #        common_name,
  #        sa_gom,
  #        fhier_quantity_by_4,
  #        acl_estimate_catch_by_4) %>%
  dplyr::group_by(species_itis,
           common_name,
           sa_gom) %>%
  summarise(
    fhier_cnts_by_year = sum(fhier_quantity_by_4),
    rec_acl_cnts_by_year = sum(acl_estimate_catch_by_4)
  ) %>%
  dplyr::ungroup()

## split by sa_gom ----
fhier_acl_catch_by_species_region_year_list <-
  fhier_acl_catch_by_species_region_year %>%
  ungroup %>%
  split(as.factor(fhier_acl_catch_by_species_region_year$sa_gom)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-"sa_gom")))

# test 167760 GROUPER, BLACK ----
fhier_acl_catch_by_species_region_year_list$sa %>%
  dplyr::filter(species_itis == '167760') %>%
  glimpse()
# gom
# fhier_cnts_by_year   <int> 2016
# rec_acl_cnts_by_year <int> 1808
# sa
# fhier_cnts_by_year   <int> 140
# rec_acl_cnts_by_year <int> 262

acl_estimate_2022 %>%
  dplyr::filter(itis_code == '167760') %>%
  dplyr::group_by(itis_code, new_moden, year, sub_reg) %>%
  summarise(GROUPER_BLACK_cnts_2022 = sum(ab1))
# 2070
# correct (262 + 1808)


# 4) Data By year and state ----

fhier_acl_catch_by_species_state_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::select(species_itis,
         common_name,
         state,
         fhier_quantity_by_4,
         acl_estimate_catch_by_4) %>%
  dplyr::group_by(species_itis,
           common_name,
           state) %>%
  dplyr::mutate(
    fhier_sum_cnts = sum(fhier_quantity_by_4),
    rec_acl_sum_cnts = sum(acl_estimate_catch_by_4)
  ) %>%
  dplyr::select(-c(fhier_quantity_by_4, acl_estimate_catch_by_4)) %>%
  unique()

## split by state ----
fhier_acl_catch_by_species_state_year_list <-
  fhier_acl_catch_by_species_state_year %>%
  ungroup %>%
  split(as.factor(fhier_acl_catch_by_species_state_year$state)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-"state")))

state_year_has_rec_acl_data_list <-
  fhier_acl_catch_by_species_state_year_list
# str(state_year_has_rec_acl_data_list)

my_st_names <- names(state_year_has_rec_acl_data_list)

