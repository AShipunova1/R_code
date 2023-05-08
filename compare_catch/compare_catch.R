# TODO ----
# 3 sets of spp: 
# 1a) SEDAR; 
# 2b) Recreational ACL tops; 
# 3c) All FHIER spp

# Plots:
# 1) By wave and region
# 2) By wave and state
# 3) By year and region
# 4) By year and state
# ---

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
source("~/R_code_github/compare_catch/auxiliary/compare_catch_data_preparation.R")

## All FHIER common names and itis in a separate data frame ----
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

## 1a) SEDAR spp. lists ----
# DOLPHINFISH and DOLPHIN are combined
# CEDAR spp List by Michelle
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
# CEDAR spp List by Michelle
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
    # split the column
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
## Separate data frames by region ----
fhier_acl_catch_by_species_state_region_waves_list <-
  fhier_acl_catch_by_species_state_region_waves %>%
  # split by sa_gom column
  split(as.factor(fhier_acl_catch_by_species_state_region_waves$sa_gom)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-one_of("year", "sa_gom"))))

glimpse(fhier_acl_catch_by_species_state_region_waves_list)

## 2b) Top 12 ACL spp. ----
### GOM Top 12 ACL spp. ----
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
# List of 17

# 3) Data By year and region ----
names(fhier_acl_catch_by_species_state_region_waves)

fhier_acl_catch_by_species_region_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
  select(-c(state, wave, year)) %>%
  # select(species_itis,
  #        common_name,
  #        sa_gom,
  #        fhier_quantity_by_4,
  #        acl_estimate_catch_by_4) %>%
  group_by(species_itis,
         common_name,
         sa_gom) %>%
  summarise(
    fhier_cnts_by_year_reg = sum(fhier_quantity_by_4),
    rec_acl_cnts_by_year_reg = sum(acl_estimate_catch_by_4)
  ) %>%
  ungroup()
  # select(-c(fhier_quantity_by_4, acl_estimate_catch_by_4)) %>%
  # unique()

## split by sa_gom ----
fhier_acl_catch_by_species_region_year_list <-
  fhier_acl_catch_by_species_region_year %>%
  ungroup %>%
  split(as.factor(fhier_acl_catch_by_species_region_year$sa_gom)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-"sa_gom")))

# test 167760 GROUPER, BLACK ----
fhier_acl_catch_by_species_region_year_list$sa %>%
  filter(species_itis == '167760') %>%
  glimpse()
# gom
# fhier_cnts_by_year_reg   <int> 2016
# rec_acl_cnts_by_year_reg <int> 1808
# sa
# fhier_cnts_by_year_reg   <int> 140
# rec_acl_cnts_by_year_reg <int> 262

acl_estimate_2022 %>%
  filter(itis_code == '167760') %>%
  group_by(itis_code, new_moden, year, sub_reg) %>%
  summarise(GROUPER_BLACK_cnts_2022 = sum(ab1))
  # 2070
  # correct (262 + 1808)


# 4) Data By year and state ----

fhier_acl_catch_by_species_state_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
  select(species_itis,
         common_name,
         state,
         fhier_quantity_by_4,
         acl_estimate_catch_by_4) %>%
  group_by(species_itis,
         common_name,
         state) %>%
  mutate(
    fhier_sum_cnts = sum(fhier_quantity_by_4),
    rec_acl_sum_cnts = sum(acl_estimate_catch_by_4)
  ) %>%
  select(-c(fhier_quantity_by_4, acl_estimate_catch_by_4)) %>%
  unique()

# test
# fhier_acl_catch_by_species_state_year %>%
  # filter(species_itis == '169059') %>%
  # glimpse()

## split by state ----
fhier_acl_catch_by_species_state_year_list <-
  fhier_acl_catch_by_species_state_year %>%
  ungroup %>%
  split(as.factor(fhier_acl_catch_by_species_state_year$state)) %>%
  # remove extra columns in each df
  map(.f = list(. %>% dplyr::select(-"state")))

state_year_has_rec_acl_data_list <- fhier_acl_catch_by_species_state_year_list
# str(state_year_has_rec_acl_data_list)

state_year_has_rec_acl_data_list_new <- c()
# state_year_has_rec_acl_data_list_new["MA"] <- fhier_acl_catch_by_species_state_year_list["MA"]

# View(state_year_has_rec_acl_data_list_new)

my_st_names <- names(state_year_has_rec_acl_data_list)

for (i in 1:length(my_st_names)) {
  # browser()
  state_abbr <- my_st_names[[i]]
  if (sum(fhier_acl_catch_by_species_state_year_list[[state_abbr]]$rec_acl_sum_cnts) > 0) {
    state_year_has_rec_acl_data_list_new[state_abbr] <- state_year_has_rec_acl_data_list[state_abbr]
  }
}
View(state_year_has_rec_acl_data_list_new)

# state_year_has_rec_acl_data_list_new <-
names(state_year_has_rec_acl_data_list) %>%
  # repeat for each state
  map(function(state_abbr) {
    # get data for this state
   if (sum(fhier_acl_catch_by_species_state_year_list[[state_abbr]]$rec_acl_sum_cnts) > 0) {
    # browser()
    # state_year_has_rec_acl_data_list[names(state_year_has_rec_acl_data_list) != state_abbr]# Remove list element
     # state_year_has_rec_acl_data_list_new["MA"] <- fhier_acl_catch_by_species_state_year_list["MA"]
   state_year_has_rec_acl_data_list_new[state_abbr] = state_year_has_rec_acl_data_list[state_abbr]
     # return(state_year_has_rec_acl_data_list[[state_abbr]])
   }
  })

View(state_year_has_rec_acl_data_list_new)
# str(fhier_acl_catch_by_species_state_year)
# str(fhier_acl_catch_by_species_state_year_list)

# source("~/R_code_github/compare_catch/spp_not_in_rec_acl.R")
