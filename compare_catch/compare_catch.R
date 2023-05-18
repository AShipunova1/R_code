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

library(zoo)
library(gridExtra)
library(grid)
library(viridis)

## include auxilary functions ----
source("~/R_code_github/useful_functions_module.r")
my_paths <- set_work_dir()

# set path for auxiliary functions to be used through main code
# and then whenever you call a function from that "sourced" file, be sure to have a note to that same effect: #calling function from auxiliary file

source("~/R_code_github/compare_catch/auxiliary/compare_catch_data_preparation.R")

## separate fhier_spp data ----
fhier_spp <-
  fhier_catch_by_species_state_region_waves_renamed %>%
  select(species_itis, common_name, scientific_name) %>%
  unique()
# dim(fhier_spp)
# [1] 748   3

## Join Fhier and ACL ----
fhier_acl_catch_by_species_state_region_waves <-
  full_join(
    fhier_catch_by_species_state_region_waves_renamed,
    acl_estimate_catch_by_species_state_region_waves_renamed,
    by = join_by(scientific_name, state, sa_gom, year, wave),
    # Override the default suffixes, c(".x", ".y") in not merged cols
    suffix = c("_fhier", "_mrip")
  )

View(fhier_acl_catch_by_species_state_region_waves)
# [1] 5738   11

# fhier_acl_catch_by_species_state_region_waves %>%
#   filter(!(species_itis_fhier == species_itis_mrip)) %>%
#   glimpse()
# dolphin

## NA counts to 0 ----
# change NAs to 0 where one or another agency doesn't have counts for this species
# fhier_acl_catch_by_species_state_region_waves_0 <-
fhier_acl_catch_by_species_state_region_waves %<>%
  mutate(
    fhier_quantity_by_4 =
      replace_na(fhier_quantity_by_4, 0),
    rec_acl_estimate_catch_by_4 =
      replace_na(rec_acl_estimate_catch_by_4, 0)
  )

### test join ----
# look at the first 20 entries for mackerel spanish
fhier_acl_catch_by_species_state_region_waves %>%
  filter(scientific_name == test_species_name) %>% head(20)

#| classes: test
### test one sp in MRIP ----

#| classes: test
#### compare the saved numbers with those in the join, they should be the same ----
# names(fhier_acl_catch_by_species_state_region_waves)
fhier_acl_catch_by_species_state_region_waves %>%
  filter(scientific_name == test_species_name) %>%
# 
#   filter(species_itis == test_species_itis) %>%
  group_by(scientific_name, sa_gom) %>%
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_fhier_cnt) %>%
  identical(fhier_test_cnts$mackerel_fhier_cnt)

# acl_test_cnts
# fhier_test_cnts

fhier_acl_catch_by_species_state_region_waves %>%
  filter(scientific_name == test_species_name) %>%
  group_by(scientific_name, sa_gom) %>%
  summarise(mackerel_acl_cnt = sum(rec_acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  use_series(mackerel_acl_cnt) %>%
  identical(acl_test_cnts$mackerel_acl_cnt)

## 1a) SEDAR spp. lists ----
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

grep(
  "dolphin",
  fhier_acl_catch_by_species_state_region_waves$common_name_fhier,
  ignore.case = T,
  value = T
) %>% unique()
# [1] "DOLPHINFISH"      "DOLPHIN, POMPANO" "DOLPHIN"         

sa_top_spp <-
  fhier_spp %>%
  filter(common_name %in% sa_top)
# 11

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
  fhier_spp %>%
  filter(common_name %in% gom_top)

# glimpse(gom_top_spp)
# 11

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

dim(fhier_acl_catch_by_species_state_region_waves)
# Rows: 6,327
# Columns: 7
# [1] 5738   11

#| warning: false
## Separate data frames by region ----
fhier_acl_catch_by_species_state_region_waves_list <-
  fhier_acl_catch_by_species_state_region_waves %>%
  # split by sa_gom column
  split(as.factor(fhier_acl_catch_by_species_state_region_waves$sa_gom)) 
# %>%
#   # remove extra columns in each df
#   map(.f = list(. %>% dplyr::select(-one_of("year", "sa_gom"))))

# glimpse(fhier_acl_catch_by_species_state_region_waves_list)

## 2b) Top 12 ACL spp. ----
### GOM Top 12 ACL spp. ----
# names(acl_estimate_catch_by_species_state_region_waves)

# the common manipulation for both "GOM" and "SA"
get_acl_top_cnts <- function(my_df, top_num = 12) {
  my_df %>%
    select(new_sci, rec_acl_estimate_catch_by_4) %>%
    group_by(new_sci) %>%
    summarise(acl_count = sum(rec_acl_estimate_catch_by_4)) %>%
    # sort
    arrange(desc(acl_count)) %>%
    head(top_num) %>%
    return()
}

gom_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  filter(sa_gom == "gom") %>% 
  get_acl_top_cnts()

glimpse(gom_acl_top_spp)

# rename the column for future use
gom_acl_top_spp <-
  rename(gom_acl_top_spp, scientific_name = new_sci)

### SA Top 10 ACL spp. ----
sa_acl_top_spp <-
  acl_estimate_catch_by_species_state_region_waves %>%
  filter(sa_gom == "sa") %>%
  get_acl_top_cnts()

glimpse(sa_acl_top_spp)

# rename the column for future use
sa_acl_top_spp <-
  rename(sa_acl_top_spp, scientific_name = new_sci)

# 2) Data By wave and state ----
# str(fhier_catch_by_species_state_region_waves)
# str(acl_estimate_catch_by_species_state_region_waves)
# fhier_acl_catch_by_species_state_region_waves - has only common species

### split by region and state ----

fhier_acl_catch_by_species_state_region_waves_states_list <-
  fhier_acl_catch_by_species_state_region_waves_list %>%
  map(function(current_df) {
    # browser()
    current_df %>%
      split(as.factor(current_df$state))
  })

### split by state ----
# View(fhier_acl_catch_by_species_state_region_waves_states_list)
# fhier_acl_catch_by_species_state_region_waves_states_list <-
#   fhier_acl_catch_by_species_state_region_waves %>%
#   split(as.factor(fhier_acl_catch_by_species_state_region_waves$state))
# 
# # View(fhier_acl_catch_by_species_state_region_waves_states_list)
# 
### remove where is no rec acl count ----
remove_no_mrip_cnts <- function(my_df_list) {
  # browser()
  new_list <- c()
  
  my_names <- names(my_df_list)
  
  for (i in 1:length(my_names)) {
    # browser()
    current_name <- my_names[[i]]
    if (sum(my_df_list[[current_name]]$rec_acl_estimate_catch_by_4) > 0) {
      new_list[current_name] <- my_df_list[current_name]
    }
  }
  return(new_list)
  
}

# state_wave_has_rec_acl_data_list_new <- c()
# 
# my_st_names <- names(fhier_acl_catch_by_species_state_region_waves_states_list)
# 
# for (i in 1:length(my_st_names)) {
#   # browser()
#   state_abbr <- my_st_names[[i]]
#   if (sum(fhier_acl_catch_by_species_state_region_waves_states_list[[state_abbr]]$rec_acl_estimate_catch_by_4) > 0) {
#     state_wave_has_rec_acl_data_list_new[state_abbr] <- fhier_acl_catch_by_species_state_region_waves_states_list[state_abbr]
#   }
# }
# 
# View(state_wave_has_rec_acl_data_list_new)

# 3) Data By year and region ----
names(fhier_acl_catch_by_species_state_region_waves)

fhier_acl_catch_by_species_region_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
    select(
    species_itis_fhier,
    common_name_fhier,
    scientific_name,
    sa_gom,
    fhier_quantity_by_4,
    rec_acl_estimate_catch_by_4
  ) %>%
  group_by(scientific_name,
         sa_gom) %>%
  mutate(
    fhier_cnts_by_year_reg = sum(fhier_quantity_by_4),
    rec_acl_cnts_by_year_reg = sum(rec_acl_estimate_catch_by_4)
  ) %>%
  ungroup() %>%
  select(-c(fhier_quantity_by_4, rec_acl_estimate_catch_by_4)) %>%
  filter(!is.na(species_itis_fhier)) %>%
  unique()

# test, should be sa and gom, df 2 by 6
fhier_acl_catch_by_species_region_year %>%
  filter(scientific_name == "SCOMBEROMORUS MACULATUS") %>%
  glimpse()

## split by sa_gom ----
fhier_acl_catch_by_species_region_year_list <-
  fhier_acl_catch_by_species_region_year %>%
  ungroup %>%
  split(as.factor(fhier_acl_catch_by_species_region_year$sa_gom))

# test 167760 GROUPER, BLACK ----
fhier_acl_catch_by_species_region_year_list$sa %>%
  filter(species_itis_fhier == '167760') %>%
  glimpse()
# gom
# fhier_cnts_by_year_reg   <int> 2016
# rec_acl_cnts_by_year_reg <int> 1808
# sa
# fhier_cnts_by_year_reg   <int> 140
# rec_acl_cnts_by_year_reg <int> 262
fhier_acl_catch_by_species_region_year_list$gom %>%
  filter(species_itis_fhier == '167760') %>%
  glimpse()
# new file and sero only gom
# $ fhier_cnts_by_year_reg   <int> 1731
# $ rec_acl_cnts_by_year_reg <int> 1894
# new file and sero only sa
# $ fhier_cnts_by_year_reg   <int> 140
# $ rec_acl_cnts_by_year_reg <int> 259

acl_estimate_2022 %>%
  filter(new_sci == 'MYCTEROPERCA BONACI') %>%
  group_by(new_sci, new_moden, year, sub_reg) %>%
  summarise(GROUPER_BLACK_cnts_2022 = sum(ab1))
# same

# 4) Data By year and state ----

fhier_acl_catch_by_species_state_year <-
  fhier_acl_catch_by_species_state_region_waves %>%
  select(
    species_itis_fhier,
    scientific_name,
    common_name_fhier,
    state,
    fhier_quantity_by_4,
    rec_acl_estimate_catch_by_4
  ) %>%
  group_by(species_itis_fhier,
           scientific_name,
           common_name_fhier,
           state) %>%
  mutate(
    fhier_sum_cnts = sum(fhier_quantity_by_4),
    rec_acl_sum_cnts = sum(rec_acl_estimate_catch_by_4)
  ) %>%
  select(-c(fhier_quantity_by_4, rec_acl_estimate_catch_by_4)) %>%
  unique()

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

my_st_names <- names(state_year_has_rec_acl_data_list)

for (i in 1:length(my_st_names)) {
  # browser()
  state_abbr <- my_st_names[[i]]
  if (sum(fhier_acl_catch_by_species_state_year_list[[state_abbr]]$rec_acl_sum_cnts) > 0) {
    state_year_has_rec_acl_data_list_new[state_abbr] <- state_year_has_rec_acl_data_list[state_abbr]
  }
}
View(state_year_has_rec_acl_data_list_new)

# # state_year_has_rec_acl_data_list_new <-
# names(state_year_has_rec_acl_data_list) %>%
#   # repeat for each state
#   map(function(state_abbr) {
#     # get data for this state
#    if (sum(fhier_acl_catch_by_species_state_year_list[[state_abbr]]$rec_acl_sum_cnts) > 0) {
#     # browser()
#     # state_year_has_rec_acl_data_list[names(state_year_has_rec_acl_data_list) != state_abbr]# Remove list element
#      # state_year_has_rec_acl_data_list_new["MA"] <- fhier_acl_catch_by_species_state_year_list["MA"]
#    state_year_has_rec_acl_data_list_new[state_abbr] = state_year_has_rec_acl_data_list[state_abbr]
#      # return(state_year_has_rec_acl_data_list[[state_abbr]])
#    }
#   })

# View(state_year_has_rec_acl_data_list_new)
# str(fhier_acl_catch_by_species_state_year)
# str(fhier_acl_catch_by_species_state_year_list)

# source("~/R_code_github/compare_catch/spp_not_in_rec_acl.R")

