##| echo: false

source("~/R_code_github/compare_catch/auxiliary/get_data.R")

## prepare FHIER data ----

### use only entries with sero_vessel_permit ----

logbooks_content_sero <-
  logbooks_content %>%
  dplyr::filter(!is.na(sero_vessel_permit))
# dim(logbooks_content)
# 346022
# dim(logbooks_content_sero)
# 275734

## get column names vars ----
# There are different formats in different available files.
# Find a column name with "itis" in it
itis_field_name <- grep("itis", names(logbooks_content_sero), value = T)
# catch_species_itis

# Same for "vessel.*official"
vessel_id_field_name <-
  grep("vessel.*official", names(logbooks_content_sero), value = T)
# vessel_official_nbr

## Fix dates ----

fhier_logbooks_content <-
  logbooks_content_sero %>%
  # create a new column
  dplyr::mutate(trip_start_date_time =
    # trip start: combine a date without time, a space and a time
    paste(substr(trip_start_date, 1, 10),
    trip_start_time)) %>%
  # Same for the trip end
  dplyr::mutate(trip_end_date_time = paste(substr(trip_end_date, 1, 10), trip_end_time)) %>%
  # change the new column types to a date
  change_to_dates("trip_start_date_time", "%Y-%m-%d %H%M") %>%
  change_to_dates("trip_end_date_time", "%Y-%m-%d %H%M") %>%
  # change the column type to a number
  dplyr::mutate(reported_quantity = as.integer(reported_quantity))

# view
fhier_logbooks_content %>% dplyr::select(starts_with("trip")) %>% str()

fhier_logbooks_content_date_fixed_tmp <-
  fhier_logbooks_content %>%
  # if a "trip_end_date" is before 2020 - use "notif_trip_end_date" column instead
  dplyr::mutate(trip_end_date1 = ifelse(
    trip_end_date < "2020-01-01",
    notif_trip_end_date,
    trip_end_date
  ))

fhier_logbooks_content_date_fixed <-
  fhier_logbooks_content_date_fixed_tmp %>%
  # manually change the wrong value
  dplyr::mutate(trip_end_date2 = ifelse(
    # find it
    grepl("1992", fhier_logbooks_content_date_fixed_tmp$trip_end_date1),
    # change it
    "2022-10-16 01:00:00",
    # don't change anything else
    trip_end_date1
  ))

fhier_logbooks_content_date_fixed_2022 <-
  fhier_logbooks_content_date_fixed %>%
  dplyr::filter(year(trip_end_date) == "2022")

fhier_logbooks_content_waves <-
  fhier_logbooks_content_date_fixed_2022 %>%
  # add a new column with a trip end Month
  dplyr::mutate(end_month = as.yearmon(trip_end_date2)) %>%
  # add a new column with a trip end Year
  dplyr::mutate(end_year =
           year(trip_end_date2)) %>%
  # add a new column with a number for each trip end Month
  dplyr::mutate(end_month_num = month(trip_end_date2)) %>%
  # add a new column with a Wave
  dplyr::mutate(end_wave  = floor((end_month_num + 1) / 2))

#| classes: test
# test: show the new columns ----
fhier_logbooks_content_waves %>%
  dplyr::select(end_month, end_year, end_month_num, end_wave) %>%
  unique() %>%
  # sort by end_month_num
  dplyr::arrange(end_month_num)

# Florida counties by region (from the Internet) ----
fl_counties <- list(
  "SA" = c(
    "Brevard",
    "Broward",
    "Duval",
    "Flagler",
    "Indian River",
    "Martin",
    "Miami-Dade",
    "Nassau",
    "Palm Beach",
    "St. Johns",
    "St. Lucie",
    "Volusia"
  ),
  "GOM" = c(
    "Bay",
    "Charlotte",
    "Citrus",
    "Collier",
    "Dixie",
    "Escambia",
    "Franklin",
    "Gulf",
    "Hernando",
    "Hillsborough",
    "Lee",
    "Levy",
    "Manatee",
    "Monroe",
    "Okaloosa",
    "Pasco",
    "Pinellas",
    "Santa Rosa",
    "Sarasota",
    "Taylor",
    "Wakulla",
    "Walton"
  )
)

fhier_logbooks_content_waves_fl_county <-
  fhier_logbooks_content_waves %>%
  # create a new column "end_port_fl_reg" with SA, GOM or whatever else left
  dplyr::mutate(
    end_port_fl_reg = dplyr::case_when(
      # check in the list
      # if there is no end county, use the start
      fix_names(start_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(start_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      fix_names(end_port_county) %in% fix_names(fl_counties$SA) ~ "sa",
      fix_names(end_port_county) %in% fix_names(fl_counties$GOM) ~ "gom",
      # if not on the list - keep it
      .default = end_port_county
    )
  )

## test: check regions ----
fhier_logbooks_content_waves_fl_county %>%
  # get FL only
  dplyr::filter(end_port_state == "FL") %>%
  # sort by county
  dplyr::arrange(end_port_county) %>%
  dplyr::distinct() %>%
  # data_overview()
  # 37 counties
  # vessel_official_number          1096
  dplyr::select(end_port_fl_reg) %>%
    table()
# using only end_port_counties
       #    gom NOT-SPECIFIED            sa 
       # 201559           188         30220 
# using a start_port_county where there is no end_port_county
#           gom NOT-SPECIFIED            sa 
#        201703           112         30152 
  # what else is in the new column beside sa and gom
  # dplyr::filter(!(end_port_fl_reg %in% c("sa", "gom"))) %>% unique()

# NOT-SPECIFIED

## Other states to regions ----
# list of states in the South Atlantic region
# https://safmc.net/about/#:~:text=The%20South%20Atlantic%20Council%20is,east%20Florida%20to%20Key%20West
# The South Atlantic Council is responsible for the conservation and management of fishery resources in federal waters ranging from 3 to 200 miles off the coasts of North Carolina, South Carolina, Georgia, and east Florida to Key West.

states_sa <- data.frame(
  state_name = c(
    # "Delaware",
    # "District of Columbia",
    # "Florida", # exclude, we have it separated by county
    "Georgia",
    # "Maryland",
    "North Carolina",
    "South Carolina"
    # ,
    # "Virginia",
    # "West Virginia"
  )
)

sa_state_abb <-
  # a default R table
  state_tbl %>%
  # get only these in our list
  dplyr::filter(state_name %in% tolower(states_sa$state_name)) %>%
  # get abbreviations
  dplyr::select(state_abb)

fhier_logbooks_content_waves__sa_gom <-
  fhier_logbooks_content_waves_fl_county %>%
  # add a new column "end_port_sa_gom" with sa or gom for each state
  # use fix_name aux function to unify state names (lower case, no spaces etc.)
  dplyr::mutate(end_port_sa_gom = dplyr::case_when(
    # if a name is in our SA list - "sa", otherwise - "gom"
    fix_names(end_port_state) %in% fix_names(sa_state_abb$state_abb) ~ "sa",
    .default = "gom"
  )) %>%
  # go through the new column again
  # if an end port state is Florida - use the region from the previous step (column "end_port_fl_reg")
  # otherwise don't change
  dplyr::mutate(end_port_sa_gom = ifelse(
    tolower(end_port_state) == "fl",
    end_port_fl_reg,
    end_port_sa_gom
  )) %>%
  # remove this column, we don't need it anymore
  dplyr::select(-end_port_fl_reg)

#| classes: test
## test: states and regions ----
fhier_logbooks_content_waves__sa_gom %>%
  # look at states and regions
  dplyr::select(end_port_state, end_port_sa_gom) %>%
  unique() %>%
  dplyr::glimpse()

# dplyr::glimpse(fhier_logbooks_content_waves__sa_gom)

## add scientific names ----
# grep("sci", names(fhier_logbooks_content_waves__sa_gom_dolph), value = T, ignore.case = T)

# grep("sci", names(fhier_logbooks_content), value = T, ignore.case = T)

sefhier_spp <-
  sefhier_sp_all %>%
  dplyr::select(species_itis, scientific_name, common_name) %>%
  unique()

# dplyr::glimpse(sefhier_spp)
# 736

fhier_logbooks_content_waves__sa_gom %<>%
  # rename a column
  rename(species_itis = catch_species_itis)

# mrip_spp_2022 %<>%
  # dplyr::mutate(scientific_name_mrip = toupper(new_sci))
# names(fhier_logbooks_content_waves__sa_gom)
# grep("common", names(fhier_logbooks_content_waves__sa_gom), value = T, ignore.case = T)

#### add scientific names to fhier data ----
# fhier_logbooks_content_waves__sa_gom
# grep("sci", names(fhier_logbooks_content_waves__sa_gom), value = T, ignore.case = T) %>%
#   unique()
# 0

fhier_catch_by_species_state_region_waves_w_spp <-
full_join(fhier_logbooks_content_waves__sa_gom,
          sefhier_spp,
          by = join_by(species_itis, common_name)) 
# some common_names have 2 species_itis, e.g. GRUNT, WHITE

# check if sci name is a NA
fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(is.na(scientific_name)) %>%
  dplyr::select(species_itis, common_name, reported_quantity) %>% 
  dplyr::group_by(species_itis, common_name) %>% 
  summarise(sum_cnt = sum(reported_quantity)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(sum_cnt)) %>%
  head()
# all permits:
#   species_itis common_name             sum_cnt
#   <chr>        <chr>                     <int>
# 1 169059       GRUNT, WHITE              69394
# 2 172734       FLOUNDERS, PARALICHTHYS    3050
# SERO permit
# 1 172734       FLOUNDERS, PARALICHTHYS    3007
# Taxonomic Serial No.: 172734
# Genus	Paralichthys Girard, 1858 – Summer flounders, southern flounders
# many spp
# https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=172734#null

#### check DOLPHIN ----
grep("DOLPHIN", fhier_catch_by_species_state_region_waves_w_spp$common_name, value = T, ignore.case = T) %>%
  unique()

# 3 in FHIER
fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(grepl("DOLPHIN", fhier_catch_by_species_state_region_waves_w_spp$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name      species_itis scientific_name     
# 1 DOLPHINFISH      168790       CORYPHAENA          
# 2 DOLPHIN          168791       CORYPHAENA HIPPURUS 
# 3 DOLPHIN, POMPANO 168792       CORYPHAENA EQUISETIS

# 1 in MRIP
grep("CORYPHAENA", acl_estimate$new_sci, value = T, ignore.case = T) %>%
  unique()
# [1] "Coryphaena hippurus"
acl_estimate %>%
  dplyr::filter(grepl("CORYPHAENA", acl_estimate$new_sci, ignore.case = T)) %>%
  dplyr::select(new_sci, new_com, itis_code) %>%
  unique()
#   new_sci             new_com itis_code
# 1 Coryphaena hippurus dolphin NA       
# 2 Coryphaena hippurus dolphin 168791

### combine all dolphins for FHIER data ----
fhier_catch_by_species_state_region_waves_w_spp_dolph_com_name <-
  fhier_catch_by_species_state_region_waves_w_spp %>%
  # "save" the original columns
  rename(
    common_name_orig = common_name,
    scientific_name_orig = scientific_name,
    species_itis_orig = species_itis
  ) %>%
  # rename all DOLPHINs to "DOLPHIN"
  dplyr::mutate(common_name =
           dplyr::case_when(startsWith(tolower(common_name_orig), "dolphin") ~ "DOLPHIN",
                     .default = common_name_orig)) %>%
  # rename scientific names as in MRIP
  dplyr::mutate(
    scientific_name =
      dplyr::case_when(
        startsWith(tolower(scientific_name_orig),
                   "coryphaena") ~ "CORYPHAENA HIPPURUS",
        .default = scientific_name_orig
      )
  ) %>%
  # if "dolphin" - change itis to the one in MRIP
  dplyr::mutate(species_itis =
           dplyr::case_when(startsWith(tolower(common_name_orig), "dolphin") ~ "168791",
                     .default = species_itis_orig))

# dplyr::glimpse(fhier_catch_by_species_state_region_waves_w_spp_dolph_com_name)

### test: dolphins to ensure they now have the same common name in new "common_name" col----
fhier_catch_by_species_state_region_waves_w_spp_dolph_com_name %>%
  # dplyr::filter(tolower(common_name_orig) %in% c("dolphin", "dolphinfish")) %>%
  dplyr::filter(startsWith(tolower(common_name_orig), "dolphin")) %>%
  dplyr::select(species_itis, common_name_orig, common_name, scientific_name) %>% unique()

# remove orig columns
fhier_catch_by_species_state_region_waves_w_spp <-
fhier_catch_by_species_state_region_waves_w_spp_dolph_com_name %>%
  dplyr::select(-ends_with("_orig"))
# %>%
# # combine counts for DOLPHIN
#   dplyr::group_by(scientific_name) %>%
#   summarise(new_cnts = sum(fhie))

### add sci name for FLOUNDERS, PARALICHTHYS for FHIER data ----

fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::filter(grepl("FLOUNDERS", fhier_catch_by_species_state_region_waves_w_spp$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name             species_itis scientific_name
# 1 FLOUNDERS, PARALICHTHYS 172734       NA             

fhier_logbooks_content_waves__sa_gom_fla <-
  fhier_catch_by_species_state_region_waves_w_spp %>%
  dplyr::mutate(scientific_name = ifelse(
    is.na(scientific_name) & species_itis == "172734",
    "PARALICHTHYS",
    scientific_name
  ))

#### test: FLOUNDERS ----
fhier_logbooks_content_waves__sa_gom_fla %>%
  dplyr::filter(grepl("FLOUNDERS", fhier_logbooks_content_waves__sa_gom_fla$common_name, ignore.case = T)) %>%
  dplyr::select(common_name, species_itis, scientific_name) %>%
  unique()
#   common_name             species_itis scientific_name
# 1 FLOUNDERS, PARALICHTHYS 172734       PARALICHTHYS   

## calculate catch ----
# names(fhier_logbooks_content_waves__sa_gom_dolph)
fhier_catch_by_species_state_region_waves <-
  fhier_logbooks_content_waves__sa_gom_fla %>%
  # dplyr::select only relevant columns
  dplyr::select(
    scientific_name,
    species_itis,
    common_name,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave,
    reported_quantity
  ) %>%
  # group by all of them but "reported_quantity"
  dplyr::group_by(
    scientific_name,
    species_itis,
    common_name,
    end_port_state,
    end_port_sa_gom,
    end_year,
    end_wave
  ) %>%
  # save a sum of reported_quantity in each group in fhier_quantity_by_4
  # remove NAs
  summarise(fhier_quantity_by_4 = sum(as.integer(reported_quantity),
                                      na.rm = TRUE)) %>%
  as.data.frame()

# data_overview(fhier_catch_by_species_state_region_waves)
# names(fhier_logbooks_content)

#| classes: test
### test: cnts for 1 sp. ----
test_species_name <-
  fhier_catch_by_species_state_region_waves %>%
  # dplyr::filter(tolower(common_name) == "mackerel, spanish") %>%
  dplyr::filter(tolower(scientific_name) == "scomberomorus maculatus") %>%
  dplyr::select(scientific_name) %>%
  unique() %>%
  # get a string, not a df
  use_series(scientific_name)

fhier_test_cnts <-
  fhier_catch_by_species_state_region_waves %>%
  # get the same species
  dplyr::filter(scientific_name == test_species_name) %>%
  # group by region
  dplyr::group_by(scientific_name, end_port_sa_gom) %>%
  # sum the FHIER catch
  summarise(mackerel_fhier_cnt = sum(fhier_quantity_by_4, na.rm = TRUE)) %>%
  as.data.frame()

# source("~/R_code_github/compare_catch/compare_catch_fhier_q.R")

## rec ACL / MRIP ----

# from get_data.R
acl_estimate %<>%
  # using ab1 for catch counts
  # convert to numbers
  dplyr::mutate(ab1 = as.integer(ab1))

# str(acl_estimate)

### MRIP data dplyr::filtering ----
acl_estimate_2022 <-
  acl_estimate %>%
  dplyr::filter(year == "2022") %>%
  # dplyr::filtering here for just SA (6) and Gulf (7) sub regions
  dplyr::filter(sub_reg %in% c(6, 7)) %>%
  # Exclude the SRHS survey according to Dominique and Mike May 1
  dplyr::filter(!(ds == "SRHS")) %>%
  # dplyr::select(new_mode) %>% unique()
  # the "new_mode" column only has options 1,3 & 4 remaining
  # -	New variable ‘agg_moden’ divides all estimates into for-hire (cbt, hbt, or cbt/hbt) or private (private or shore) mode fishing
  # new_mode	recoded mode of fishing used by SFD (1=shore, 2=headboat, 3=charterboat, 4=private boat, 5=charter/headboat, 6=priv/shore)
  # new_moden		alpha description of ‘new_mode’
  dplyr::filter(new_mode %in% c(2, 3, 5))

# View(acl_estimate)
dim(acl_estimate)
# [1] 1442   67
# [1] 347379 67
# new file
# [1] 372065     69
# 
dim(acl_estimate_2022)
# 8332
# 1442   
# new file
# [1] 2088   69

names(acl_estimate_2022)
# data_overview(acl_estimate_2022)
# new_sci            77

## change_case for scientific_names ----
acl_estimate_2022 %<>%
  dplyr::mutate(new_sci = toupper(new_sci))

#### check FL sa_gom ----
acl_estimate_2022 %>%
  dplyr::select(new_sta, sub_reg, fl_reg) %>% unique() %>%
  dplyr::filter(new_sta %in% c("FLE", "FLW"))
#   new_sta sub_reg fl_reg
# 1 FLE     6       5     
# 2 FLE     6       4     
# 3 FLW     7       1     
# 4 FLW     7       3     
# 5 FLW     7       2     

### change FLE and FLW to FL ----
# to compare with FHIER

acl_estimate_2022 %<>%
  dplyr::mutate(state = dplyr::case_when(new_sta %in% c("FLE", "FLW") ~ "FL",
                           .default = new_sta))

## Get MRIP counts ----
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_2022 %>%
  # dplyr::select the relevant columns only
  dplyr::select(new_sci,
         itis_code,
         new_com,
         state,
         sub_reg,
         # fl_reg,
         year,
         wave,
         ab1) %>%
  # group by all except the counts
  dplyr::group_by(new_sci,
           itis_code,
           new_com,
           state,
           sub_reg,
           # fl_reg,
           year,
           wave) %>%
  # save the sum of "ab1" for each group in "rec_acl_estimate_catch_by_4"
  # remove NAs
  summarise(rec_acl_estimate_catch_by_4 = sum(as.integer(ab1), na.rm = TRUE)) %>%
  # back to an ungrouped form
  as.data.frame()

glimpse(acl_estimate_catch_by_species_state_region_waves)
# 'data.frame':	878 obs. of  6 variables
# new file
# Rows: 1,244 with fl_reg
# Rows: 968

# "year" and "wave" to numbers
acl_estimate_catch_by_species_state_region_waves1 <-
  acl_estimate_catch_by_species_state_region_waves %>%
  dplyr::mutate(year = as.double(year)) %>%
  dplyr::mutate(wave = as.double(wave))

### change regions to the same format as in FHIER ----
acl_estimate_catch_by_species_state_region_waves <-
  acl_estimate_catch_by_species_state_region_waves1 %>%
  # change a 6 to "sa" and a 7 "gom", leave everything else in place
  dplyr::mutate(sa_gom = dplyr::case_when(sub_reg == "6" ~ "sa",
                            sub_reg == "7" ~ "gom",
                            .default = sub_reg),
                            # put the new column after sub_reg (by default at the end)
                            .after = sub_reg) %>%
  # drop sub_reg
  dplyr::select(-sub_reg)


### make a test acl one sp. var ----
# names(acl_estimate_catch_by_species_state_region_waves)
acl_test_cnts <-
  acl_estimate_catch_by_species_state_region_waves %>%
  # get one species
  dplyr::filter(tolower(new_sci) == "scomberomorus maculatus") %>%
  # group by region
  dplyr::group_by(new_sci, sa_gom) %>%
  # sum the ACL catch
  summarise(mackerel_acl_cnt = sum(rec_acl_estimate_catch_by_4, na.rm = TRUE)) %>%
  as.data.frame()

## Rename fields to compare to FHIER ----

#### FHIER names ----
names(fhier_catch_by_species_state_region_waves) %>%
  paste0(collapse = "', '")

fhier_names <-
  c(
    'scientific_name',
    'species_itis',
    'common_name',
    'end_port_state',
    'end_port_sa_gom',
    'end_year',
    'end_wave',
    'fhier_quantity_by_4'
  )

#### MRIP names ----
names(acl_estimate_catch_by_species_state_region_waves) %>%
  paste0(collapse = "', '")

acl_names <- c(
  'new_sci',
  'itis_code',
  'new_com',
  'state',
  'sa_gom',
  # 'fl_reg',
  'year',
  'wave',
  'rec_acl_estimate_catch_by_4'
)

#### common field names ----
wave_data_names_common <- c(
    'scientific_name',
    'species_itis',
    'common_name',
    "state",
    "sa_gom",
    "year",
    "wave"
)

acl_names_len_to_change <- length(acl_names) - 1

acl_estimate_catch_by_species_state_region_waves_renamed <-
  acl_estimate_catch_by_species_state_region_waves %>%
  rename_at(vars(acl_names[1:acl_names_len_to_change]),
            function(x) wave_data_names_common[1:acl_names_len_to_change])

fhier_names_len_to_change <- length(fhier_names) - 1

fhier_catch_by_species_state_region_waves_renamed <-
  fhier_catch_by_species_state_region_waves %>%
  rename_at(vars(fhier_names[1:fhier_names_len_to_change]),
            function(x)
              wave_data_names_common[1:fhier_names_len_to_change])

### rename fields in the test variables ----
names(fhier_test_cnts) <- c("scientific_name", "sa_gom", "mackerel_fhier_cnt")

# names(acl_test_cnts)
# "new_sci"          "sa_gom"           "mackerel_acl_cnt"

### test: rename fields ----
names(fhier_catch_by_species_state_region_waves)
names(acl_estimate_catch_by_species_state_region_waves)
identical(names(fhier_catch_by_species_state_region_waves_renamed)[1:7],
          names(acl_estimate_catch_by_species_state_region_waves_renamed)[1:7])
# T

