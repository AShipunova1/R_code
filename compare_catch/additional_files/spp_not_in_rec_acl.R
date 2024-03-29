# run from compare_catch.R
# see "~\R_files_local\my_outputs\compare_catch\spp_research\not_in_rec_acl.xlsx"
# 3c) All FHIER spp with large catch which is not in rec ACL ----
# View(fhier_acl_catch_by_species_state_region_waves)
spp_cnts_in_fhier_not_in_acl <-
  fhier_acl_catch_by_species_state_region_waves %>%
  dplyr::select(
    species_itis_fhier,
    # species_itis_mrip,
    common_name_fhier,
    # common_name_mrip,
    scientific_name,
    sa_gom,
    fhier_quantity_by_4,
    rec_acl_estimate_catch_by_4
  ) %>%
  dplyr::group_by(scientific_name,
           sa_gom) %>%
  dplyr::mutate(
    fhier_cnts = sum(fhier_quantity_by_4),
    rec_acl_cnts = sum(rec_acl_estimate_catch_by_4)
  ) %>%
  dplyr::select(-c(fhier_quantity_by_4, rec_acl_estimate_catch_by_4)) %>%
  unique() %>%
  dplyr::filter(rec_acl_cnts == 0) %>%
  dplyr::select(-c(rec_acl_cnts)) %>%
  dplyr::filter(!(fhier_cnts == 0)) %>%
    dplyr::arrange(desc(fhier_cnts)) %>%
  dplyr::ungroup()
# %>%
# head(20)
# dplyr::select(common_name)
# dplyr::ungroup() %>%
# str()
# 628

# View(spp_cnts_in_fhier_not_in_acl)
# 908
  # dplyr::filter(!(fhier_cnts == 0)) %>%
# 592
# without
    # species_itis_mrip,
    # common_name_mrip,
# 581

# write_csv(spp_cnts_in_fhier_not_in_acl, "spp_cnts_in_fhier_not_in_acl.csv")

# dplyr::glimpse(spp_cnts_in_fhier_not_in_acl)

# test
acl_estimate_catch_by_species_state_region_waves_renamed %>%
  dplyr::filter(toupper(scientific_name) == "SCOMBER COLIAS")

acl_estimate_2022 %>%
  dplyr::filter(toupper(new_sci) == "SCOMBER COLIAS") %>% dim()
0

acl_estimate_2022 %>%
  dplyr::filter(toupper(new_sci) == "MICROPOGONIAS UNDULATUS") %>% dim()
0

# spp from separate ds ----
fhier_catch_spp <-
  fhier_catch_by_species_state_region_waves %>%
  dplyr::select(scientific_name, common_name, species_itis, fhier_quantity_by_4) %>%
  dplyr::group_by(scientific_name) %>%
  summarise(fhier_cnts = sum(fhier_quantity_by_4)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(fhier_cnts)) %>%
  unique()

head(fhier_catch_spp, 5)
# %>% tail(5)


# View(acl_estimate_2022)

rec_acl_estimate_2022_spp <-
  acl_estimate_2022 %>%
  dplyr::select(new_sci, new_com, itis_code, ab1) %>%
  dplyr::group_by(new_sci) %>%
  summarise(rec_acl_cnts = sum(ab1)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(rec_acl_cnts))

head(rec_acl_estimate_2022_spp, 5)

# View(acl_species_list[[1]])
# View(acl_estimate_2022)

spp_join <-
  full_join(fhier_catch_spp, rec_acl_estimate_2022_spp,
          join_by(scientific_name == new_sci),
          # keep all cols
          keep = T,
          # na are not equal
          na_matches = "never")

# spp_join %>%
#   dplyr::filter(is.na(species_itis))

glimpse(spp_join)

## spp by fhier count if not in rec acl ----
not_in_rec_acl <-
  spp_join %>%
  dplyr::arrange(desc(fhier_cnts)) %>%
  dplyr::filter(is.na(scientific_name))

str(not_in_rec_acl)
# 5

fhier_catch_spp %>%
  dplyr::filter(grepl('GRUNT, WHITE', common_name))
 # 1 GRUNT, WHITE                613026           104954
 # 2 GRUNT, WHITE                169059            69394

# not_in_rec_acl$common_name %>%
#   purrr::map(function(x) {
#     browser()
#     # grep("(.)"x)
#   })
  
# run once
# write_csv(not_in_rec_acl, "not_in_rec_acl.csv")

rec_acl_estimate_2022_spp %>%
  dplyr::filter(grepl('dolphin', new_com))

rec_acl_estimate_2022_spp %>%
  dplyr::filter(grepl('atlantic croaker', new_com))

rec_acl_spp_full <- dplyr::select(acl_estimate,
       c(species,
       species_code,
       sp_code,
       new_com,
       new_sci,
       itis_code))

# names(acl_species_list[[1]])
# acl_species_list[[1]] %>%
  # dplyr::filter(grepl('SCOMBER', SCIENTIFIC_NAME)) %>% View()

rec_acl_spp <-
  rec_acl_spp_full %>%
  full_join(acl_species_list[[1]],
            join_by(sp_code),
            keep = T) %>%
  unique()
  
# View(rec_acl_spp)
rec_acl_spp %>%
  # dplyr::filter(is.na(sp_code.x))
  dplyr::filter(is.na(sp_code.x) | is.na(sp_code.y))
# A tibble: 1,682 × 16
# A tibble: 1,683 × 16

rec_acl_spp %>%
  dplyr::filter(grepl('ATLANTIC CROAKER', COMMON_NAME))
# sp_code.x == NA

# GRUNTS, HAEMULIDAE (FAMILY)
rec_acl_spp %>%
  dplyr::filter(grepl('GRUNT', COMMON_NAME)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Micropogon', new_sci, ignore.case = T))
# %>%
#   View()

rec_acl_spp %>%
  dplyr::filter(grepl('TUNA', COMMON_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('ATLANTIC', new_com, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('sard', new_com, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Pleuronectes', SCIENTIFIC_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Paralichthys', SCIENTIFIC_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('flounder', COMMON_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('summer', new_com, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Trachipteridae', SCIENTIFIC_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('seatrout', COMMON_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('spotted', new_com, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Cynoscion', SCIENTIFIC_NAME, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('Cynoscion', new_sci, ignore.case = T)) %>%
  View()

rec_acl_spp %>%
  dplyr::filter(grepl('triggerf', new_com, ignore.case = T)) %>%
  View()

# triggerfishes
# filefishes

rec_acl_spp %>%
  dplyr::filter(grepl('filefi', new_com, ignore.case = T)) %>%
  View()
# 0

# rec_acl_spp %>%
#   dplyr::filter(grepl('triggerf', new_com, ignore.case = T)) %>%
#   dplyr::select(new_com, new_sci, itis_code, FAMILY, GROUP_NAME) %>%
#   write_csv(
#     "temp.csv"
#   )

spp_join %>%
  # dplyr::filter(itis_code == '173138')
  dplyr::filter(itis_code == '173139')

# Rhizoprionodon
rec_acl_spp %>%
  dplyr::filter(grepl('Rhizoprionodon', SCIENTIFIC_NAME, ignore.case = T)) %>%
  View()

# ---

names(acl_species_list[[1]])
# str(spp_cnts_in_fhier_not_in_acl)
grep("ATLANTIC.*MACKEREL", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "ATLANTIC MACKEREL"
grep("ATLANTIC.*CROAKER", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "ATLANTIC CROAKER"
grep("RIBBONFISH", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "RIBBONFISH FAMILY"    "TAPERTAIL RIBBONFISH" "POLKA-DOT RIBBONFISH"
# [4] "SCALLOPED RIBBONFISH"
grep("GRUNT", acl_species_list[[1]]$COMMON_NAME, value = T)
#  [1] "GRUNT SCULPIN"     "GRUNT FAMILY"      "GRUNT GENUS"      
#  [4] "WHITE GRUNT"       "CAESAR GRUNT"      "SMALLMOUTH GRUNT" 
#  [7] "FRENCH GRUNT"      "SPANISH GRUNT"     "BLUESTRIPED GRUNT"
# [10] "STRIPED GRUNT"     "BARRED GRUNT"      "BURRO GRUNT"      

grep("BLACKFIN.*TUNA", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "BLACKFIN TUNA"
grep("DOLPHIN", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "DOLPHIN FAMILY"  "DOLPHIN GENUS"   "DOLPHIN"         "POMPANO DOLPHIN"
grep("SUMMER.*FLOUNDER", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "SUMMER FLOUNDER"
grep("SCUP", acl_species_list[[1]]$COMMON_NAME, value = T)
# [1] "SCUP"
# 169182       

# acl_estimate
acl_species_list[[1]] %>%
  str()

# TODO compare species_itis if names are similar
# acl_species_list[[1]] %>%
acl_estimate_2022 %>%
  dplyr::filter(new_com == "scup") %>%
  dplyr::select(itis_code) %>%
  unique()
# 169182   

acl_estimate_2022 %>%
  dplyr::filter(new_com == "scup") %>%
  dplyr::count(ab1)

names(acl_estimate_2022)

# white grunt 2 ITIS ----
grep("permit", names(fhier_logbooks_content), value = T) %>% paste0(collapse = ", ")

# fhier_logbooks_content %>%
#   dplyr::filter(catch_species_itis == "169059") %>%
#   dplyr::select(trip_id, supplier_trip_id, trip_start_date_time, trip_de, accsp_permit_license_nbr, sero_vessel_permit, garfo_vessel_permit, dea_permit_id) %>%
#   unique() %>% 
#   dplyr::arrange(trip_de, trip_start_date_time) %>%
#   write_csv("169059_itis_date_permit.csv")


grep("GRUNT", acl_species_list[[1]]$COMMON_NAME, value = T)
#  [1] "GRUNT SCULPIN"     "GRUNT FAMILY"      "GRUNT GENUS"      
#  [4] "WHITE GRUNT"       "CAESAR GRUNT"      "SMALLMOUTH GRUNT" 
#  [7] "FRENCH GRUNT"      "SPANISH GRUNT"     "BLUESTRIPED GRUNT"
# [10] "STRIPED GRUNT"     "BARRED GRUNT"      "BURRO GRUNT"      

grep("GRUNT", toupper(fhier_common_names$common_name), value = T)
fhier_common_names %>%
  dplyr::filter(grepl("GRUNT", toupper(common_name))) %>%
  unique() %>% dplyr::glimpse()
# 12


# names(acl_species_list[[1]])
acl_species_list[[1]] %>%
  dplyr::filter(grepl("GRUNT", toupper(COMMON_NAME))) %>%
  dplyr::select(sp_code, COMMON_NAME) %>%
  unique() %>% dplyr::glimpse()
# 12

fhier_logbooks_content %>%
  dplyr::filter(grepl("GRUNT", toupper(common_name))) %>%
    dplyr::select(catch_species_itis, common_name) %>%
  unique() %>% write_csv("fhier_grunt_temp.csv") 
  # head(12)


# names(acl_estimate_2022)
acl_estimate_2022 %>%
  dplyr::filter(grepl("GRUNT", toupper(new_com))) %>%
    dplyr::select(itis_code, new_com) %>%
  unique() %>% write_csv("acl_grunt_temp.csv")
  # head()
# 4
#   itis_code new_com          
#   <chr>     <chr>            
# 1 169069    bluestriped grunt
# 2 169065    french grunt     
# 3 169056    grunt family     
# 4 169059    white grunt      

