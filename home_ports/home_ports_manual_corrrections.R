# check typos ----
# vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
#   dplyr::filter(vessel_official_number == "558651") |> 
#   View()
# 
# vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
#   dplyr::count(city_fixed) |> 
#   View()

# find vessel ids
typo_addresses <-
  c(
    "BAYOU LABATRE, AL",
    "CAROLINA BEACH, UN",
    "CHALESTON, SC",
    "CHAUVIN, LA, LA",
    "FERNADINA BCH, FL",
    "FORT MORGAN MARINA, AL",
    "GALLINANO, LA",
    "GEORGRTOWN, SC",
    "GULFSHORES, AL",
    "HILISBORO INLET, FL",
    "HOMOASSA, FL",
    "HOUMA LA, LA",
    "INTERCOASTAL CITY, LA",
    "ISLAMORADA, UN",
    "KEYWEST, FL",
    "LITTLE RIVERNHV1N4WH, SC",
    "LOXLEY AL, AL",
    "MADIERA BEACH, FL",
    "MAYPPORT, FL",
    "MCLELLANVILLE, SC",
    "MURELLS INLET, SC",
    "MURRELS INLET, SC",
    "NEW SMYMA BEACH, FL",
    "NEW SYMRNA BEACH, FL",
    "OCEEAN CITY, MD",
    "POINT PLEASANT NJ, NJ",
    "PORT CANVERAL, FL",
    "PORT O CANNOR, TX",
    "PORT OCONNOR, TX",
    "PORT ST.LUICE, FL",
    "PUNTA GORGA, FL",
    "RIVERIA BEACH, FL",
    "S PADRE ISLE, TX",
    "SEBASTAIN, FL",
    "ST AUGUSTIN, FL",
    "STEINAHTCHEE, FL",
    "SUMMRLND KEY, FL",
    "SWANQUARTER, FL",
    "TAVENIER, FL",
    "WANCHEESE, NC",
    "ALEXANDER CITY, AL, AL"
  )

# sink()
vessels_from_pims_short_ok |>
  # auxfunctions::print_df_names()
  dplyr::filter(hailingport %in% typo_addresses) |>
  dplyr::arrange(hailingport) |> 
  # head()
  auxfunctions::see_res_in_outfile()
  
  # filter(
  #     str_detect(letters, "a|f|o")
  # )

xml_ports <-
  vessels_from_pims_short_ok |>
  dplyr::filter(stringr::str_detect(hailingport, ".*xml.*")) |> 
  auxfunctions::see_res_in_outfile()

# find more not fixed cities ----

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |>
  dplyr::count(city_fixed) |>
  # dplyr::distinct()
  left_join(
    vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short,
    join_by(city_fixed)
  ) |>
  select(city_fixed, state_fixed, n) |>
  distinct() |>
  auxfunctions::see_res_in_outfile()
  # View()

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
  # filter(state_fixed == "AL, AL")
  filter(city_fixed == "APALIACHICOLA")

# get_vessel_id for typos ----
# corrected typos
get_vessel_id <- 
  my_read_xlsx(r"(C:\Users\anna.shipunova\Documents\R_code_github\home_ports\temp.xlsx)")

print_df_names(get_vessel_id)

get_vessel_id_1 <-
  get_vessel_id |>
  select(-x1, city_repeated)

get_vessel_id_2 <-
  get_vessel_id_1 |>
  dplyr::mutate(city = stringr::str_replace(city, "RINCÃ“N", "RINCÓN")) |>
  dplyr::mutate(city = stringr::str_replace(city, "CATAÃ‘O", "CATAÑO")) |>
  mutate(
    city_state_typo = paste(city, state_from, sep = ", "),
    city_state_change_to = paste(city_change_to, state_to, sep = ", "),
    city_state_typo_space = paste(city, state_from, sep = " , ")
  )

# find ids
vessels_w_ports_0 <-
  vessels_from_pims_short_ok |>
  dplyr::filter(hailingport %in% get_vessel_id_2$city_state_typo) |>
  dplyr::arrange(hailingport)

# dplyr::left_join(get_vessel_id_2,
  #                  dplyr::join_by(hailingport == city_state_typo))
# head()
  # auxfunctions::see_res_in_outfile()

n_distinct(vessels_w_ports_0$hailingport)
# 200
n_distinct(get_vessel_id_2$city_state_typo)
# 207

not_found <-
  dplyr::setdiff(unique(get_vessel_id_2$city_state_typo),
                 unique(vessels_w_ports_0$hailingport)) |> 
  sort()

# [1] "NEW SYMNRA, FL"               "OCEAN SPRIGS, MS"            
# [3] "PORT CANAVERAL CANAVERAL, FL" "PORT ST. JOHN, FL"           
# [5] "RINCÓN, PR"                   "ROCK PORT, TX"               
# [7] "SOUTH PORT, NC"              

dplyr::setdiff(unique(vessels_w_ports_0$hailingport),
               unique(get_vessel_id_2$city_state_typo)) |> 
  sort()
# 0

View(get_vessel_id_2)

not_found_1 <-
  not_found |>
  stringr::str_split(pattern = ", ") |>
  purrr::map( ~ stringr::str_c(., collapse = " , "))

vessels_w_ports_1 <-
  vessels_from_pims_short_ok |>
  dplyr::filter(hailingport %in% not_found_1) |>
  dplyr::arrange(hailingport)


# get_vessel_id_5 <- 
#   rbind(get_vessel_id_3, get_vessel_id_4)

# check missing again
# not_found_2 <-

  dplyr::setdiff(unique(stringr::str_squish(get_vessel_id_2$city_state_typo)),
                 unique(stringr::str_squish(vessels_w_ports_1$hailingport))) |>
  sort()
  
  
# dplyr::left_join(get_vessel_id_2,
  #                  dplyr::join_by(hailingport == city_state_typo))
# head()
  # auxfunctions::see_res_in_outfile()

get_vessel_id_5
  