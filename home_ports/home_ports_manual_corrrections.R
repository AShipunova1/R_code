library(diffdf)
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
  head()
  # auxfunctions::see_res_in_outfile()
  
  # filter(
  #     str_detect(letters, "a|f|o")
  # )

xml_ports <-
  vessels_from_pims_short_ok |>
  dplyr::filter(stringr::str_detect(hailingport, ".*xml.*")) |> 
  glimpse()
  # auxfunctions::see_res_in_outfile()

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
  # auxfunctions::see_res_in_outfile()
  glimpse()

vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
  # filter(state_fixed == "AL, AL")
  filter(city_fixed == "APALIACHICOLA")

# get_vessel_id for typos ----
# corrected typos
get_vessel_id <- 
  my_read_xlsx(r"(C:\Users\anna.shipunova\Documents\R_code_github\home_ports\temp.xlsx)")

dim(get_vessel_id)
# 207

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

## add ids ----
# View(get_vessel_id_2)
# left_join(x, y[-2], by = "id_1") %>% 
#   left_join(y[-1], by = "id_2") %>% 
#   mutate(region = coalesce(region.x, region.y)) %>% 
#   select(-c(region.x, region.y))

# vessels_w_ports_3 <-
#   left_join(get_vessel_id_2,
#             join_by(hailingport == city_state_typo))

vessel_id_join1 <-
  vessels_from_pims_short_ok |>
  full_join(get_vessel_id_2, 
            join_by(hailingport == city_state_typo)) 

vessel_id_join1 |> dim()
# 22716     

vessel_id_join11 <-
  vessel_id_join1 |>
  filter(!is.na(city_state_change_to))

dim(vessel_id_join11)
# 1001

vessel_id_join2 <-
  vessels_from_pims_short_ok |>
  full_join(get_vessel_id_2, 
            join_by(hailingport == city_state_typo_space)) 

dim(vessel_id_join2)
# [1] 22896     9

vessel_id_join22 <-
  vessel_id_join2 |>
  filter(!is.na(city_state_change_to))

dim(vessel_id_join22)
# 216

print_df_names(vessel_id_join11)

setdiff(names(vessel_id_join11),
        names(vessel_id_join22))
# [1] "city_state_typo_space"

setdiff(names(vessel_id_join22),
        names(vessel_id_join11))
# [1] "city_state_typo"

vessel_id_join00 <-
  vessel_id_join11 |>
  full_join(
    vessel_id_join22,
    join_by(
      vessel_official_number,
      city,
      city_change_to,
      state_from,
      state_to,
      city_repeated,
      city_state_change_to
    ),
    relationship = "many-to-many"
  )

vessel_id_typo_correction <-
  vessel_id_join00 |>
  filter(!is.na(vessel_official_number)) |>
  mutate(hailingport_typo = coalesce(hailingport.x, hailingport.y)) |>
  select(vessel_official_number,
         hailingport_typo,
         city_state_change_to,
         city,
         state_from) |>
  filter(!hailingport_typo == city_state_change_to) |> 
  arrange(vessel_official_number) |> 
  rename("city_from" = "city")

n_distinct(vessel_id_typo_correction$hailingport_typo)
# 218

# see in csv
outfile <- tempfile(fileext = ".csv")

vessel_id_typo_correction |> 
  readr::write_csv(outfile)

file.show(outfile)
