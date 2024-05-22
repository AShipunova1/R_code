mylongtext <- 
  "asfds9v  sd9fvzsdxvmzx.,3q rweafzxcv0qawwer sdv\nq3REFXZDCV,1Q"

see_outfile(mylongtext)

see_outfile <- function(var_to_output) {
  # Creates a temporary file with the extension ".txt" and assigns its path to the variable `outfile`.
  outfile <- tempfile(fileext = ".txt")
  sink(file = outfile)
  # Writes the content of `text_to_output` to the temporary file specified by `outfile`.
  print(var_to_output, 
        na.print = "",
        n = nrow(var_to_output))
  # Opens the temporary file in the default text editor for viewing.
  file.show(outfile)
  sink()
}

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
  see_df_in_outfile()
  
  # filter(
  #     str_detect(letters, "a|f|o")
  # )

xml_ports <-
  vessels_from_pims_short_ok |>
  dplyr::filter(stringr::str_detect(hailingport, ".*xml.*")) |> 
  see_df_in_outfile()

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
  see_df_in_outfile()
  # View()


vessels_from_pims_split_addr__city_state__fix2_ok__good_ids_short |> 
  # filter(state_fixed == "AL, AL")
  filter(city_fixed == "APALIACHICOLA")

"C:\Users\anna.shipunova\Documents\R_code_github\home_ports\temp.xlsx"