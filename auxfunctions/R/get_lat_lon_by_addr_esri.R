get_lat_lon_by_addr_esri <-
  function(input_df) {
    input_data_raw_esri <- input_df %>%
      dplyr::mutate(my_address = paste(STREET,
                                    CITY,
                                    STATE,
                                    ZIP,
                                    sep = ", ")) |>
      tidygeocoder::geocode(address = my_address,
                            return_addresses = TRUE,
                            method = 'arcgis',
                            full_results = TRUE)
    return(input_data_raw_esri)
  }
# Passing 558 addresses to the ArcGIS single address geocoder
# [===========================================] 558/558 (100%) Elapsed:  4m Remaining:  0s
# geocode_esri: 268.83 sec elapsed

# esri_rds_file_path <-
#   file.path(my_paths$inputs,
#             r"(ifq_landing_locations\input_data_raw_esri.rds)")
# 
# input_data_raw_esri <-
#   read_rds_or_run(esri_rds_file_path,
#                   my_data = input_data_raw,
#                   get_lat_lon_by_addr)
# # 2023-11-10 run for input_data_raw_esri.rds: 262.95 sec elapsed
