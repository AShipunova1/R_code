# vessel_permit_port_info_perm_reg_short <-
#   vessel_permit_port_info_perm_reg |>
#   select(-c(TOP, all_permits)) |>
#   distinct()
#
# vessel_permit_port_info_perm_reg_short |>
#   data_overview()
# [1] 6763    8
# SERO_OFFICIAL_NUMBER  6762
# SERO_HOME_PORT_CITY    941
# permit_sa_gom            1 ?

# vessel_permit_port_info_perm_reg_short |>
#   filter(is.na(PORT_CODE) |
#            PORT_CODE == "00000") |>
#   dim()
# [1] 5957    8

# vessel_permit_port_info_perm_reg_short |>
#  filter(!PERMIT_VESSEL_ID == SERO_OFFICIAL_NUMBER) |>
#    glimpse()
# Rows: 1
# Columns: 8
# $ PERMIT_VESSEL_ID      <chr> "FL2310RW"
# $ VESSEL_VESSEL_ID      <dbl> 280699
# $ PORT_CODE             <chr> NA
# $ SERO_HOME_PORT_CITY   <chr> "PORT CANAVERAL"
# $ SERO_HOME_PORT_COUNTY <chr> "BREVARD"
# $ SERO_HOME_PORT_STATE  <chr> "FL"
# $ SERO_OFFICIAL_NUMBER  <chr> "1000164"
# $ permit_sa_gom         <chr> "dual"
