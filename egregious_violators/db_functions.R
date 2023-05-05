library(ROracle)

con = dbConnect(
  dbDriver("Oracle"),
  username = keyring::key_list("SECPR")[1, 2],
  password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1, 2]),
  dbname = "SECPR"
)

get_fields <- function() {
  con <- connect_to_secpr()
  
  permit_fields <- "select * from
             SAFIS.PERMITS@secapxdv_dblk
    FETCH NEXT 1 ROWS ONLY"
  
  permit_info <- dbGetQuery(con,
                            permit_fields)
  
  # transpose and write
  t_permit_info <- t(permit_info)
  str(t_permit_info)
  # write.csv(t_permit_info,
  # file.path(my_paths$inputs, "permit.csv"))
  
  my_out_xls <-
    file.path(my_paths, "../Documents/db", "fields.xlsx")
  
  # add to an existing excel
  write.xlsx(
    as.data.frame(t_permit_info),
    file = my_out_xls,
    sheetName = "permits",
    rowNames = TRUE,
    colNames = TRUE
    # ,
    # append = TRUE
  )
  
  write.xlsx(t_permit_info,
             my_out_xls,
             sheetName = "permits",
             append = TRUE)
  
  
  mv_safis_trip_download_fields <- dbGetQuery(con,
                                              "select * from
             srh.mv_safis_trip_download@secapxdv_dblk
               FETCH NEXT 1 ROWS ONLY")
  
  mv_sero_fh_permits_his_fields <- dbGetQuery(con,
                                              "select * from
             srh.mv_sero_fh_permits_his@Secapxdv_Dblk
               FETCH NEXT 1 ROWS ONLY")
  
  mv_sero_fh_permits_his_fields <- dbGetQuery(con,
                                              "select * from
             srh.mv_sero_fh_permits_his@Secapxdv_Dblk
               FETCH NEXT 1 ROWS ONLY")
  
  
  # write_csv(mv_sero_fh_permits_his_fields,
  # file.path(my_paths$inputs, "mv_sero_fh_permits_his_fields.csv"))
  
  
  dbDisconnect(con)
}

get_permit_expirations_by_vessel <- function() {
  con <- connect_to_secpr()
  
  vessel_ids_not_in_compl <- c(
    "1072853",
    "1206692",
    "1256792",
    "1298239",
    "620273",
    "960018",
    "FL0100RC",
    "FL1613RP",
    "FL1626MC",
    "FL2065SM",
    "FL9599SN",
    "NC-9578 WS",
    "NC0676EK"
  )
  
  vessel_ids_not_in_compl_str <- paste0("'",
                                        paste(vessel_ids_not_in_compl,
                                              collapse = "', '"),
                                        "'")
  
  permit_info_query <- paste(
    "select * from
             srh.mv_sero_fh_permits_his@Secapxdv_Dblk
             WHERE
    vessel_id in (",
    vessel_ids_not_in_compl_str,
    ")"
  )
  
  permit_info <- dbGetQuery(con,
                            permit_info_query)
  
  permit_info %>% select(VESSEL_ID) %>% unique() %>% str()
  # 11
  
  setdiff(vessel_ids_not_in_compl, permit_info$VESSEL_ID)
  # missing "NC-9578 WS" "NC0676EK"  
  
  dbDisconnect(con)
}

# TODO: use the query result instead of compliance for permit expiration info in email needed
