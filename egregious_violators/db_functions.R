get_fields <- function() {
  con <- connect_to_secpr()
  
  permit_fields <- paste("select * from
             SAFIS.PERMITS@secapxdv_dblk
    FETCH NEXT 1 ROWS ONLY")
  
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
             srh.mv_safis_trip_download@secapxdv_dblk
             WHERE
    VESSEL_OFFICIAL_NBR in (",
    vessel_ids_not_in_compl_str,
    ")"
  )
  
  
  
  permit_info <- dbGetQuery(con,
                            permit_info_query1)
  
  
  
  names(permit_info)
  
  dbDisconnect(con)
  
}
