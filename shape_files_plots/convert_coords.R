#q <-"select * from port_coord"
#rr <- main(q)
#insert_all_into_db(rr)
library(stringr)
library(NISTunits)

main <- function(q) {
  db_data <- dbGetQuery(con_nova, q)
  all_coords <- get_all_clean_coords(db_data)
  all_coords <- all_coords[complete.cases(all_coords), ]
  res <- convert_all_to_decimal_degree(all_coords)
  res <- res[complete.cases(res), ]
}

get_all_clean_coords <- function(db_data) {
  all_coords <- data.frame(NA, NA, NA)
  names(all_coords) <- c("port_name", "lat", "lon")
  for (i in 1:nrow(db_data)) {
    l_row <- db_data[i, ]
    lat_1 <- clean_coord(l_row[3])
    lon_1 <- clean_coord(l_row[4])
    temp_df <- data.frame(l_row[2], lat_1, lon_1)
    all_coords[nrow(all_coords) + 1, ] <- temp_df
  }
  all_coords
}

clean_coord <- function(str_val) {
  pattern <- "[^0-9]+"
  replacement <- "."
  res1 <- str_replace_all(str_val, pattern, replacement)
  res <- str_replace_all(res1, "\\.$", "")
  res
}

convert_to_decimal_degree <- function(dm_num) {
  deg_min_sec <- str_split(dm_num, "\\.", simplify = TRUE)

  degree <- as.numeric(deg_min_sec[1, 1])
  min <- as.numeric(deg_min_sec[1, 2])
  tryCatch(
    {
      sec <- as.numeric(deg_min_sec[1, 3])
    },
    error = function(e) {
      sec <- 0
    }
  )
  gis_coord <- degree + (min / 60) + (sec / 3600)
  gis_coord
}

convert_all_to_decimal_degree <- function(all_coords) {
  gis_coords <- data.frame(NA, NA, NA, NA, NA)
  names(gis_coords) <- c("port_name", "ddmm_lat", "ddmm_lon", "gis_lat", "gis_lon")
  for (i in 1:nrow(all_coords)) {
    l_row <- all_coords[i, ]
    ddmm_lat <- convert_to_decimal_degree(l_row[2])
    ddmm_lon <- convert_to_decimal_degree(l_row[3])
    temp_df <- data.frame(l_row[1], l_row[2], l_row[3], round(ddmm_lat, digits = 6), round(ddmm_lon, digits = 6))
    gis_coords[nrow(gis_coords) + 1, ] <- temp_df
  }
  gis_coords
}

insert_all_into_db <- function(result) {
  #create or truncate the table first
  table_name <- "port_coord_gis"
  col_names <- paste("port_name", "ddmm_lat", "ddmm_lon", "gis_lat", "gis_lon", sep = ", ")
  my_q_str <- paste("INSERT INTO", table_name, "(", col_names, ") VALUES (%s)", sep = " ")
  sqls <- sprintf(my_q_str,
                apply(result, 1, function(i) paste("'", i, "'", sep = "", collapse=",")))
  lapply(sqls, function(s) dbExecute(con_nova, s))
}

get_distance <- function(lat1, lat2, lon1, lon2){
  rm <- 3963.0 # miles
  rk <- 6371 #metres
    # convert decimal degrees to radians
  lon1r <- NISTdegTOradian(lon1)
  lat1r <- NISTdegTOradian(lat1)
  lon2r <- NISTdegTOradian(lon2)
  lat2r <- NISTdegTOradian(lat2)

  # haversine formula
  dlon <- lon2r - lon1r
  dlat <- lat2r - lat1r

  a <- sin(dlat/2)**2 + cos(lat1r) * cos(lat2r) * sin(dlon/2)**2
  c <- 2 * asin(sqrt(a))
  d_mi <- rm * c
  d_me <- rk * c
}