library("tidyverse")
library("leaflet")

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) - floor(abs(gis_coord))
  floor(dd * 60)
}

convert_to_ten_min <- function(num) {
  as.numeric(substr(as.character(num), 1, 1)) * 10
}

convert_to_decimal_degree <- function(dm_num) {
  degree <- as.numeric(substr(as.character(dm_num), 1, 2))
  dd <- as.numeric(substr(as.character(dm_num), 3, 4)) / 60
  degree + dd
}

get_gis_h_data_from_db <- function(all_link3) {
  q <- paste("SELECT DISTINCT
    TO_NUMBER(haulnum) || '_beg' as haulnum,
    gis_lathbeg,
    gis_lonhbeg
FROM
    obhau
WHERE
    link3 in (", all_link3, ")
union
SELECT DISTINCT
    TO_NUMBER(haulnum)  || '_end' as haulnum,
    gis_lathend,
    gis_lonhend
FROM
    obhau
WHERE
link3 in (", all_link3, ")
UNION
SELECT DISTINCT
    TO_NUMBER(haulnum) || '_beg' as haulnum,
    gis_lathbeg,
    gis_lonhbeg
FROM
    asmhau
WHERE
    link3 in (", all_link3, ")
union
SELECT DISTINCT
    TO_NUMBER(haulnum)  || '_end' as haulnum,
    gis_lathend,
    gis_lonhend
FROM
    asmhau
WHERE
    link3 in (", all_link3, ")", sep = "")

  print(q)
  dbGetQuery(con_nova, q)
}


get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  num <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(num)
  dm_num <- paste(deg, ten_min_num, sep = "")
  convert_to_decimal_degree(dm_num)
}

get_lon_ten_min <- function(gis_lon) {
  res <- get_lat_ten_min(abs(gis_lon))
  if (gis_lon < 0) {
    res * -1
  }
}

get_link3_from_db <- function(table_name) {
  q <- paste("SELECT DISTINCT
    link3,
    gis_lathbeg,
    gis_lonhbeg from ", table_name, sep = "")
  print(q)
  dbGetQuery(con_nova, q)
}

get_ten_min_coords <- function(db_data) {
  ten_min_coords <- data.frame(NA, NA, NA)
  names(ten_min_coords) <- c("ten_min", "lat_ten_min", "lon_ten_min")

  for (i in 1:nrow(db_data)) {
    l_row <- db_data[i, ]
    link3 <- l_row[1]
    gis_lat <- l_row[2]
    gis_lon <- l_row[3]

    ten_min_lat <- get_lat_ten_min(as.numeric(gis_lat))
    ten_min_lon <- get_lon_ten_min(as.numeric(gis_lon))
    temp_df <- data.frame("ten_min", ten_min_lat, ten_min_lon)

    ten_min_coords[nrow(ten_min_coords) + 1, ] <- temp_df
  }
  res <- ten_min_coords[complete.cases(ten_min_coords), ]
  distinct(res)
}

#clean_ten_min_res <- ten_min_res


# main
# gis_lat <- 41.790278
# gis_lon <- -69.844444
# link3 <- '000201001H620020003'
table_name <- "MA_STATE_STURGEON"
# all_link3 <- get_link3_from_db(table_name)

# get_ten_min_coords()