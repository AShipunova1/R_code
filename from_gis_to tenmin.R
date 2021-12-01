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
    link3 in (", all_link3, ")", sep = ""
)
  
  print(q)
  dbGetQuery(con_nova, q)
}


get_lat_ten_min <- function(gis_lat) {
  deg <- get_degree(gis_lat)
  num <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(num)
  dm_num <- paste(deg, ten_min_num, sep = '')
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
    link3 from ", table_name, sep = "")
  print(q)
  dbGetQuery(con_nova, q)    
}

# main
# gis_lat <- 41.790278
# gis_lon <- -69.844444
# link3 <- '000201001H620020003'
table_name = 'MA_STATE_STURGEON'
all_link3 <- get_link3_from_db(table_name)
# for (link3 in all_link3) {
  gis_h_db_data <- get_gis_h_data_from_db(all_link3)

# }