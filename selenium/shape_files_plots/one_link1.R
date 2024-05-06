library("tidyverse")
library("leaflet")
library(gtools)

clean_where_part <- function(where_part) {
  where_begin <- tolower("where")
  no_where <- !startsWith(tolower(trimws(where_part)), where_begin)
  if (where_part != "" && no_where) {
    where_part <- paste(where_begin, where_part, sep = " ")
  }
  return(where_part)
}

get_hdata_from_db <- function(link1) {
  q <- paste("SELECT DISTINCT
    TO_NUMBER(haulnum) || '_beg' as haulnum,
    gis_lathbeg,
    gis_lonhbeg
FROM
    obhau
WHERE
    link1 = '", link1, "'
union
SELECT DISTINCT
    TO_NUMBER(haulnum)  || '_end' as haulnum,
    gis_lathend,
    gis_lonhend
FROM
    obhau
WHERE
    link1 = '", link1, "'", sep = "")

  # q <- paste("select distinct
  # haulnum,
  # GIS_LATHBEG,
  #   GIS_LATHEND,
  #   GIS_LONHBEG,
  #   GIS_LONHEND
  #            FROM ", table_name, where_part, sep = " ")
  print(q)
  dbGetQuery(con_nova, q)
}

clean_data <- function(lat_lon_data_all) {
  cbind(stack(lat_lon_data_all[1:2]), stack(lat_lon_data_all[3:4])) -> res1

  colnames(res1) <- c("lat", "i1", "lon", "i2")

  res2 <- dplyr::select(res1, !starts_with("i"))

  # remove NAs
  res2[complete.cases(res2), ]
}

clean_dat_w_hnum <- function(lat_lon_data_w_hnum) {
  # c:\Users\anna.shipunova\work_dir\today\requests\temp_results\export_w_hnum.csv

  stacked_lat_lon <- cbind(
    stack(lat_lon_data_w_hnum[2:3]),
    stack(lat_lon_data_w_hnum[4:5])
  )
  rep_data <- as.data.frame(rep(lat_lon_data_w_hnum$HAULNUM, each = 2))
  row_odd <- seq_len(nrow(rep_data)) %% 2
  rep_data[row_odd == 1, 1] <- paste0(rep_data[row_odd == 1, 1], "_beg")
  rep_data[row_odd == 0, 1] <- paste0(rep_data[row_odd == 0, 1], "_end")

  stacked_lat_lon$haulnum <- rep_data[, 1]
  colnames(stacked_lat_lon) <- c("latitude", "i1", "longitude", "i2", "haulnum")

  res2 <- dplyr::select(stacked_lat_lon, !starts_with("i"))

  # remove NAs
  res2[complete.cases(res2), ]
}

show_dots <- function(lat_lon) {
  lat_lon %>%
    leaflet() %>%
    addTiles() %>%
    addPolylines(data = lat_lon, lng = ~longitude, lat = ~latitude, group = ~haulnum) %>%
    addMarkers(~longitude, ~latitude,
      label = paste(lat_lon$haulnum, " ", lat_lon$latitude, " ", lat_lon$longitude),
      labelOptions = labelOptions(noHide = T),
      clusterOptions = markerClusterOptions()
    )
}

show_dots_only <- function(lat_lon) {
  lat_lon %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(~longitude, ~latitude,
      label = lat_lon$haulnum,
      # paste(lat_lon$haulnum, " ", lat_lon$latitude, " ", lat_lon$longitude),
      labelOptions = labelOptions(noHide = T),
      clusterOptions = markerClusterOptions()
    )
}

new_coords_rows <- function(de1) {
  #  de1 <- list(c("set_123_beg", 40.71833, -73.95167),
  #              c("set_456_beg", 40.735, -73.90167))

  de2 <- do.call(rbind, de1)
  de3 <- as.data.frame(de2)

  names(de3) <- c("haulnum", "latitude", "longitude")
  num_columns <- c("latitude", "longitude")
  de3[, num_columns] <- lapply(num_columns, function(x) as.numeric(de3[[x]]))

  # newdf <- rbind(export1, de3)

  de3
}

add_point <- function(new_coords) {
  # new_coords <- c("port", 36.829797, -75.967353)
  qq <- rbind(export1, new_coords)
  num_columns <- c("latitude", "longitude")
  qq[, num_columns] <- lapply(num_columns, function(x) as.numeric(qq[[x]]))
  show_dots(qq)
}

from_paper <- function(list_coords) {
  #   list_coords <- list(c("1l_beg",41.02167, -72.55667),
  # c("1l_end",40.98667, -72.63333),
  # c("2l_beg",40.98833, -72.64167),
  # c("2l_end",41.02333, -72.55167))

  de2 <- do.call(rbind, list_coords)
  de3 <- as.data.frame(de2)

  names(de3) <- c("haulnum", "latitude", "longitude")
  num_columns <- c("latitude", "longitude")
  de3[, num_columns] <- lapply(num_columns, function(x) as.numeric(de3[[x]]))
  show_dots(de3)
  de3
}

run_all <- function(link1) {
  export1 <- get_hdata_from_db(link1)
  colnames(export1) <- c("haulnum", "latitude", "longitude")
  export1 <- export1[complete.cases(export1), ]
  rownames(export1) <- export1$haulnum
  # or
  # export1 <- read.csv("C:/Users/anna.shipunova/work_dir/today/temp/1.csv")
  # dots1 <- clean_dat_w_hnum(export1)
  export1 <- export1[mixedsort(as.character(export1$haulnum)),]
  
  show_dots(export1)
  export1
}

# __main__
link1 <- "010201711N54059"
export1 <- run_all(link1)
show_dots(export1)
