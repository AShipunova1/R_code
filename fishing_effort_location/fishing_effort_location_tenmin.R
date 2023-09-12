# library("tidyverse")
# library("leaflet")
library(stringi)

get_degree <- function(gis_coord) {
  floor(abs(gis_coord))
}

get_minute <- function(gis_coord) {
  dd <- abs(gis_coord) %% 1
  minute <- floor(dd * 60)
}

convert_to_ten_min <- function(minute) {
  floor(minute/10) * 10
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
  minute <- get_minute(gis_lat)
  ten_min_num <- convert_to_ten_min(minute)
  dm_num <- paste(deg, stri_pad_left(ten_min_num, 2, 0), sep = "")
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
  db_data <- db_data[complete.cases(db_data), ]

  ten_min_coords <- data.frame(NA, NA, NA)
  names(ten_min_coords) <- c("coord_name", "lat", "lon")

  for (i in 1:nrow(db_data)) {
    l_row <- db_data[i, ]
    # link3 <- l_row[1]
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

show_dots_only <- function(lat_lon) {
  lat_lon %>%
    leaflet() %>%
    addTiles() %>%
    addMarkers(
      label = lat_lon$haulnum,
      # paste(lat_lon$haulnum, " ", lat_lon$latitude, " ", lat_lon$longitude),
      labelOptions = labelOptions(noHide = T),
      clusterOptions = markerClusterOptions()
    )
}

getColor <- function(lat_lon_data) {
  sapply(lat_lon_data$coord_name, function(coord_name) {
    if (coord_name == "ten_min") {
      "green"
    } else {
      "red"
    }
  })
}

get_leaf_icons <- function(lat_lon_data) {
  leafIcons <- icons(
    iconUrl = ifelse(lat_lon_data$coord_name == "ten_min",
      "https://leafletjs.com/examples/custom-icons/leaf-green.png",
      "https://leafletjs.com/examples/custom-icons/leaf-red.png"
    ),
    iconWidth = 38, iconHeight = 95,
    iconAnchorX = 22, iconAnchorY = 94,
    shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
  )
  leafIcons
}

create_map_w_circles <- function(data_df) {
leaflet(data_df) %>% addTiles() %>%
  addCircleMarkers(~lon, ~lat,
                   clusterOptions = markerClusterOptions()
    # radius = ~ifelse(coord_name == "ten_min", 7, 4),
    # color = ~ifelse(coord_name == "ten_min", "green", "red"),
    # stroke = FALSE, fillOpacity = 0.5,
    # label = paste(data_df$coord_name, round(data_df$lat, 3), round(data_df$lon, 3), sep = "_")
  ) -> m
  m %>% addGraticule(interval = 1 / 60 * 10, style = list(color = "#FF0000", weight = 1))
}

create_map_w_circles_green_red <- function(data_df) {
  leaflet(data_df) %>% addTiles() %>%
    addCircleMarkers(
      ~ lon,
      ~ lat,
      clusterOptions = markerClusterOptions(),
      radius = ~ ifelse(coord_name == "ten_min", 7, 4),
      color = ~ ifelse(coord_name == "ten_min", "green", "red"),
      stroke = FALSE,
      fillOpacity = 0.5,
      label = paste(
        data_df$coord_name,
        round(data_df$lat, 3),
        round(data_df$lon, 3),
        sep = "_"
      )
    ) -> m
  m %>% addGraticule(interval = 1 / 60 * 10,
                     style = list(color = "grey", weight = 1))
}


create_map_w_drop_markers <- function(data_df) {
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(data_df)
  )

  leaflet(data_df) %>%
    addTiles() %>%
    addAwesomeMarkers(~lon, ~lat, icon = icons, label = ~as.character(coord_name))
}

create_map <- function(data_df) {
  leafIcons <- get_leaf_icons(data_df)
  leaflet(data = data_df) %>%
    addTiles() %>%
    addMarkers(~lon, ~lat,
      icon = leafIcons,
      label = paste(data_df$coord_name, round(data_df$lat, 3), round(data_df$lon, 3), sep = "_"),
      #labelOptions = labelOptions(noHide = T),
    ) -> m
  m %>% addGraticule(interval = 1 / 60 * 10, style = list(color = "#FF0000", weight = 1))
}

small_example_df <- function() {
  de1 <- list(
    c("230201202L170080004", 42.46667, -70.54000),
    c("230201205F850220002", 40.85000, -70.38667),
    c("230201205J240060008", 42.36333, -70.25167),
    c("230201205J400170007", 42.34000, -70.25167),
    c("230201205L430060004", 41.05500, -71.44000),
    c("230201407M650090002", 42.80500, -70.25000)
  )
  de2 <- do.call(rbind, de1)
  small_df <- as.data.frame(de2)
  names(small_df) <- c("coord_name", "lat", "lon")
  num_columns <- c("lat", "lon")
  small_df[, num_columns] <-
    lapply(num_columns, function(x)
      as.numeric(small_df[[x]]))
  return(small_df)
}

small_df <- small_example_df()

example_short <- function(small_df) {
  tm_c <- get_ten_min_coords(small_df)

  full_df <- rbind(small_df, tm_c)
  create_map_w_circles(full_df)
  #create_map(full_df)
}

example_db <- function() {
  table_name <- "MA_STATE_STURGEON"
  all_link3 <- get_link3_from_db(table_name)
  names(all_link3) <- c("coord_name", "lat", "lon")
  tm_c <- get_ten_min_coords(all_link3)

  full_df <- rbind(all_link3, tm_c)

  # create_map(full_df)
  create_map_w_circles(full_df)
}

# main ----
# gis_lat <- 41.0790278
# gis_lon <- -69.0844444
# link3 <- '000201001H620020003'

example_short()

create_map_w_drop_markers()

small_df
