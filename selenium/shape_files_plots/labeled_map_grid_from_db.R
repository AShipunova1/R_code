source("~/R_code/from_gis_to tenmin.R")
prepare_data <- function(db_data) {
    names(db_data) <- c("coord_name", "lat", "lon")
    db_data <- db_data[complete.cases(db_data), ]
    num_columns <- c("lat", "lon")
    db_data[, num_columns] <- lapply(num_columns, function(x) as.numeric(db_data[[x]]))
    db_data
}

make_map1 <- function(db_data) {
        leaflet(db_data) %>% addTiles() %>%
        # setView(-70, 40, zoom = 1) %>%
        addCircleMarkers(~lon, ~lat,
            radius = 4,
            color = "red",
            stroke = FALSE, fillOpacity = 0.5,
            label = paste(db_data$coord_name, round(db_data$lat, 3), round(db_data$lon, 3), sep = "_")
        ) -> m
    m %>%
        addSimpleGraticule(interval = 1)
}

make_map2 <- function(db_data1, db_data2) {
    full_df <- rbind(db_data1, db_data2)
    leaflet(full_df) %>% addTiles() %>%
    addCircleMarkers(~lon, ~lat,
        # radius = ~ ifelse(grepl("\\w*c2", coord_name), 4, 4),
        radius = 4,
        color = ~ ifelse(grepl("\\w*c2", coord_name), "green", "red"),
        stroke = FALSE, fillOpacity = 0.5,
        label = paste(full_df$coord_name, round(full_df$lat, 3), round(full_df$lon, 3), sep = "_")
    ) -> m
    m %>%
        addSimpleGraticule(interval = 1)
    m
}

show_map2_w_rect -> function(ready_map){
  ready_map %>%
  addRectangles(
    lng1=-70, lat1=35,
    lng2=-78, lat2=42,
    fillColor = "transparent") %>%
    addRectangles(
      lng1=-66, lat1=40,
      lng2=-70, lat2=42.5,
      fillColor = "transparent",
      color = "yellow") %>% 
    addSimpleGraticule(interval = 1)
}

run_example <- function() {
    q1 <- paste("SELECT DISTINCT
    link3 || '_beg_c1',
    lat_beg,
    lon_beg
FROM
    sea_turtle_bycatch_geom
WHERE
    ( lat_beg BETWEEN 35 AND 42
      AND lon_beg BETWEEN -80 AND -70 )
    OR ( lat_end BETWEEN 35 AND 42
         AND lon_end BETWEEN -80 AND -70 )
UNION
SELECT DISTINCT
    link3 || '_end_c1',
    lat_end,
    lon_end
FROM
    sea_turtle_bycatch_geom
WHERE
    ( lat_beg BETWEEN 35 AND 42
      AND lon_beg BETWEEN -80 AND -70 )
    OR ( lat_end BETWEEN 35 AND 42
         AND lon_end BETWEEN -80 AND -70 )")

q2 <- paste("SELECT DISTINCT
    link3 || '_beg_c2',
    lat_beg,
    lon_beg
FROM
    sea_turtle_bycatch_geom
WHERE
    ( lat_beg BETWEEN 40 AND 42.5
      AND lon_beg BETWEEN -70 AND -66 )
    OR ( lat_end BETWEEN 40 AND 42.5
         AND lon_end BETWEEN -70 AND -66 )
UNION
SELECT DISTINCT
    link3 || '_end_c2',
    lat_end,
    lon_end
FROM
    sea_turtle_bycatch_geom
WHERE
    ( lat_beg BETWEEN 40 AND 42.5
      AND lon_beg BETWEEN -70 AND -66 )
    OR ( lat_end BETWEEN 40 AND 42.5
         AND lon_end BETWEEN -70 AND -66 )")

    db_data1 <- dbGetQuery(con_nova, q1)
    db_data2 <- dbGetQuery(con_nova, q2)
    dat1 <- prepare_data(db_data1)
    dat2 <- prepare_data(db_data2)
    m <- make_map2(dat1, dat2)
    m
}