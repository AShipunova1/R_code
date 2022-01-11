source('~/R_code/from_gis_to tenmin.R')
q <- paste("SELECT DISTINCT
    link3  || '_beg_1' as haul,
    LAT_BEG,
    LON_BEG    
FROM
    sea_turtle_bycatch_geom
    where (lat_beg BETWEEN 35 AND 42
AND lon_beg BETWEEN -80 AND -70)
or  (lat_end BETWEEN 35 AND 42
AND lon_end BETWEEN -80 AND -70)
union
SELECT DISTINCT
    link3 || '_end1' as haul,
    LAT_END,
    LON_END
FROM
    sea_turtle_bycatch_geom
    where (lat_beg BETWEEN 35 AND 42
AND lon_beg BETWEEN -80 AND -70)
or  (lat_end BETWEEN 35 AND 42
AND lon_end BETWEEN -80 AND -70)
union
SELECT DISTINCT
    link3  || '_beg2' as haul,
    LAT_BEG,
    LON_BEG    
FROM
    sea_turtle_bycatch_geom
    where (lat_beg BETWEEN 40 AND 42.5
AND lon_beg BETWEEN -70 AND -66)
or  (lat_end BETWEEN 40 AND 42.5
AND lon_end BETWEEN -70 AND -66)
union
SELECT DISTINCT
    link3 || '_end2' as haul,
    LAT_END,
    LON_END
FROM
    sea_turtle_bycatch_geom
    where 
(lat_beg BETWEEN 40 AND 42.5
AND lon_beg BETWEEN -70 AND -66)
or  (lat_end BETWEEN 40 AND 42.5
AND lon_end BETWEEN -70 AND -66)")
db_data <-   dbGetQuery(con_nova, q)

names(db_data) <- c("coord_name", "lat", "lon")
db_data <- db_data[complete.cases(db_data), ]
num_columns <- c("lat", "lon")
db_data[, num_columns] <- lapply(num_columns, function(x) as.numeric(db_data[[x]]))

leaflet(db_data) %>% addTiles() %>%
  #setView(-70, 40, zoom = 1) %>%
addCircleMarkers(~lon, ~lat,
                 radius = ~ifelse(coord_name == "ten_min", 7, 4),
                 color = ~ifelse(coord_name == "ten_min", "green", "red"),
                 stroke = FALSE, fillOpacity = 0.5,
                 label = paste(db_data$coord_name, round(db_data$lat, 3), round(db_data$lon, 3), sep = "_")
) -> m
m %>%
  addSimpleGraticule(interval = 1)    
