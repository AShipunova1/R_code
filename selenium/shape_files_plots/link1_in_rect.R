source('~/R_code/one_link1.R')

#link1 = '000200002E72003'
map_w_rect <- function(link1) {

lat_lon <- run_all(link1)
#show_dots(export1)

lat_lon %>%
  leaflet() %>%
  addTiles() %>%
  addPolylines(data = lat_lon, lng = ~longitude, lat = ~latitude, group = ~haulnum) %>%
  addMarkers(~longitude, ~latitude,
             label = paste(lat_lon$haulnum, " ", lat_lon$latitude, " ", lat_lon$longitude),
             labelOptions = labelOptions(noHide = T),
             clusterOptions = markerClusterOptions()
  ) -> m1
m1 %>%   addRectangles(
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