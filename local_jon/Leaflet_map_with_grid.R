library(dplyr)
library(leaflet)

leaflet()%>%
  addTiles() %>%
  setView(lng = -112.0, lat = 37.5, zoom = 12) %>%
  addGraticule(interval = 0.04, sphere = FALSE) #%>%
  #addMarkers(101.6995, 3.1473)
