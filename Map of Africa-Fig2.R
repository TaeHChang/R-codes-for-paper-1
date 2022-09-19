### Map of Africa, with Ebola and Marburg


library(sf)
Africa <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses\\African continent map\\afr_g2014_2013_0.shp")
Ebola <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses\\Ebola location\\Ebola_location.shp")
Marburg <- st_read('C:\\Users\\TaeheeCH\\Desktop\\Filoviruses\\Marburg location\\Marburg location.shp')
Countries <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses\\Maps of African countries\\African countries\\United map of countries.shp")
library(sf)
library(raster)
library(dplyr)
library(tmap)
library(ggplot2)

map1 <- tm_shape(Africa) + tm_borders() + tm_fill("white")
map1


map2 <- map1 + tm_shape(Countries) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "blue", alpha = 0.05)
map2 

map3 <- map2 + # Ebola map
  tm_shape(Ebola) + tm_dots(size = 0.3, col = "red", alpha = 0.5, shape = "Species") +
  tm_scale_bar(size = 1, position = "right") + #스케일 바 집어넣기
  tm_compass(type = "8star", position = c("RIGHT", "top")) 
map3 

map3 <- map2 + # Ebola map + Marburg map
  tm_shape(Ebola) + tm_dots(size = 0.3, col = "red", alpha = 0.6, shape = "Species") 
map3 

map4 <- map3 + # Ebola map + Marburg map
  tm_shape(Marburg) + tm_dots(size = 0.3, alpha = 0.7, col = "cyan") +
   tm_scale_bar(size = 1, position = "right") + #스케일 바 집어넣기 
  tm_compass(type = "8star", position = c("RIGHT", "top")) 
map4 






