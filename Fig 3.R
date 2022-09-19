
### Figure 3. 



library(dplyr)
library(tmap)
library(ggplot2)
library(sf)
Africa <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses-Africa\\African continent map\\afr_g2014_2013_0.shp")
Ebola_grid <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses\\Maps of African countries\\African countries clipped_grid\\Ebolagridmap_numpoints\\numpoints.shp")
Marburg_grid <- st_read("C:\\Users\\TaeheeCH\\Desktop\\Filoviruses-Africa\\Maps of African countries\\African countries clipped_grid\\Marburg_numpoints\\Marburg_numpoints_processed.shp")


map1 <- tm_shape(Africa) + tm_borders() + tm_fill("white")
map1

Ebola_grid_1 <- Ebola_grid %>% 
  mutate(pointGP = ifelse(NUMPOINTS < 1, 1, 2))
  
Marburg_grid_1 <- Marburg_grid %>% 
  mutate(pointGP = ifelse(NUMPOINTS < 1, 1, 2))

map2 <- map1 + tm_shape(Ebola_grid_1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "pointGP", alpha = 0.7) +
  tmap_options(check.and.fix = TRUE)
map2 

map3 <- map1 + tm_shape(Marburg_grid_1) + 
  tm_borders(lwd = 1.5, col = "black") + 
  tm_fill(col = "pointGP", alpha = 0.7) +
  tm_scale_bar(size = 1, position = "left") +
  tmap_options(check.and.fix = TRUE)
map3 


