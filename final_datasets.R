#######################################
###### marburg 
###########################################

############## 예비분석용

library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)
library(tidyverse)
library(readr)

#### This is marburg base map
base_map <- readOGR(dsn = "D:\\Environmental data\\Base_map_numpoints_2000\\base_map.shp")

base_map <- as.data.frame(base_map@data)
base_map <- base_map %>% 
  arrange(id)
#filter the grids with area over 50%
base_map <- base_map %>% 
  filter(area > 10000000000 / 2)
base_map <- base_map %>% 
  dplyr::select(id, NUMPOINTS_, NUMPOINT_1)
colnames(base_map) <- c("ID", "marburg", "marburg")



## 예비분석의 설명변수는 각 변수의 전체 기간 평균.

#species richness
species_richness <- read_csv("D:\\Environmental data\\Species richness\\Intersection_area/Species_richness_int_final.csv")
species_richness <- species_richness %>% 
  dplyr::select(ID, 7, 8, 9, 10, 11)


# 1. Human footprint score  /  2000 ~ 2018 annually / mean 
hfp_total_mean <- read_csv("D:/Environmental data/Human_footprint_4326/hfp_total_mean.csv")
hfp_total_mean[is.na(hfp_total_mean)] <- mean(hfp_total_mean$hfp, na.rm = T)
hfp_total_mean$hfp <- round(as.numeric(hfp_total_mean$hfp), 2)
sum(is.na(hfp_total_mean$hfp))

# 2. Elevation  /  mean
elevation <- read_csv("D:\\Environmental data\\SRTM_elevation\\global_elevation.csv")
elevation[is.na(elevation)] <- mean(elevation$elevation, na.rm = T)
elevation$elevation <- round(as.numeric(elevation$elevation), 2)
sum(is.na(elevation$elevation))

# 3. Precipitation  /  monthly average  /  mean
precipitation <- read_csv("D:\\Environmental data\\Precipitation_1\\global_preci_annual.csv")
sum(is.na(precipitation$precipitation_annual))


# 4. Temperature  /  monthly average  /  mean
temperature <- read_csv("D:\\Environmental data\\Temperature_1\\global_temp_annual.csv")
sum(is.na(temperature$temp_annual))

# 5. Population count  /  2000 ~ 2020 by 5 years  /  sum
population <- read_csv("D:\\Environmental data\\Worldpop\\global_pop.csv")
sum(is.na(population$total_mean_pop))
population <- population %>% 
  dplyr::select(ID, total_mean_pop)

# 6. GDP  /  2000 ~ 2015 annually  / mean 
GDP <- read_csv("D:\\Environmental data\\GDP\\GDP_1\\global_GDP_2000.csv")
sum(is.na(GDP$total_mean_GDP))
GDP <- GDP %>% 
  dplyr::select(ID, total_mean_GDP)

# 7. HDI  /  1991 ~ 2015 annually  / mean
HDI <- read_csv("D:\\Environmental data\\GDP\\HDI_1\\global_HDI_2000.csv")
sum(is.na(HDI$total_mean_HDI))
HDI <- HDI %>% 
  dplyr::select(ID, total_mean_HDI)

# 8. Forest cover /  2000  /  count of 30m x 30m pixel 
#    Forest loss /  2001 ~ 2020  /  count of 30m x 30m pixel per loss year
#    Forest gain /  until 2012  /  count of 30m x 30m pixel
### make total forest cover.

treecover <- read_csv("D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\treecover.csv")
treeloss <- read_csv("D:\\Environmental data\\Forest_change\\Forest_loss\\treeloss.csv")
treegain <- read_csv("D:\\Environmental data\\Forest_change\\Forest_gain\\treegain.csv")

treecover_total <- cbind(treecover, treeloss, treegain)

treecover_total <- treecover_total %>% 
  dplyr::select(1, treecover, treeloss_total, treegain)

treecover_total <- treecover_total %>% 
  mutate(treecover_total = treecover +treegain - treeloss_total)

treecover_total <- treecover_total %>% 
  dplyr::select(ID, treecover_total)

# 9. Agricultural land use  /  2001 ~ 2020 annually
landcover_1 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_1_total.csv")
landcover_2 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_2_total.csv")
landcover_3 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_3_total.csv")
landcover_4 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_4_total.csv")
landcover_5 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_5_total.csv")
landcover_6 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_6_total.csv")
landcover_7 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_7_total.csv")
landcover_8 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_8_total.csv")
landcover_9 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_9_total.csv")
landcover_10 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_10_total.csv")
landcover_11 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_11_total.csv")
landcover_12 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_12_total.csv")
landcover_13 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_13_total.csv")
landcover_14 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_14_total.csv")
landcover_15 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_15_total.csv")
landcover_16 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_16_total.csv")
landcover_17 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_17_total.csv")

landcover_1 <- landcover_1 %>% 
  dplyr::select(ID, proportion_1)
landcover_2 <- landcover_2 %>% 
  dplyr::select(ID, proportion_2)
landcover_3 <- landcover_3 %>% 
  dplyr::select(ID, proportion_3)
landcover_4 <- landcover_4 %>% 
  dplyr::select(ID, proportion_4)
landcover_5 <- landcover_5 %>% 
  dplyr::select(ID, proportion_5)
landcover_6 <- landcover_6 %>% 
  dplyr::select(ID, proportion_6)
landcover_7 <- landcover_7 %>% 
  dplyr::select(ID, proportion_7)
landcover_8 <- landcover_8 %>% 
  dplyr::select(ID, proportion_8)
landcover_9 <- landcover_9 %>% 
  dplyr::select(ID, proportion_9)
landcover_10 <- landcover_10 %>% 
  dplyr::select(ID, proportion_10)
landcover_11 <- landcover_11 %>% 
  dplyr::select(ID, proportion_11)
landcover_12 <- landcover_12 %>% 
  dplyr::select(ID, proportion_12)
landcover_13 <- landcover_13 %>% 
  dplyr::select(ID, proportion_13)
landcover_14 <- landcover_14 %>% 
  dplyr::select(ID, proportion_14)
landcover_15 <- landcover_15 %>% 
  dplyr::select(ID, proportion_15)
landcover_16 <- landcover_16 %>% 
  dplyr::select(ID, proportion_16)
landcover_17 <- landcover_17 %>% 
  dplyr::select(ID, proportion_17)


landcover_1$proportion_1 <- as.numeric(landcover_1$proportion_1)
landcover_2$proportion_2 <- as.numeric(landcover_2$proportion_2)
landcover_3$proportion_3 <- as.numeric(landcover_3$proportion_3)
landcover_4$proportion_4 <- as.numeric(landcover_4$proportion_4)
landcover_5$proportion_5 <- as.numeric(landcover_5$proportion_5)
landcover_6$proportion_6 <- as.numeric(landcover_6$proportion_6)
landcover_7$proportion_7 <- as.numeric(landcover_7$proportion_7)
landcover_8$proportion_8 <- as.numeric(landcover_8$proportion_8)
landcover_9$proportion_9 <- as.numeric(landcover_9$proportion_9)
landcover_10$proportion_10 <- as.numeric(landcover_10$proportion_10)
landcover_11$proportion_11 <- as.numeric(landcover_11$proportion_11)
landcover_12$proportion_12 <- as.numeric(landcover_12$proportion_12)
landcover_13$proportion_13 <- as.numeric(landcover_13$proportion_13)
landcover_14$proportion_14 <- as.numeric(landcover_14$proportion_14)
landcover_15$proportion_15 <- as.numeric(landcover_15$proportion_15)
landcover_16$proportion_16 <- as.numeric(landcover_16$proportion_16)
landcover_17$proportion_17 <- as.numeric(landcover_17$proportion_17)


landcover_1[is.na(landcover_1)] <- mean(landcover_1$proportion_1, na.rm = T)
landcover_2[is.na(landcover_2)] <- mean(landcover_2$proportion_2, na.rm = T)
landcover_3[is.na(landcover_3)] <- mean(landcover_3$proportion_3, na.rm = T)
landcover_4[is.na(landcover_4)] <- mean(landcover_4$proportion_4, na.rm = T)
landcover_5[is.na(landcover_5)] <- mean(landcover_5$proportion_5, na.rm = T)
landcover_6[is.na(landcover_6)] <- mean(landcover_6$proportion_6, na.rm = T)
landcover_7[is.na(landcover_7)] <- mean(landcover_7$proportion_7, na.rm = T)
landcover_8[is.na(landcover_8)] <- mean(landcover_8$proportion_8, na.rm = T)
landcover_9[is.na(landcover_9)] <- mean(landcover_9$proportion_9, na.rm = T)
landcover_10[is.na(landcover_10)] <- mean(landcover_10$proportion_10, na.rm = T)
landcover_11[is.na(landcover_11)] <- mean(landcover_11$proportion_11, na.rm = T)
landcover_12[is.na(landcover_12)] <- mean(landcover_12$proportion_12, na.rm = T)
landcover_13[is.na(landcover_13)] <- mean(landcover_13$proportion_13, na.rm = T)
landcover_14[is.na(landcover_14)] <- mean(landcover_14$proportion_14, na.rm = T)
landcover_15[is.na(landcover_15)] <- mean(landcover_15$proportion_15, na.rm = T)
landcover_16[is.na(landcover_16)] <- mean(landcover_16$proportion_16, na.rm = T)
landcover_17[is.na(landcover_17)] <- mean(landcover_17$proportion_17, na.rm = T)

#전체 635개 
landcover_1 %>% 
  dplyr::filter(proportion_1 > 0) # 0개
landcover_2 %>% 
  dplyr::filter(proportion_2 > 0) #403개
landcover_3 %>% 
  dplyr::filter(proportion_3 > 0) # 0개
landcover_4 %>% 
  dplyr::filter(proportion_4 > 0) # 150개
landcover_5 %>% 
  dplyr::filter(proportion_5 > 0) # 133개
landcover_6 %>% 
  dplyr::filter(proportion_6 > 0) # 55개
landcover_7 %>% 
  dplyr::filter(proportion_7 > 0) # 67개
landcover_8 %>% 
  dplyr::filter(proportion_8 > 0) # 368개
landcover_9 %>% 
  dplyr::filter(proportion_9 > 0) # 487개
landcover_10 %>% 
  dplyr::filter(proportion_10 > 0) #413개
landcover_11 %>% 
  dplyr::filter(proportion_11 > 0) #110개
landcover_12 %>% 
  dplyr::filter(proportion_12 > 0) #167개
landcover_13 %>% 
  dplyr::filter(proportion_13 > 0) #65개
landcover_14 %>% 
  dplyr::filter(proportion_14 > 0) #98개
landcover_15 %>% 
  dplyr::filter(proportion_15 > 0) #0개
landcover_16 %>% 
  dplyr::filter (proportion_16 > 0) # 15개
landcover_17 %>% 
  dplyr::filter(proportion_17 > 0) # 0개

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14




### 이제 데이터 합치기.
marburg_total <- base_map %>% 
  left_join(hfp_total_mean, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(species_richness, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(elevation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(precipitation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(precipitation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(temperature, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(population, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(GDP, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(HDI, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(treecover_total, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(landcover_2, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_4, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_5, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_8, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_9, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_10, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_11, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_12, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_13, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_14, by = 'ID')

marburg_total <- marburg_total %>% 
  dplyr::select(-3)

write.csv(marburg_total, "D:\\Environmental data\\data_final\\marburg_total.csv")


### Marburg_total 
#### this is marburg base map
base_map <- readOGR(dsn = "D:\\Environmental data\\Base_map_numpoints_2000\\Marburg_map.shp")

base_map <- as.data.frame(base_map@data)
base_map <- base_map %>% 
  arrange(id)
#filter the grids with area over 50%
base_map <- base_map %>% 
  filter(area > 10000000000 / 2)
base_map <- base_map %>% 
  dplyr::select(id, NUMPOINTS_, NUMPOINT_1)
colnames(base_map) <- c("ID", "marburg", "marburg")


### 이제 데이터 합치기.
marburg_total <- base_map %>% 
  left_join(hfp_total_mean, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(species_richness, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(elevation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(precipitation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(precipitation, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(temperature, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(population, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(GDP, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(HDI, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(treecover_total, by = 'ID')

marburg_total <- marburg_total %>% 
  left_join(landcover_2, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_4, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_5, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_8, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_9, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_10, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_11, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_12, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_13, by = 'ID')
marburg_total <- marburg_total %>% 
  left_join(landcover_14, by = 'ID')

marburg_total <- marburg_total %>% 
  dplyr::select(-2)

write.csv(marburg_total, "D:\\Environmental data\\data_final\\marburg_total.csv")















######################################
########### 본분석
########################################



## 본분석의 설명변수는 2000, 2005, 2010, 2015, 2020으로 나누어서 각각 분석

# 1. Human footprint score  /  2000 ~ 2018 annually / mean 
hfp <- read_csv("D:/Environmental data/Human_footprint_4326/hfp.csv")
hfp$hfp_2000[is.na(hfp$hfp_2000)] <- mean(hfp$hfp_2000, na.rm = T)
hfp$hfp_2005[is.na(hfp$hfp_2005)] <- mean(hfp$hfp_2005, na.rm = T)
hfp$hfp_2010[is.na(hfp$hfp_2010)] <- mean(hfp$hfp_2010, na.rm = T)
hfp$hfp_2015[is.na(hfp$hfp_2015)] <- mean(hfp$hfp_2015, na.rm = T)
hfp$hfp_2018[is.na(hfp$hfp_2018)] <- mean(hfp$hfp_2018, na.rm = T) #hfp는 2018년도껄로.

hfp_total_mean$hfp <- round(as.numeric(hfp_total_mean$hfp), 2)
sum(is.na(hfp$hfp_2000))
sum(is.na(hfp$hfp_2005))
sum(is.na(hfp$hfp_2010))
sum(is.na(hfp$hfp_2015))
sum(is.na(hfp$hfp_2018))

hfp_2000 <- hfp %>% 
  dplyr::select(ID, hfp_2000)
hfp_2005 <- hfp %>% 
  dplyr::select(ID, hfp_2005)
hfp_2010 <- hfp %>% 
  dplyr::select(ID, hfp_2010)
hfp_2015 <- hfp %>% 
  dplyr::select(ID, hfp_2015)
hfp_2018 <- hfp %>% 
  dplyr::select(ID, hfp_2018)



# 2. Elevation  /  mean 그대로.
elevation <- read_csv("D:\\Environmental data\\SRTM_elevation\\global_elevation.csv")
elevation[is.na(elevation)] <- mean(elevation$elevation, na.rm = T)
elevation$elevation <- round(as.numeric(elevation$elevation), 2)
sum(is.na(elevation$elevation))

# 3. Precipitation  /  monthly average  /  mean 그대로.
precipitation <- read_csv("D:\\Environmental data\\Precipitation_1\\global_preci_annual.csv")
sum(is.na(precipitation$precipitation_annual))


# 4. Temperature  /  monthly average  /  mean 그대로.
temperature <- read_csv("D:\\Environmental data\\Temperature_1\\global_temp_annual.csv")
sum(is.na(temperature$temp_annual))

# 5. Population count  /  2000 ~ 2020 by 5 years  /  sum
population <- read_csv("D:\\Environmental data\\Worldpop\\global_pop.csv")
sum(is.na(population$total_mean_pop))
population_2000 <- population %>% 
  dplyr::select(ID, pop_2000)
population_2005 <- population %>% 
  dplyr::select(ID, pop_2005)
population_2010 <- population %>% 
  dplyr::select(ID, pop_2010)
population_2015 <- population %>% 
  dplyr::select(ID, pop_2015)
population_2020 <- population %>% 
  dplyr::select(ID, pop_2020)



# 6. GDP  /  2000 ~ 2015 annually  / mean 
GDP <- read_csv("D:\\Environmental data\\GDP\\GDP_1\\global_GDP_2000.csv")
sum(is.na(GDP$total_mean_GDP))

GDP_2000 <- GDP %>% 
  dplyr::select(ID, GDP_2000)
GDP_2005 <- GDP %>% 
  dplyr::select(ID, GDP_2005)
GDP_2010 <- GDP %>% 
  dplyr::select(ID, GDP_2010)
GDP_2015 <- GDP %>% 
  dplyr::select(ID, GDP_2015) # GDP는 2015까지만 존재.




# 7. HDI  /  1991 ~ 2015 annually  / mean
HDI <- read_csv("D:\\Environmental data\\GDP\\HDI_1\\global_HDI_2000.csv")
sum(is.na(HDI$total_mean_HDI))

HDI_2000 <- HDI %>% 
  dplyr::select(ID, HDI_2000)
HDI_2005 <- HDI %>% 
  dplyr::select(ID, HDI_2005)
HDI_2010 <- HDI %>% 
  dplyr::select(ID, HDI_2010)
HDI_2015 <- HDI %>% 
  dplyr::select(ID, HDI_2015) # HDI는 2015까지만 존재.


# 8. Forest cover /  2000  /  count of 30m x 30m pixel 
#    Forest loss /  2001 ~ 2020  /  count of 30m x 30m pixel per loss year
#    Forest gain /  until 2012  /  count of 30m x 30m pixel
### make total forest cover.

treecover <- read_csv("D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\treecover.csv")
treeloss <- read_csv("D:\\Environmental data\\Forest_change\\Forest_loss\\treeloss.csv")
treegain <- read_csv("D:\\Environmental data\\Forest_change\\Forest_gain\\treegain.csv")

treecover_total <- cbind(treecover, treeloss[,-1], treegain[,-1])

treecover_2000 <- treecover
colnames(treecover_2000) <- c("ID", "treecover_2000")
treecover_2005 <- treecover_total %>%  ## treecover 2005년까지 loss만 계산
  mutate(treecover_2005 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005) %>% 
  dplyr::select(1, treecover_2005)
treecover_2010 <- treecover_total %>%  ## treecover 2010년까지 loss만 계산
  mutate(treecover_2010 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010) %>% 
  dplyr::select(1, treecover_2010)
treecover_2015 <- treecover_total %>%  ## treecover 2015년까지 loss + gain 계산
  mutate(treecover_2015 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 + treegain) %>% 
  dplyr::select(1, treecover_2015)
treecover_2020 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2020 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 - Y2018 - Y2019 - Y2020 + treegain) %>% 
  dplyr::select(1, treecover_2020)



# 9. Agricultural land use  /  2001 ~ 2020 annually
landcover_1 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_1_total.csv")
landcover_2 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_2_total.csv")
landcover_3 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_3_total.csv")
landcover_4 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_4_total.csv")
landcover_5 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_5_total.csv")
landcover_6 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_6_total.csv")
landcover_7 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_7_total.csv")
landcover_8 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_8_total.csv")
landcover_9 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_9_total.csv")
landcover_10 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_10_total.csv")
landcover_11 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_11_total.csv")
landcover_12 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_12_total.csv")
landcover_13 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_13_total.csv")
landcover_14 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_14_total.csv")
landcover_15 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_15_total.csv")
landcover_16 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_16_total.csv")
landcover_17 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_17_total.csv")

landcover_1 <- landcover_1 %>% 
  dplyr::select(ID, proportion_1)
landcover_2 <- landcover_2 %>% 
  dplyr::select(ID, proportion_2)
landcover_3 <- landcover_3 %>% 
  dplyr::select(ID, proportion_3)
landcover_4 <- landcover_4 %>% 
  dplyr::select(ID, proportion_4)
landcover_5 <- landcover_5 %>% 
  dplyr::select(ID, proportion_5)
landcover_6 <- landcover_6 %>% 
  dplyr::select(ID, proportion_6)
landcover_7 <- landcover_7 %>% 
  dplyr::select(ID, proportion_7)
landcover_8 <- landcover_8 %>% 
  dplyr::select(ID, proportion_8)
landcover_9 <- landcover_9 %>% 
  dplyr::select(ID, proportion_9)
landcover_10 <- landcover_10 %>% 
  dplyr::select(ID, proportion_10)
landcover_11 <- landcover_11 %>% 
  dplyr::select(ID, proportion_11)
landcover_12 <- landcover_12 %>% 
  dplyr::select(ID, proportion_12)
landcover_13 <- landcover_13 %>% 
  dplyr::select(ID, proportion_13)
landcover_14 <- landcover_14 %>% 
  dplyr::select(ID, proportion_14)
landcover_15 <- landcover_15 %>% 
  dplyr::select(ID, proportion_15)
landcover_16 <- landcover_16 %>% 
  dplyr::select(ID, proportion_16)
landcover_17 <- landcover_17 %>% 
  dplyr::select(ID, proportion_17)


landcover_1$proportion_1 <- as.numeric(landcover_1$proportion_1)
landcover_2$proportion_2 <- as.numeric(landcover_2$proportion_2)
landcover_3$proportion_3 <- as.numeric(landcover_3$proportion_3)
landcover_4$proportion_4 <- as.numeric(landcover_4$proportion_4)
landcover_5$proportion_5 <- as.numeric(landcover_5$proportion_5)
landcover_6$proportion_6 <- as.numeric(landcover_6$proportion_6)
landcover_7$proportion_7 <- as.numeric(landcover_7$proportion_7)
landcover_8$proportion_8 <- as.numeric(landcover_8$proportion_8)
landcover_9$proportion_9 <- as.numeric(landcover_9$proportion_9)
landcover_10$proportion_10 <- as.numeric(landcover_10$proportion_10)
landcover_11$proportion_11 <- as.numeric(landcover_11$proportion_11)
landcover_12$proportion_12 <- as.numeric(landcover_12$proportion_12)
landcover_13$proportion_13 <- as.numeric(landcover_13$proportion_13)
landcover_14$proportion_14 <- as.numeric(landcover_14$proportion_14)
landcover_15$proportion_15 <- as.numeric(landcover_15$proportion_15)
landcover_16$proportion_16 <- as.numeric(landcover_16$proportion_16)
landcover_17$proportion_17 <- as.numeric(landcover_17$proportion_17)


landcover_1[is.na(landcover_1)] <- mean(landcover_1$proportion_1, na.rm = T)
landcover_2[is.na(landcover_2)] <- mean(landcover_2$proportion_2, na.rm = T)
landcover_3[is.na(landcover_3)] <- mean(landcover_3$proportion_3, na.rm = T)
landcover_4[is.na(landcover_4)] <- mean(landcover_4$proportion_4, na.rm = T)
landcover_5[is.na(landcover_5)] <- mean(landcover_5$proportion_5, na.rm = T)
landcover_6[is.na(landcover_6)] <- mean(landcover_6$proportion_6, na.rm = T)
landcover_7[is.na(landcover_7)] <- mean(landcover_7$proportion_7, na.rm = T)
landcover_8[is.na(landcover_8)] <- mean(landcover_8$proportion_8, na.rm = T)
landcover_9[is.na(landcover_9)] <- mean(landcover_9$proportion_9, na.rm = T)
landcover_10[is.na(landcover_10)] <- mean(landcover_10$proportion_10, na.rm = T)
landcover_11[is.na(landcover_11)] <- mean(landcover_11$proportion_11, na.rm = T)
landcover_12[is.na(landcover_12)] <- mean(landcover_12$proportion_12, na.rm = T)
landcover_13[is.na(landcover_13)] <- mean(landcover_13$proportion_13, na.rm = T)
landcover_14[is.na(landcover_14)] <- mean(landcover_14$proportion_14, na.rm = T)
landcover_15[is.na(landcover_15)] <- mean(landcover_15$proportion_15, na.rm = T)
landcover_16[is.na(landcover_16)] <- mean(landcover_16$proportion_16, na.rm = T)
landcover_17[is.na(landcover_17)] <- mean(landcover_17$proportion_17, na.rm = T)


### marburg_2000
marburg_2000 <- base_map %>% 
  left_join(hfp_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(elevation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(precipitation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(temperature, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(population_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(GDP_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(HDI_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(treecover_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  dplyr::select(-3)

write.csv(marburg_2000, "D:\\Environmental data\\data_final\\marburg_2000.csv")



### marburg_2000
marburg_2000 <- base_map %>% 
  left_join(hfp_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(elevation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(precipitation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(temperature, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(population_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(GDP_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(HDI_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(treecover_2000, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  dplyr::select(-2)

write.csv(marburg_2000, "D:\\Environmental data\\data_final\\marburg_2000.csv")



### marburg_2005
marburg_2005 <- base_map %>% 
  left_join(hfp_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(elevation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(precipitation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(temperature, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(population_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(GDP_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(HDI_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(treecover_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  dplyr::select(-3)

write.csv(marburg_2005, "D:\\Environmental data\\data_final\\marburg_2005.csv")



### marburg_2005
marburg_2005 <- base_map %>% 
  left_join(hfp_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(elevation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(precipitation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(temperature, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(population_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(GDP_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(HDI_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(treecover_2005, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  dplyr::select(-2)

write.csv(marburg_2005, "D:\\Environmental data\\data_final\\marburg_2005.csv")




### marburg_2010
marburg_2010 <- base_map %>% 
  left_join(hfp_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(elevation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(precipitation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(temperature, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(population_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(GDP_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(HDI_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(treecover_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  dplyr::select(-3)

write.csv(marburg_2010, "D:\\Environmental data\\data_final\\marburg_2010.csv")




### marburg_2010
marburg_2010 <- base_map %>% 
  left_join(hfp_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(elevation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(precipitation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(temperature, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(population_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(GDP_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(HDI_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(treecover_2010, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  dplyr::select(-2)

write.csv(marburg_2010, "D:\\Environmental data\\data_final\\marburg_2010.csv")




### marburg_2015
marburg_2015 <- base_map %>% 
  left_join(hfp_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(elevation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(precipitation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(temperature, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(population_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(GDP_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(HDI_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(treecover_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  dplyr::select(-3)

write.csv(marburg_2015, "D:\\Environmental data\\data_final\\marburg_2015.csv")




### marburg_2015
marburg_2015 <- base_map %>% 
  left_join(hfp_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(elevation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(precipitation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(temperature, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(population_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(GDP_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(HDI_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(treecover_2015, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  dplyr::select(-2)

write.csv(marburg_2015, "D:\\Environmental data\\data_final\\marburg_2015.csv")





### marburg_2020 여기서는 hfp는 2018, GDP, HDI는 2015사용
marburg_2020 <- base_map %>% 
  left_join(hfp_2018, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(elevation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(precipitation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(temperature, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(population_2020, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(GDP_2015, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(HDI_2015, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(treecover_2020, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  dplyr::select(-3)

write.csv(marburg_2020, "D:\\Environmental data\\data_final\\marburg_2020.csv")




### marburg_2020   hfp는 2018, GDP, HDI는 2015사용
marburg_2020 <- base_map %>% 
  left_join(hfp_2018, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(elevation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(precipitation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(temperature, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(population_2020, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(GDP_2015, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(HDI_2015, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(treecover_2020, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_2, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_4, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_5, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_8, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_9, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_10, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_11, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_12, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_13, by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_14, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  dplyr::select(-2)

write.csv(marburg_2020, "D:\\Environmental data\\data_final\\marburg_2020.csv")
























#######################################################
############# Timescale도 고려하는 본분석
#######################################################





#################################################
############### marburg 먼저 하면서 공통 데이터셋 정리해두기
################################################
#각 연도 별 결과변수와 설명변수를 행으로 쭉 쌓아서 만든다.
#일단은 각 연도 별로 결과변수와 설명변수를 할당해서 개별적인 데이터프레임을 만들고, 그걸 rbind로 합칠 생각.
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                        col_types = cols(...1 = col_skip()))
marburg_base <- marburg_total %>% 
  dplyr::select(ID, marburg)
marburg_base$marburg <- 0
marburg_base


### 본분석의 경우에는 timescale을 고려할 것.
#species richness
species_richness <- read_csv("D:\\Environmental data\\Species richness\\Intersection_area/Species_richness_int_final.csv")
species_richness <- species_richness %>% 
  dplyr::select(ID, 7, 8, 9, 10, 11)


# 1. Human footprint score  /  2000 ~ 2018 annually / mean 
hfp <- read_csv("D:/Environmental data/Human_footprint_4326/hfp.csv")
hfp$hfp_2000[is.na(hfp$hfp_2000)] <- mean(hfp$hfp_2000, na.rm = T)
hfp$hfp_2001[is.na(hfp$hfp_2001)] <- mean(hfp$hfp_2001, na.rm = T)
hfp$hfp_2002[is.na(hfp$hfp_2002)] <- mean(hfp$hfp_2002, na.rm = T)
hfp$hfp_2003[is.na(hfp$hfp_2003)] <- mean(hfp$hfp_2003, na.rm = T)
hfp$hfp_2004[is.na(hfp$hfp_2004)] <- mean(hfp$hfp_2004, na.rm = T)
hfp$hfp_2005[is.na(hfp$hfp_2005)] <- mean(hfp$hfp_2005, na.rm = T)
hfp$hfp_2006[is.na(hfp$hfp_2006)] <- mean(hfp$hfp_2006, na.rm = T)
hfp$hfp_2007[is.na(hfp$hfp_2007)] <- mean(hfp$hfp_2007, na.rm = T)
hfp$hfp_2008[is.na(hfp$hfp_2008)] <- mean(hfp$hfp_2008, na.rm = T)
hfp$hfp_2009[is.na(hfp$hfp_2009)] <- mean(hfp$hfp_2009, na.rm = T)
hfp$hfp_2010[is.na(hfp$hfp_2010)] <- mean(hfp$hfp_2010, na.rm = T)
hfp$hfp_2011[is.na(hfp$hfp_2011)] <- mean(hfp$hfp_2011, na.rm = T)
hfp$hfp_2012[is.na(hfp$hfp_2012)] <- mean(hfp$hfp_2012, na.rm = T)
hfp$hfp_2013[is.na(hfp$hfp_2013)] <- mean(hfp$hfp_2013, na.rm = T)
hfp$hfp_2014[is.na(hfp$hfp_2014)] <- mean(hfp$hfp_2014, na.rm = T)
hfp$hfp_2015[is.na(hfp$hfp_2015)] <- mean(hfp$hfp_2015, na.rm = T)
hfp$hfp_2016[is.na(hfp$hfp_2016)] <- mean(hfp$hfp_2016, na.rm = T)
hfp$hfp_2017[is.na(hfp$hfp_2017)] <- mean(hfp$hfp_2017, na.rm = T)
hfp$hfp_2018[is.na(hfp$hfp_2018)] <- mean(hfp$hfp_2018, na.rm = T)

sum(is.na(hfp))

# 2. Elevation  /  mean
elevation <- read_csv("D:\\Environmental data\\SRTM_elevation\\global_elevation.csv")
elevation[is.na(elevation)] <- mean(elevation$elevation, na.rm = T)
elevation$elevation <- round(as.numeric(elevation$elevation), 2)
sum(is.na(elevation$elevation))

# 3. Precipitation  /  monthly average  /  mean
precipitation <- read_csv("D:\\Environmental data\\Precipitation_1\\global_preci_annual.csv")
sum(is.na(precipitation$precipitation_annual))


# 4. Temperature  /  monthly average  /  mean
temperature <- read_csv("D:\\Environmental data\\Temperature_1\\global_temp_annual.csv")
sum(is.na(temperature$temp_annual))

# 5. Population count  /  2000 ~ 2020 annually  /  sum
population <- read_csv("D:\\Environmental data\\Worldpop\\global_pop_total.csv")
population$pop_2000[is.na(population$pop_2000)] <- mean(population$pop_2000, na.rm = T)
population$pop_2001[is.na(population$pop_2001)] <- mean(population$pop_2001, na.rm = T)
population$pop_2002[is.na(population$pop_2002)] <- mean(population$pop_2002, na.rm = T)
population$pop_2003[is.na(population$pop_2003)] <- mean(population$pop_2003, na.rm = T)
population$pop_2004[is.na(population$pop_2004)] <- mean(population$pop_2004, na.rm = T)
population$pop_2005[is.na(population$pop_2005)] <- mean(population$pop_2005, na.rm = T)
population$pop_2006[is.na(population$pop_2006)] <- mean(population$pop_2006, na.rm = T)
population$pop_2007[is.na(population$pop_2007)] <- mean(population$pop_2007, na.rm = T)
population$pop_2008[is.na(population$pop_2008)] <- mean(population$pop_2008, na.rm = T)
population$pop_2009[is.na(population$pop_2009)] <- mean(population$pop_2009, na.rm = T)
population$pop_2010[is.na(population$pop_2010)] <- mean(population$pop_2010, na.rm = T)
population$pop_2011[is.na(population$pop_2011)] <- mean(population$pop_2011, na.rm = T)
population$pop_2012[is.na(population$pop_2012)] <- mean(population$pop_2012, na.rm = T)
population$pop_2013[is.na(population$pop_2013)] <- mean(population$pop_2013, na.rm = T)
population$pop_2014[is.na(population$pop_2014)] <- mean(population$pop_2014, na.rm = T)
population$pop_2015[is.na(population$pop_2015)] <- mean(population$pop_2015, na.rm = T)
population$pop_2016[is.na(population$pop_2016)] <- mean(population$pop_2016, na.rm = T)
population$pop_2017[is.na(population$pop_2017)] <- mean(population$pop_2017, na.rm = T)
population$pop_2018[is.na(population$pop_2018)] <- mean(population$pop_2018, na.rm = T)
population$pop_2019[is.na(population$pop_2019)] <- mean(population$pop_2019, na.rm = T)
population$pop_2020[is.na(population$pop_2020)] <- mean(population$pop_2020, na.rm = T)

sum(is.na(population))

# 6. GDP  /  2000 ~ 2015 annually  / mean 
GDP <- read_csv("D:\\Environmental data\\GDP\\GDP_1\\global_GDP_2000.csv")
sum(is.na(GDP))


# 7. HDI  /  1991 ~ 2015 annually  / mean
HDI <- read_csv("D:\\Environmental data\\GDP\\HDI_1\\global_HDI_2000.csv")
sum(is.na(HDI$total_mean_HDI))


# 8. Forest cover /  2000  /  count of 30m x 30m pixel 
#    Forest loss /  2001 ~ 2020  /  count of 30m x 30m pixel per loss year
#    Forest gain /  until 2012  /  count of 30m x 30m pixel
### make total forest cover.

treecover <- read_csv("D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\treecover.csv")
treeloss <- read_csv("D:\\Environmental data\\Forest_change\\Forest_loss\\treeloss.csv")
treegain <- read_csv("D:\\Environmental data\\Forest_change\\Forest_gain\\treegain.csv")

treecover_total <- cbind(treecover, treeloss[,-1], treegain[,-1])

treecover_2000 <- treecover
colnames(treecover_2000) <- c("ID", "treecover_2000")
treecover <- read_csv("D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\treecover.csv")
treeloss <- read_csv("D:\\Environmental data\\Forest_change\\Forest_loss\\treeloss.csv")
treegain <- read_csv("D:\\Environmental data\\Forest_change\\Forest_gain\\treegain.csv")

treecover_total <- cbind(treecover, treeloss[,-1], treegain[,-1])

treecover_2000 <- treecover
colnames(treecover_2000) <- c("ID", "treecover_2000")

treecover_2001 <- treecover_total %>%  
  mutate(treecover_2001 = treecover - Y2001) %>% 
  dplyr::select(1, treecover_2001)
treecover_2002 <- treecover_total %>%  
  mutate(treecover_2002 = treecover - Y2001 - Y2002) %>% 
  dplyr::select(1, treecover_2002)
treecover_2003 <- treecover_total %>% 
  mutate(treecover_2003 = treecover - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2003)
treecover_2004 <- treecover_total %>% 
  mutate(treecover_2004 = treecover - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2004)
treecover_2005 <- treecover_total %>% 
  mutate(treecover_2005 = treecover - Y2005 - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2005)
treecover_2006 <- treecover_total %>%  
  mutate(treecover_2006 = treecover - Y2006 - Y2005 - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2006)
treecover_2007 <- treecover_total %>% 
  mutate(treecover_2007 = treecover - Y2007 - Y2006 - Y2005 - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2007)
treecover_2008 <- treecover_total %>% 
  mutate(treecover_2008 = treecover - Y2008- Y2007 - Y2006 - Y2005 - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2008)
treecover_2009 <- treecover_total %>% 
  mutate(treecover_2009 = treecover - Y2009 - Y2008- Y2007 - Y2006 - Y2005 - Y2004 - Y2001 - Y2002 - Y2003) %>% 
  dplyr::select(1, treecover_2009)
treecover_2010 <- treecover_total %>%  ## treecover 2010년까지 loss만 계산
  mutate(treecover_2010 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010) %>% 
  dplyr::select(1, treecover_2010)
treecover_2011 <- treecover_total %>%  ## treecover 2010년까지 loss만 계산
  mutate(treecover_2011 = treecover - Y2011 - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010) %>% 
  dplyr::select(1, treecover_2011)
treecover_2012 <- treecover_total %>%  ## treecover 2010년까지 loss만 계산
  mutate(treecover_2012 = treecover  - Y2011 - Y2012 - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010) %>% 
  dplyr::select(1, treecover_2012)

treecover_2013 <- treecover_total %>%  ## treecover 2015년까지 loss + gain 계산
  mutate(treecover_2013 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013 + treegain) %>% 
  dplyr::select(1, treecover_2013)
treecover_2014 <- treecover_total %>%  ## treecover 2015년까지 loss + gain 계산
  mutate(treecover_2014 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014  + treegain) %>% 
  dplyr::select(1, treecover_2014)
treecover_2015 <- treecover_total %>%  ## treecover 2015년까지 loss + gain 계산
  mutate(treecover_2015 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 + treegain) %>% 
  dplyr::select(1, treecover_2015)
treecover_2016 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2016 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 + treegain) %>% 
  dplyr::select(1, treecover_2016)
treecover_2017 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2017 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 + treegain) %>% 
  dplyr::select(1, treecover_2017)
treecover_2018 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2018 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 - Y2018 + treegain) %>% 
  dplyr::select(1, treecover_2018)
treecover_2019 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2019 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 - Y2018 - Y2019  + treegain) %>% 
  dplyr::select(1, treecover_2019)
treecover_2020 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2020 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 - Y2018 - Y2019 - Y2020 + treegain) %>% 
  dplyr::select(1, treecover_2020)
treecover_2021 <- treecover_total %>%  ## treecover 2005년까지 loss + gain 계산
  mutate(treecover_2021 = treecover - Y2001 - Y2002 - Y2003 - Y2004 - Y2005 - Y2006 - Y2007 - Y2008 - Y2009 - Y2010 -
           Y2011 - Y2012 - Y2013  - Y2014 - Y2015 - Y2016 - Y2017 - Y2018 - Y2019 - Y2020 - Y2021 + treegain) %>% 
  dplyr::select(1, treecover_2021)



### landcover
landcover_2001 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2001.csv")
landcover_2002 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2002.csv")
landcover_2003 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2003.csv")
landcover_2004 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2004.csv")
landcover_2005 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2005.csv")
landcover_2006 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2006.csv")
landcover_2007 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2007.csv")
landcover_2008 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2008.csv")
landcover_2009 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2009.csv")
landcover_2010 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2010.csv")
landcover_2011 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2011.csv")
landcover_2012 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2012.csv")
landcover_2013 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2013.csv")
landcover_2014 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2014.csv")
landcover_2015 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2015.csv")
landcover_2016 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2016.csv")
landcover_2017 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2017.csv")
landcover_2018 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2018.csv")
landcover_2019 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2019.csv")
landcover_2020 <- read_csv("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\landcover_mat_2020.csv")

landcover_2001 <- landcover_2001 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2002 <- landcover_2002 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2003 <- landcover_2003 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2004 <- landcover_2004 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2005 <- landcover_2005 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2006 <- landcover_2006 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2007 <- landcover_2007 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2008 <- landcover_2008 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2009 <- landcover_2009 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2010 <- landcover_2010 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2011 <- landcover_2011 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2012 <- landcover_2012 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2013 <- landcover_2013 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2014 <- landcover_2014 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2015 <- landcover_2015 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2016 <- landcover_2016 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2017 <- landcover_2017 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2018 <- landcover_2018 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2019 <- landcover_2019 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)

landcover_2020 <- landcover_2020 %>% 
  mutate(proportion_1 = landcover_1/(total + 0.01) * 100,
         proportion_2 = landcover_2/(total + 0.01) * 100,
         proportion_3 = landcover_3/(total + 0.01) * 100,
         proportion_4 = landcover_4/(total + 0.01) * 100,
         proportion_5 = landcover_5/(total + 0.01) * 100,
         proportion_6 = landcover_6/(total + 0.01) * 100,
         proportion_7 = landcover_7/(total + 0.01) * 100,
         proportion_8 = landcover_8/(total + 0.01) * 100,
         proportion_9 = landcover_9/(total + 0.01) * 100,
         proportion_10 = landcover_10/(total + 0.01) * 100,
         proportion_11 = landcover_11/(total + 0.01) * 100,
         proportion_12 = landcover_12/(total + 0.01) * 100,
         proportion_13 = landcover_13/(total + 0.01) * 100,
         proportion_14 = landcover_14/(total + 0.01) * 100,
         proportion_15 = landcover_15/(total + 0.01) * 100,
         proportion_16 = landcover_16/(total + 0.01) * 100,
         proportion_17 = landcover_17/(total + 0.01) * 100,)


### marburg_2000
marburg_2000 <- marburg_base %>% 
  left_join(hfp[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(elevation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(precipitation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(temperature, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(population[,c(2,3)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(GDP[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(HDI[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(treecover_2000, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_2001[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2000, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2000.csv")



### marburg_2001
marburg_2001 <- marburg_base %>% 
  left_join(hfp[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(elevation, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(precipitation, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(temperature, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(population[,c(2,4)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(GDP[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(HDI[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(treecover_2001, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2001 <- marburg_2001 %>% 
  left_join(landcover_2001[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                                  "proportion_9", "proportion_10", "proportion_11","proportion_12",
                                  "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2001, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2001.csv")


### marburg_2002
marburg_2002 <- marburg_base %>% 
  left_join(hfp[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(elevation, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(precipitation, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(temperature, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(population[,c(2,5)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(GDP[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(HDI[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(treecover_2002, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2002 <- marburg_2002 %>% 
  left_join(landcover_2002[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2002, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2002.csv")



### marburg_2003
marburg_2003 <- marburg_base %>% 
  left_join(hfp[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(elevation, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(precipitation, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(temperature, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(population[,c(2,6)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(GDP[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(HDI[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(treecover_2003, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2003 <- marburg_2003 %>% 
  left_join(landcover_2003[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2003, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2003.csv")



### marburg_2004
marburg_2004 <- marburg_base %>% 
  left_join(hfp[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(elevation, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(precipitation, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(temperature, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(population[,c(2,7)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(GDP[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(HDI[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(treecover_2004, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2004 <- marburg_2004 %>% 
  left_join(landcover_2004[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2004, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2004.csv")




### marburg_2005
marburg_2005 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(elevation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(precipitation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(temperature, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(population[,c("ID","pop_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(GDP[,c("ID","GDP_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(HDI[,c("ID","HDI_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(treecover_2005, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_2005[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2005, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2005.csv")


### marburg_2006
marburg_2006 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(elevation, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(precipitation, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(temperature, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(population[,c("ID","pop_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(GDP[,c("ID","GDP_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(HDI[,c("ID","HDI_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(treecover_2006, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2006 <- marburg_2006 %>% 
  left_join(landcover_2006[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2006, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2006.csv")



### marburg_2007
marburg_2007 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(elevation, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(precipitation, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(temperature, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(population[,c("ID","pop_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(GDP[,c("ID","GDP_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(HDI[,c("ID","HDI_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(treecover_2007, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2007 <- marburg_2007 %>% 
  left_join(landcover_2007[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2007, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2007.csv")


### marburg_2008
marburg_2008 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(elevation, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(precipitation, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(temperature, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(population[,c("ID","pop_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(GDP[,c("ID","GDP_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(HDI[,c("ID","HDI_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(treecover_2008, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2008 <- marburg_2008 %>% 
  left_join(landcover_2008[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2008, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2008.csv")


### marburg_2009
marburg_2009 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(elevation, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(precipitation, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(temperature, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(population[,c("ID","pop_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(GDP[,c("ID","GDP_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(HDI[,c("ID","HDI_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(treecover_2009, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2009 <- marburg_2009 %>% 
  left_join(landcover_2009[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2009, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2009.csv")


### marburg_2010
marburg_2010 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(elevation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(precipitation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(temperature, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(population[,c("ID","pop_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(GDP[,c("ID","GDP_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(HDI[,c("ID","HDI_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(treecover_2010, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_2010[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2010, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2010.csv")


### marburg_2011
marburg_2011 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(elevation, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(precipitation, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(temperature, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(population[,c("ID","pop_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(GDP[,c("ID","GDP_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(HDI[,c("ID","HDI_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(treecover_2011, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2011 <- marburg_2011 %>% 
  left_join(landcover_2011[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2011, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2011.csv")


### marburg_2012
marburg_2012 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(elevation, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(precipitation, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(temperature, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(population[,c("ID","pop_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(GDP[,c("ID","GDP_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(HDI[,c("ID","HDI_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(treecover_2012, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2012 <- marburg_2012 %>% 
  left_join(landcover_2012[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2012, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2012.csv")


### marburg_2013
marburg_2013 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(elevation, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(precipitation, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(temperature, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(population[,c("ID","pop_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(GDP[,c("ID","GDP_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(HDI[,c("ID","HDI_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(treecover_2013, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2013 <- marburg_2013 %>% 
  left_join(landcover_2013[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2013, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2013.csv")


### marburg_2014
marburg_2014 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(elevation, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(precipitation, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(temperature, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(population[,c("ID","pop_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(GDP[,c("ID","GDP_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(HDI[,c("ID","HDI_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(treecover_2014, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2014 <- marburg_2014 %>% 
  left_join(landcover_2014[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2014, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2014.csv")


### marburg_2015
marburg_2015 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(elevation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(precipitation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(temperature, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(population[,c("ID","pop_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(GDP[,c("ID","GDP_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(HDI[,c("ID","HDI_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(treecover_2015, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_2015[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2015, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2015.csv")


### 그리고 HDI, GDP는 사하라 이남 아프리카 평균 성장률 따라서 조정해주기. 
#거의 정체되어 있음 https://fred.stlouisfed.org/series/NYGDPPCAPKDSSF#
#그래서 오히려 전체적으로 0.5%씩 줄여주기.

GDP <- GDP %>% 
  mutate(GDP_2016 = GDP_2015 * 0.995,
         GDP_2017 = GDP_2016 * 0.995,
         GDP_2018 = GDP_2017 * 0.995,
         GDP_2019 = GDP_2018 * 0.995,
         GDP_2020 = GDP_2019 * 0.995,
         GDP_2021 = GDP_2020 * 1.005,)

HDI <- HDI %>% 
  mutate(HDI_2016 = HDI_2015 * 0.995,
         HDI_2017 = HDI_2016 * 0.995,
         HDI_2018 = HDI_2017 * 0.995,
         HDI_2019 = HDI_2018 * 0.995,
         HDI_2020 = HDI_2019 * 0.995,
         HDI_2021 = HDI_2020 * 1.005,)

### marburg_2016
marburg_2016 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(elevation, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(precipitation, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(temperature, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(population[,c("ID","pop_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(GDP[,c("ID","GDP_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(HDI[,c("ID","HDI_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(treecover_2016, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2016 <- marburg_2016 %>% 
  left_join(landcover_2016[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2016, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2016.csv")


### marburg_2017
marburg_2017 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(elevation, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(precipitation, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(temperature, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(population[,c("ID","pop_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(GDP[,c("ID","GDP_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(HDI[,c("ID","HDI_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(treecover_2017, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2017 <- marburg_2017 %>% 
  left_join(landcover_2017[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2017, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2017.csv")


### marburg_2018
marburg_2018 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(elevation, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(precipitation, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(temperature, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(population[,c("ID","pop_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(GDP[,c("ID","GDP_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(HDI[,c("ID","HDI_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(treecover_2018, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2018 <- marburg_2018 %>% 
  left_join(landcover_2018[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2018, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2018.csv")


### marburg_2019
marburg_2019 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(elevation, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(precipitation, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(temperature, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(population[,c("ID","pop_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(GDP[,c("ID","GDP_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(HDI[,c("ID","HDI_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(treecover_2019, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2019 <- marburg_2019 %>% 
  left_join(landcover_2019[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2019, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2019.csv")


### marburg_2020
marburg_2020 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(elevation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(precipitation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(temperature, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(population[,c("ID","pop_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(GDP[,c("ID","GDP_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(HDI[,c("ID","HDI_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(treecover_2020, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_2020[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2020, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2020.csv")


### marburg_2021
marburg_2021 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(elevation, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(precipitation, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(temperature, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(population[,c("ID","pop_2020")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(GDP[,c("ID","GDP_2021")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(HDI[,c("ID","HDI_2021")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(treecover_2020, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2021 <- marburg_2021 %>% 
  left_join(landcover_2020[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2021, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2021.csv")


### marburg_timescale_final
ebola_2000 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2000.csv")
ebola_2001 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2001.csv")
ebola_2002 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2002.csv")
ebola_2003 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2003.csv")
ebola_2004 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2004.csv")
ebola_2005 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2005.csv")
ebola_2006 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2006.csv")
ebola_2007 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2007.csv")
ebola_2008 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2008.csv")
ebola_2009 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2009.csv")
ebola_2010 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2010.csv")
ebola_2011 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2011.csv")
ebola_2012 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2012.csv")
ebola_2013 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2013.csv")
ebola_2014 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2014.csv")
ebola_2015 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2015.csv")
ebola_2016 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2016.csv")
ebola_2017 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2017.csv")
ebola_2018 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2018.csv")
ebola_2019 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2019.csv")
ebola_2020 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2020.csv")
ebola_2021 <- read.csv("D:\\Environmental data\\data_final\\ebola_timescale\\ebola_2021.csv")

species_richness <- read.csv("D:\\Environmental data\\Species richness\\Species_richness.csv")

ebola_2000 <- ebola_2000 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2001 <- ebola_2001 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2002 <- ebola_2002 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2003 <- ebola_2003 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2004 <- ebola_2004 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2005 <- ebola_2005 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2006 <- ebola_2006 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2007 <- ebola_2007 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2008 <- ebola_2008 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2009 <- ebola_2009 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2010 <- ebola_2010 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2011 <- ebola_2011 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2012 <- ebola_2012 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')

ebola_2013 <- ebola_2013 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2014 <- ebola_2014 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2015 <- ebola_2015 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2016 <- ebola_2016 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2017 <- ebola_2017 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2018 <- ebola_2018 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2019 <- ebola_2019 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2020 <- ebola_2020 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
ebola_2021 <- ebola_2021 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')



colnames(ebola_2000) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14", 
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2001) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2002) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2003) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2004) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2005) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2006) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2007) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2008) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2009) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2010) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2011) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2012) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2013) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2014) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2015) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2016) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2017) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2018) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2019) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2020) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(ebola_2021) <- c("ID", "ebola", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")



ebola_timescale_final <- rbind(ebola_2000, ebola_2001, ebola_2002,ebola_2003,ebola_2004,ebola_2005,ebola_2006,ebola_2007,
                               ebola_2008,ebola_2009,ebola_2010,ebola_2011,ebola_2012,ebola_2013,ebola_2014,ebola_2015,
                               ebola_2016, ebola_2017, ebola_2018, ebola_2019, ebola_2020, ebola_2021)

write.csv(ebola_timescale_final, "D:\\Environmental data\\data_final\\ebola_timescale\\ebola_timescale_final.csv")












######################################
######### Marburg 본분석 용. 이거는 에볼라 할 때 데이터 건드려놔서 합치기만
#######################################

#일단은 각 연도 별로 결과변수와 설명변수를 할당해서 개별적인 데이터프레임을 만들고, 그걸 rbind로 합칠 생각.
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                        col_types = cols(...1 = col_skip()))
marburg_base <- marburg_total %>% 
  dplyr::select(ID, marburg)
marburg_base$marburg <- 0
marburg_base


### marburg_2000
marburg_2000 <- marburg_base %>% 
  left_join(hfp[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(elevation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(precipitation, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(temperature, by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(population[,c(2,3)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(GDP[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(HDI[,c(1,2)], by = 'ID')

marburg_2000 <- marburg_2000 %>% 
  left_join(treecover_2000, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2000 <- marburg_2000 %>% 
  left_join(landcover_2001[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2000, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2000.csv")



### marburg_2001
marburg_2001 <- marburg_base %>% 
  left_join(hfp[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(elevation, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(precipitation, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(temperature, by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(population[,c(2,4)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(GDP[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(HDI[,c(1,3)], by = 'ID')

marburg_2001 <- marburg_2001 %>% 
  left_join(treecover_2001, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2001 <- marburg_2001 %>% 
  left_join(landcover_2001[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2001, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2001.csv")


### marburg_2002
marburg_2002 <- marburg_base %>% 
  left_join(hfp[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(elevation, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(precipitation, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(temperature, by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(population[,c(2,5)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(GDP[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(HDI[,c(1,4)], by = 'ID')

marburg_2002 <- marburg_2002 %>% 
  left_join(treecover_2002, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2002 <- marburg_2002 %>% 
  left_join(landcover_2002[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2002, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2002.csv")



### marburg_2003
marburg_2003 <- marburg_base %>% 
  left_join(hfp[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(elevation, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(precipitation, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(temperature, by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(population[,c(2,6)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(GDP[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(HDI[,c(1,5)], by = 'ID')

marburg_2003 <- marburg_2003 %>% 
  left_join(treecover_2003, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2003 <- marburg_2003 %>% 
  left_join(landcover_2003[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2003, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2003.csv")



### marburg_2004
marburg_2004 <- marburg_base %>% 
  left_join(hfp[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(elevation, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(precipitation, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(temperature, by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(population[,c(2,7)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(GDP[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(HDI[,c(1,6)], by = 'ID')

marburg_2004 <- marburg_2004 %>% 
  left_join(treecover_2004, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2004 <- marburg_2004 %>% 
  left_join(landcover_2004[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2004, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2004.csv")




### marburg_2005
marburg_2005 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(elevation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(precipitation, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(temperature, by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(population[,c("ID","pop_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(GDP[,c("ID","GDP_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(HDI[,c("ID","HDI_2005")], by = 'ID')

marburg_2005 <- marburg_2005 %>% 
  left_join(treecover_2005, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2005 <- marburg_2005 %>% 
  left_join(landcover_2005[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2005, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2005.csv")


### marburg_2006
marburg_2006 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(elevation, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(precipitation, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(temperature, by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(population[,c("ID","pop_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(GDP[,c("ID","GDP_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(HDI[,c("ID","HDI_2006")], by = 'ID')

marburg_2006 <- marburg_2006 %>% 
  left_join(treecover_2006, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2006 <- marburg_2006 %>% 
  left_join(landcover_2006[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2006, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2006.csv")



### marburg_2007
marburg_2007 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(elevation, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(precipitation, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(temperature, by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(population[,c("ID","pop_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(GDP[,c("ID","GDP_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(HDI[,c("ID","HDI_2007")], by = 'ID')

marburg_2007 <- marburg_2007 %>% 
  left_join(treecover_2007, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2007 <- marburg_2007 %>% 
  left_join(landcover_2007[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2007, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2007.csv")


### marburg_2008
marburg_2008 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(elevation, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(precipitation, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(temperature, by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(population[,c("ID","pop_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(GDP[,c("ID","GDP_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(HDI[,c("ID","HDI_2008")], by = 'ID')

marburg_2008 <- marburg_2008 %>% 
  left_join(treecover_2008, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2008 <- marburg_2008 %>% 
  left_join(landcover_2008[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2008, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2008.csv")


### marburg_2009
marburg_2009 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(elevation, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(precipitation, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(temperature, by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(population[,c("ID","pop_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(GDP[,c("ID","GDP_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(HDI[,c("ID","HDI_2009")], by = 'ID')

marburg_2009 <- marburg_2009 %>% 
  left_join(treecover_2009, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2009 <- marburg_2009 %>% 
  left_join(landcover_2009[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2009, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2009.csv")


### marburg_2010
marburg_2010 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(elevation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(precipitation, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(temperature, by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(population[,c("ID","pop_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(GDP[,c("ID","GDP_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(HDI[,c("ID","HDI_2010")], by = 'ID')

marburg_2010 <- marburg_2010 %>% 
  left_join(treecover_2010, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2010 <- marburg_2010 %>% 
  left_join(landcover_2010[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2010, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2010.csv")


### marburg_2011
marburg_2011 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(elevation, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(precipitation, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(temperature, by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(population[,c("ID","pop_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(GDP[,c("ID","GDP_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(HDI[,c("ID","HDI_2011")], by = 'ID')

marburg_2011 <- marburg_2011 %>% 
  left_join(treecover_2011, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2011 <- marburg_2011 %>% 
  left_join(landcover_2011[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2011, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2011.csv")


### marburg_2012
marburg_2012 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(elevation, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(precipitation, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(temperature, by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(population[,c("ID","pop_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(GDP[,c("ID","GDP_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(HDI[,c("ID","HDI_2012")], by = 'ID')

marburg_2012 <- marburg_2012 %>% 
  left_join(treecover_2012, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2012 <- marburg_2012 %>% 
  left_join(landcover_2012[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2012, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2012.csv")


### marburg_2013
marburg_2013 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(elevation, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(precipitation, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(temperature, by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(population[,c("ID","pop_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(GDP[,c("ID","GDP_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(HDI[,c("ID","HDI_2013")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(treecover_2013, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2013 <- marburg_2013 %>% 
  left_join(landcover_2013[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2013, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2013.csv")


### marburg_2014
marburg_2014 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(elevation, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(precipitation, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(temperature, by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(population[,c("ID","pop_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(GDP[,c("ID","GDP_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(HDI[,c("ID","HDI_2014")], by = 'ID')

marburg_2014 <- marburg_2014 %>% 
  left_join(treecover_2014, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2014 <- marburg_2014 %>% 
  left_join(landcover_2014[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2014, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2014.csv")


### marburg_2015
marburg_2015 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(elevation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(precipitation, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(temperature, by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(population[,c("ID","pop_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(GDP[,c("ID","GDP_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(HDI[,c("ID","HDI_2015")], by = 'ID')

marburg_2015 <- marburg_2015 %>% 
  left_join(treecover_2015, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2015 <- marburg_2015 %>% 
  left_join(landcover_2015[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2015, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2015.csv")


### 그리고 HDI, GDP는 사하라 이남 아프리카 평균 성장률 따라서 조정해주기. 
#거의 정체되어 있음 https://fred.stlouisfed.org/series/NYGDPPCAPKDSSF#
#그래서 오히려 전체적으로 0.5%씩 줄여주기.

GDP <- GDP %>% 
  mutate(GDP_2016 = GDP_2015 * 0.995,
         GDP_2017 = GDP_2016 * 0.995,
         GDP_2018 = GDP_2017 * 0.995,
         GDP_2019 = GDP_2018 * 0.995,
         GDP_2020 = GDP_2019 * 0.995,
         GDP_2021 = GDP_2020 * 1.005,)

HDI <- HDI %>% 
  mutate(HDI_2016 = HDI_2015 * 0.995,
         HDI_2017 = HDI_2016 * 0.995,
         HDI_2018 = HDI_2017 * 0.995,
         HDI_2019 = HDI_2018 * 0.995,
         HDI_2020 = HDI_2019 * 0.995,
         HDI_2021 = HDI_2020 * 1.005,)

### marburg_2016
marburg_2016 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(elevation, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(precipitation, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(temperature, by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(population[,c("ID","pop_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(GDP[,c("ID","GDP_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(HDI[,c("ID","HDI_2016")], by = 'ID')

marburg_2016 <- marburg_2016 %>% 
  left_join(treecover_2016, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2016 <- marburg_2016 %>% 
  left_join(landcover_2016[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2016, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2016.csv")


### marburg_2017
marburg_2017 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(elevation, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(precipitation, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(temperature, by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(population[,c("ID","pop_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(GDP[,c("ID","GDP_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(HDI[,c("ID","HDI_2017")], by = 'ID')

marburg_2017 <- marburg_2017 %>% 
  left_join(treecover_2017, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2017 <- marburg_2017 %>% 
  left_join(landcover_2017[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2017, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2017.csv")


### marburg_2018
marburg_2018 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(elevation, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(precipitation, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(temperature, by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(population[,c("ID","pop_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(GDP[,c("ID","GDP_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(HDI[,c("ID","HDI_2018")], by = 'ID')

marburg_2018 <- marburg_2018 %>% 
  left_join(treecover_2018, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2018 <- marburg_2018 %>% 
  left_join(landcover_2018[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2018, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2018.csv")


### marburg_2019
marburg_2019 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(elevation, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(precipitation, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(temperature, by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(population[,c("ID","pop_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(GDP[,c("ID","GDP_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(HDI[,c("ID","HDI_2019")], by = 'ID')

marburg_2019 <- marburg_2019 %>% 
  left_join(treecover_2019, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2019 <- marburg_2019 %>% 
  left_join(landcover_2019[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2019, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2019.csv")


### marburg_2020
marburg_2020 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(elevation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(precipitation, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(temperature, by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(population[,c("ID","pop_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(GDP[,c("ID","GDP_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(HDI[,c("ID","HDI_2020")], by = 'ID')

marburg_2020 <- marburg_2020 %>% 
  left_join(treecover_2020, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2020 <- marburg_2020 %>% 
  left_join(landcover_2020[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2020, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2020.csv")


### marburg_2021
marburg_2021 <- marburg_base %>% 
  left_join(hfp[,c("ID","hfp_2018")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(elevation, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(precipitation, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(temperature, by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(population[,c("ID","pop_2020")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(GDP[,c("ID","GDP_2021")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(HDI[,c("ID","HDI_2021")], by = 'ID')

marburg_2021 <- marburg_2021 %>% 
  left_join(treecover_2020, by = 'ID')

### 10%정도를 기준으로 자르면, 
#2, 4, 5, 8, 9, 10, 11, 12, 13, 14
marburg_2021 <- marburg_2021 %>% 
  left_join(landcover_2020[,c("ID", "proportion_2", "proportion_4","proportion_5","proportion_8",
                              "proportion_9", "proportion_10", "proportion_11","proportion_12",
                              "proportion_13","proportion_14")], by = 'ID')


write.csv(marburg_2021, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2021.csv")


### marburg_timescale_final
marburg_2000 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2000.csv")
marburg_2001 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2001.csv")
marburg_2002 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2002.csv")
marburg_2003 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2003.csv")
marburg_2004 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2004.csv")
marburg_2005 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2005.csv")
marburg_2006 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2006.csv")
marburg_2007 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2007.csv")
marburg_2008 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2008.csv")
marburg_2009 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2009.csv")
marburg_2010 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2010.csv")
marburg_2011 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2011.csv")
marburg_2012 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2012.csv")
marburg_2013 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2013.csv")
marburg_2014 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2014.csv")
marburg_2015 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2015.csv")
marburg_2016 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2016.csv")
marburg_2017 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2017.csv")
marburg_2018 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2018.csv")
marburg_2019 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2019.csv")
marburg_2020 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2020.csv")
marburg_2021 <- read.csv("D:\\Environmental data\\data_final\\marburg_timescale\\marburg_2021.csv")


marburg_2000 <- marburg_2000 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2001 <- marburg_2001 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2002 <- marburg_2002 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2003 <- marburg_2003 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2004 <- marburg_2004 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2005 <- marburg_2005 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2006 <- marburg_2006 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2007 <- marburg_2007 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2008 <- marburg_2008 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2009 <- marburg_2009 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2010 <- marburg_2010 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2011 <- marburg_2011 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2012 <- marburg_2012 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')

marburg_2013 <- marburg_2013 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2014 <- marburg_2014 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2015 <- marburg_2015 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2016 <- marburg_2016 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2017 <- marburg_2017 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2018 <- marburg_2018 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2019 <- marburg_2019 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2020 <- marburg_2020 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')
marburg_2021 <- marburg_2021 %>% 
  left_join(species_richness[,c("ID","accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                                "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")], by = 'ID')



colnames(marburg_2000) <- c("Number", "ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14", 
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2001) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2002) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2003) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2004) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2005) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2006) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2007) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2008) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2009) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2010) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2011) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2012) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2013) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2014) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2015) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2016) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2017) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2018) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2019) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2020) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")
colnames(marburg_2021) <- c("Number","ID", "marburg", "hfp", "elevation", "precipitation_annual", "temp_annual", "pop", "GDP",
                          "HDI", "treecover", "proportion_2","proportion_4","proportion_5","proportion_8",
                          "proportion_9","proportion_10","proportion_11","proportion_12","proportion_13","proportion_14",
                          "accipitriformes_Extant", "falconiformes_Extant", "strigiformes_Extant",
                          "carnivora_Extant", "colubridae_Extant", "chiroptera_Extant")


marburg_timescale_final <- rbind(marburg_2000, marburg_2001, marburg_2002,marburg_2003,marburg_2004,marburg_2005,marburg_2006,marburg_2007,
                               marburg_2008,marburg_2009,marburg_2010,marburg_2011,marburg_2012,marburg_2013,marburg_2014,marburg_2015,
                               marburg_2016, marburg_2017, marburg_2018, marburg_2019, marburg_2020, marburg_2021)

write.csv(marburg_timescale_final, "D:\\Environmental data\\data_final\\marburg_timescale\\marburg_timescale_final.csv")

