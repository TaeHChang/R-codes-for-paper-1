library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)


#######################################
### 예비분석.
#######################################

############ Ebola (인구 밀도, forest cover (%) 계산 안 했던거 추가)
library(rgdal)
base_map <- readOGR(dsn = "D:\\Environmental data\\Base_map_numpoints_2000\\base_map.shp")

base_map <- as.data.frame(base_map@data)
base_map <- base_map %>% 
  arrange(id)
#filter the grids with area over 50%
base_map <- base_map %>% 
  filter(area > 10000000000 / 2)
base_map <- base_map %>% 
  dplyr::select(id, area)
colnames(base_map) <- c("ID", "area")

library(readr)
ebola_total <- read_csv("D:/Environmental data/data_final/ebola_total.csv", 
                        col_types = cols(...1 = col_skip()))


ebola_total <- ebola_total %>% 
  left_join(base_map, by = "ID")

ebola_total$total_mean_pop <- as.numeric(ebola_total$total_mean_pop)
ebola_total$total_mean_pop[is.na(ebola_total$total_mean_pop)] <- mean(ebola_total$total_mean_pop, na.rm = T)
ebola_total$total_mean_pop <- round(as.numeric(ebola_total$total_mean_pop), 2)

ebola_total$treecover_total <- as.numeric(ebola_total$treecover_total)
ebola_total$treecover_total[is.na(ebola_total$treecover_total)] <- mean(ebola_total$treecover_total, na.rm = T)
ebola_total$treecover_total <- round(as.numeric(ebola_total$treecover_total), 2)

ebola_total <- ebola_total %>% 
  mutate(pop_dense = total_mean_pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

ebola_total <- ebola_total %>% 
  mutate(forestcover_total = treecover_total / area * 90000) # Forest cover % 계산




write.csv(ebola_total, "D:\\Environmental data\\data_final\\ebola_total.csv")


############ Ebola 이제 본격적으로 시작.
library(readr)
ebola_total <- read_csv("D:/Environmental data/data_final/ebola_total.csv", 
                        col_types = cols(...1 = col_skip()))

ebola_total <- ebola_total %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total$ebola_gp <-factor(ebola_total$ebola_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


#### Histogram은 당연히 정규 아닐테니까 건너뛰고

#### Table 1.
table(ebola_total$ebola_gp)


#### Extant로 계산하였다. 
##  Chiroptera 
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(chiroptera_Extant),
            SD = sd(chiroptera_Extant))

ttest <- t.test(chiroptera_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(chiroptera_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(accipitriformes_Extant),
            SD = sd(accipitriformes_Extant))

ttest <- t.test(accipitriformes_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(accipitriformes_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(strigiformes_Extant),
            SD = sd(strigiformes_Extant))

ttest <- t.test(strigiformes_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(strigiformes_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(carnivora_Extant),
            SD = sd(carnivora_Extant))

ttest <- t.test(carnivora_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(carnivora_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(colubridae_Extant),
            SD = sd(colubridae_Extant))

ttest <- t.test(colubridae_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(colubridae_Extant ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  hfp
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(hfp),
            SD = sd(hfp))

ttest <- t.test(hfp ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(hfp ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  elevation
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(elevation),
            SD = sd(elevation))

ttest <- t.test(elevation ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(elevation ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Precipitaiton
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(precipitation_annual),
            SD = sd(precipitation_annual))

ttest <- t.test(precipitation_annual ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(precipitation_annual ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  Temperature
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(temp_annual),
            SD = sd(temp_annual))

ttest <- t.test(temp_annual ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(temp_annual ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Populatiuon density
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(pop_dense),
            SD = sd(pop_dense))

ttest <- t.test(pop_dense ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(pop_dense ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  GDP
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(total_mean_GDP),
            SD = sd(total_mean_GDP))

ttest <- t.test(total_mean_GDP ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(total_mean_GDP ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  HDI
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(total_mean_HDI),
            SD = sd(total_mean_HDI))

ttest <- t.test(total_mean_HDI ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(total_mean_HDI ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Forest cover
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(forestcover_total),
            SD = sd(forestcover_total))

ttest <- t.test(forestcover_total ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(forestcover_total ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_2, evergreen broadleaf forests
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_2),
            SD = sd(proportion_2))

ttest <- t.test(proportion_2 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_2 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_4, deciduous broadleaf forests
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_4),
            SD = sd(proportion_4))

ttest <- t.test(proportion_4 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_4 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_5, mixed forests
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_5),
            SD = sd(proportion_5))

ttest <- t.test(proportion_5 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_5 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_8, mixed forests
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_8),
            SD = sd(proportion_8))

ttest <- t.test(proportion_8 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_8 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_9, savannas
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_9),
            SD = sd(proportion_9))

ttest <- t.test(proportion_9 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_9 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_10, grassland
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_10),
            SD = sd(proportion_10))

ttest <- t.test(proportion_10 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_10 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_11, permanent wetlands
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_11),
            SD = sd(proportion_11))

ttest <- t.test(proportion_11 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_11 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_12, croplands
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_12),
            SD = sd(proportion_12))

ttest <- t.test(proportion_12 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_12 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_13, urban and built-up lands
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_13),
            SD = sd(proportion_13))

ttest <- t.test(proportion_13 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_13 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_14, cropland/natural vegetation mosaics
ebola_total %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(proportion_14),
            SD = sd(proportion_14))

ttest <- t.test(proportion_14 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_14 ~ ebola_gp, data = ebola_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)















############ Marburg (인구 밀도, forest cover (%) 계산 안 했던거 추가)
library(rgdal)
base_map <- readOGR(dsn = "D:\\Environmental data\\Base_map_numpoints_2000\\Marburg_map.shp")

base_map <- as.data.frame(base_map@data)
base_map <- base_map %>% 
  arrange(id)
#filter the grids with area over 50%
base_map <- base_map %>% 
  filter(area > 10000000000 / 2)
base_map <- base_map %>% 
  dplyr::select(id, area)
colnames(base_map) <- c("ID", "area")

library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                        col_types = cols(...1 = col_skip()))


marburg_total <- marburg_total %>% 
  left_join(base_map, by = "ID")

marburg_total$total_mean_pop <- as.numeric(marburg_total$total_mean_pop)
marburg_total$total_mean_pop[is.na(marburg_total$total_mean_pop)] <- mean(marburg_total$total_mean_pop, na.rm = T)
marburg_total$total_mean_pop <- round(as.numeric(marburg_total$total_mean_pop), 2)

marburg_total$treecover_total <- as.numeric(marburg_total$treecover_total)
marburg_total$treecover_total[is.na(marburg_total$treecover_total)] <- mean(marburg_total$treecover_total, na.rm = T)
marburg_total$treecover_total <- round(as.numeric(marburg_total$treecover_total), 2)

marburg_total <- marburg_total %>% 
  mutate(pop_dense = total_mean_pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

marburg_total <- marburg_total %>% 
  mutate(forestcover_total = treecover_total / area * 90000) # Forest cover % 계산




write.csv(marburg_total, "D:\\Environmental data\\data_final\\marburg_total.csv")


############ marburg 이제 본격적으로 시작.
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                        col_types = cols(...1 = col_skip()))

marburg_total <- marburg_total %>% 
  mutate(marburg_gp = ifelse(marburg >0, 1, 0))

marburg_total$marburg_gp <-factor(marburg_total$marburg_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


#### Histogram은 당연히 정규 아닐테니까 건너뛰고

#### Table 1.
table(marburg_total$marburg_gp)


#### Extant로 계산하였다. 
##  Chiroptera 
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(chiroptera_Extant),
            SD = sd(chiroptera_Extant))

ttest <- t.test(chiroptera_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(chiroptera_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(accipitriformes_Extant),
            SD = sd(accipitriformes_Extant))

ttest <- t.test(accipitriformes_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(accipitriformes_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(strigiformes_Extant),
            SD = sd(strigiformes_Extant))

ttest <- t.test(strigiformes_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(strigiformes_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(carnivora_Extant),
            SD = sd(carnivora_Extant))

ttest <- t.test(carnivora_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(carnivora_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(colubridae_Extant),
            SD = sd(colubridae_Extant))

ttest <- t.test(colubridae_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(colubridae_Extant ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  hfp
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(hfp, na.rm = T),
            SD = sd(hfp, na.rm = T))

ttest <- t.test(hfp ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(hfp ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  elevation
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(elevation),
            SD = sd(elevation))

ttest <- t.test(elevation ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(elevation ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Precipitaiton
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(precipitation_annual.x),
            SD = sd(precipitation_annual.x))

ttest <- t.test(precipitation_annual.x ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(precipitation_annual.x ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  Temperature
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(temp_annual),
            SD = sd(temp_annual))

ttest <- t.test(temp_annual ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(temp_annual ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Populatiuon density
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(pop_dense),
            SD = sd(pop_dense))

ttest <- t.test(pop_dense ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(pop_dense ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  GDP
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(total_mean_GDP),
            SD = sd(total_mean_GDP))

ttest <- t.test(total_mean_GDP ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(total_mean_GDP ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)



##  HDI
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(total_mean_HDI),
            SD = sd(total_mean_HDI))

ttest <- t.test(total_mean_HDI ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(total_mean_HDI ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Forest cover
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(forestcover_total),
            SD = sd(forestcover_total))

ttest <- t.test(forestcover_total ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(forestcover_total ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_2, evergreen broadleaf forests
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_2),
            SD = sd(proportion_2))

ttest <- t.test(proportion_2 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_2 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_4, deciduous broadleaf forests
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_4),
            SD = sd(proportion_4))

ttest <- t.test(proportion_4 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_4 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_5, mixed forests
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_5),
            SD = sd(proportion_5))

ttest <- t.test(proportion_5 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_5 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_8, woody savannas
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_8),
            SD = sd(proportion_8))

ttest <- t.test(proportion_8 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_8 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_9, savannas
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_9),
            SD = sd(proportion_9))

ttest <- t.test(proportion_9 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_9 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_10, grassland
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_10),
            SD = sd(proportion_10))

ttest <- t.test(proportion_10 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_10 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_11, permanent wetlands
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_11),
            SD = sd(proportion_11))

ttest <- t.test(proportion_11 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_11 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_12, croplands
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_12),
            SD = sd(proportion_12))

ttest <- t.test(proportion_12 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_12 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_13, urban and built-up lands
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_13),
            SD = sd(proportion_13))

ttest <- t.test(proportion_13 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_13 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Proportion_14, cropland/natural vegetation mosaics
marburg_total %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(proportion_14),
            SD = sd(proportion_14))

ttest <- t.test(proportion_14 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(proportion_14 ~ marburg_gp, data = marburg_total, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

