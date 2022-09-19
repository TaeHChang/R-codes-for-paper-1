library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)

######################################
#데이터 다시 정리
######################################

library(readr)
species_count <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN\\Species_richness_Maxent.csv", 
                          col_types = cols(...1 = col_skip()))

ebola_total <- read_csv("D:/Environmental data/data_final/ebola_total.csv", 
                        col_types = cols(...1 = col_skip()))


ebola_total_maxent_IUCN <- ebola_total %>% 
  left_join(species_count, by = 'ID')


write.csv(ebola_total_maxent_IUCN, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\ebola_total_maxent_IUCN.csv")

ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total_maxent_IUCN$ebola_gp <-factor(ebola_total_maxent_IUCN$ebola_gp, levels = c(0, 1),
                                          labels = c("Not_occured", "Occured"))


ebola_timescale_final <- read_csv("D:/Environmental data/data_final/ebola_timescale/ebola_timescale_final.csv")

ebola_total_maxent_IUCN_timescale <- ebola_timescale_final %>% 
  left_join(species_count, by = 'ID')

write.csv(ebola_total_maxent_IUCN_timescale, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\ebola_total_maxent_IUCN_timescale.csv")



library(corrplot)

whole <- ebola_total_maxent_IUCN %>% 
  dplyr::select(hfp, elevation, precipitation_annual, temp_annual, total_mean_GDP, forestcover_total,
                proportion_2, proportion_4,proportion_5,proportion_8,proportion_9,proportion_10,
                proportion_11,proportion_12,proportion_13,proportion_14, pop_dense, chiroptera_Extant_Maxent,
                accipitriformes_Extant_Maxent, strigiformes_Extant_Maxent, carnivora_Extant_Maxent, 
                colubridae_Extant_Maxent) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


whole <- whole %>% 
  dplyr::select(-elevation, -proportion_2, -proportion_13, -forestcover_total,
                -accipitriformes_Extant_Maxent)

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})





### 1. 다중공선성만 제외한 포화모형. 
ebola_rev_1 <- glm(ebola_gp ~ chiroptera_Extant_Maxent + strigiformes_Extant_Maxent + carnivora_Extant_Maxent +
                   colubridae_Extant_Maxent + hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14 
                   , 
                   data = ebola_total_maxent_IUCN, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)




library(ROCR)
ld_result <- predict(ebola_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, ebola_total_maxent_IUCN$ebola_gp)
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]] 
AUC



durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)





########## 일단 여기서 공간적 상관성 고려한 모델로 한 번 더 해보기.
ebola_total_shp <- sf::st_read("D:\\Environmental data\\data_final\\final_data_shp\\ebola_total.shp")

ebola_total_shp <- ebola_total_shp %>% 
  filter(ebola_tota != is.na(ebola_tota))


library(INLA)
library(spdep)
ebola_total_shp <- ebola_total_shp %>% 
  filter(ebola_tota != is.na(ebola_tota))

ebola_total_shp$id <- as.character(ebola_total_shp$id)

sf::sf_use_s2(FALSE)
nb <- spdep::poly2nb(as(ebola_total_shp, "Spatial"), row.names = ebola_total_shp$id)
head(nb)
nb2INLA("map.adj",nb)
g <- inla.read.graph(filename ="map.adj")
is.na(g)
sum(is.na(ebola_total))
W <- nb2mat(nb, style = "B") 


ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  rename(id = ID)
ebola_total_maxent_IUCN$X <- rep(1:524)
ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(X2 = X)


# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~  chiroptera_Extant_Maxent + strigiformes_Extant_Maxent + 
  colubridae_Extant_Maxent + hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 + f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total_maxent_IUCN, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.042  ,  0.125  ,    0.303))



ebola_total_maxent_IUCN_timescale$X <- rep(1:524, 22)
ebola_total_maxent_IUCN_timescale$X2 <- rep(1:524, 22)
ebola_total_maxent_IUCN_timescale$year <- rep(1:22, each = 524)
ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  rename(ind2 = ...1)

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

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  left_join(base_map, by = "ID")

ebola_total_maxent_IUCN_timescale$pop <- as.numeric(ebola_total_maxent_IUCN_timescale$pop)
ebola_total_maxent_IUCN_timescale$pop[is.na(ebola_total_maxent_IUCN_timescale$pop)] <- mean(ebola_total_maxent_IUCN_timescale$pop, na.rm = T)
ebola_total_maxent_IUCN_timescale$pop <- round(as.numeric(ebola_total_maxent_IUCN_timescale$pop), 2)

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ chiroptera_Extant_Maxent + strigiformes_Extant_Maxent + carnivora_Extant_Maxent + 
  colubridae_Extant_Maxent + hfp + precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") +
  f(ind2,model="iid")
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_total_maxent_IUCN_timescale, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-0.021  ,  0.118   ,   0.264  ))








############ Marburg
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                          col_types = cols(...1 = col_skip()))


marburg_total_maxent_IUCN <- marburg_total %>% 
  left_join(species_count, by = 'ID')


write.csv(marburg_total_maxent_IUCN, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\marburg_total_maxent_IUCN.csv")


marburg_total_maxent_IUCN <- marburg_total_maxent_IUCN %>% 
  mutate(marburg_gp = ifelse(marburg >0, 1, 0))

marburg_total_maxent_IUCN$marburg_gp <-factor(marburg_total_maxent_IUCN$marburg_gp, levels = c(0, 1),
                                  labels = c("Not_occured", "Occured"))









### 1. 다중공선성만 제외한 포화모형. 
marburg_rev_1 <- glm(marburg_gp ~ chiroptera_Extant_Maxent + strigiformes_Extant_Maxent + carnivora_Extant_Maxent +
                       colubridae_Extant_Maxent + hfp + precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                       proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                       proportion_14 
                     , 
                     data = marburg_total_maxent_IUCN, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)



durbinWatsonTest(marburg_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(marburg_rev_1)








############### Categorical

ebola_total_maxent_IUCN %>% summary()


#strigiformes - 6.00/8.00/9.00
#carnivora - 13.00/17.00/19.00
#colubridae - 9.00/16.00/21.00



ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant_Maxent < 6, 1, 
                                 ifelse(strigiformes_Extant_Maxent < 8, 2,
                                        ifelse(strigiformes_Extant_Maxent < 9, 3,4))))
ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant_Maxent < 13, 1, 
                              ifelse(carnivora_Extant_Maxent < 17, 2,
                                     ifelse(carnivora_Extant_Maxent < 19, 3,4))))
ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant_Maxent < 20, 1, 
                               ifelse(colubridae_Extant_Maxent < 23, 2, 3)))

ebola_total_maxent_IUCN$strigiformesGP <-factor(ebola_total_maxent_IUCN$strigiformesGP, 
                                    levels = c(1, 2, 3, 4))
ebola_total_maxent_IUCN$carnivoraGP <-factor(ebola_total_maxent_IUCN$carnivoraGP, 
                                 levels = c(1, 2, 3, 4))
ebola_total_maxent_IUCN$colubridaeGP <-factor(ebola_total_maxent_IUCN$colubridaeGP, 
                                  levels = c(1, 2, 3))

summary(ebola_total_maxent_IUCN$colubridaeGP)

ebola_total_maxent_IUCN %>% 
  group_by(colubridaeGP) %>% 
  summarize(n = sum(ebola))

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant_Maxent < 6, 1, 
                                 ifelse(strigiformes_Extant_Maxent < 8, 2,
                                        ifelse(strigiformes_Extant_Maxent < 9, 3,4))))

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant_Maxent < 13, 1, 
                              ifelse(carnivora_Extant_Maxent < 17, 2,
                                     ifelse(carnivora_Extant_Maxent < 19, 3,4))))

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant_Maxent < 20, 1, 
                               ifelse(colubridae_Extant_Maxent < 23, 2, 3)))


ebola_total_maxent_IUCN_timescale$strigiformesGP <-factor(ebola_total_maxent_IUCN_timescale$strigiformesGP, 
                                              levels = c(1, 2, 3, 4))
ebola_total_maxent_IUCN_timescale$carnivoraGP <-factor(ebola_total_maxent_IUCN_timescale$carnivoraGP, 
                                           levels = c(1, 2, 3, 4))
ebola_total_maxent_IUCN_timescale$colubridaeGP <-factor(ebola_total_maxent_IUCN_timescale$colubridaeGP, 
                                            levels = c(1, 2, 3))


strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP

### 1. 다중공선성만 제외한 포화모형. 
ebola_rev_1 <- glm(ebola_gp ~ strigiformesGP + chiroptera_Extant_Maxent + colubridaeGP + carnivoraGP + hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14 
                   , 
                   data = ebola_total_maxent_IUCN, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)




library(ROCR)
ld_result <- predict(ebola_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, ebola_total_maxent_IUCN$ebola_gp)
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]] 
AUC



durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)





########## 일단 여기서 공간적 상관성 고려한 모델로 한 번 더 해보기.
ebola_total_shp <- sf::st_read("D:\\Environmental data\\data_final\\final_data_shp\\ebola_total.shp")

ebola_total_shp <- ebola_total_shp %>% 
  filter(ebola_tota != is.na(ebola_tota))


library(INLA)
library(spdep)
ebola_total_shp <- ebola_total_shp %>% 
  filter(ebola_tota != is.na(ebola_tota))

ebola_total_shp$id <- as.character(ebola_total_shp$id)

sf::sf_use_s2(FALSE)
nb <- spdep::poly2nb(as(ebola_total_shp, "Spatial"), row.names = ebola_total_shp$id)
head(nb)
nb2INLA("map.adj",nb)
g <- inla.read.graph(filename ="map.adj")
is.na(g)
sum(is.na(ebola_total))
W <- nb2mat(nb, style = "B") 


ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  rename(id = ID)
ebola_total_maxent_IUCN$X <- rep(1:524)
ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(X2 = X)


# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~  strigiformesGP + chiroptera_Extant_Maxent + colubridaeGP + carnivoraGP + hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 + f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total_maxent_IUCN, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.099,  -0.14    ,  0.709))



ebola_total_maxent_IUCN_timescale$X <- rep(1:524, 22)
ebola_total_maxent_IUCN_timescale$X2 <- rep(1:524, 22)
ebola_total_maxent_IUCN_timescale$year <- rep(1:22, each = 524)
ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  rename(ind2 = ...1)

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

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  left_join(base_map, by = "ID")

ebola_total_maxent_IUCN_timescale$pop <- as.numeric(ebola_total_maxent_IUCN_timescale$pop)
ebola_total_maxent_IUCN_timescale$pop[is.na(ebola_total_maxent_IUCN_timescale$pop)] <- mean(ebola_total_maxent_IUCN_timescale$pop, na.rm = T)
ebola_total_maxent_IUCN_timescale$pop <- round(as.numeric(ebola_total_maxent_IUCN_timescale$pop), 2)

ebola_total_maxent_IUCN_timescale <- ebola_total_maxent_IUCN_timescale %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ strigiformesGP + chiroptera_Extant_Maxent + colubridaeGP + carnivoraGP + hfp + precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") +
  f(ind2,model="iid")
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_total_maxent_IUCN_timescale, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-0.021  ,  -0.09,   0.264  ))






marburg_total_maxent_IUCN %>% summary()


#strigiformes - 9.00
#carnivora - 18.00
#colubridae - 20.00


marburg_total <- marburg_total %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 9, 1, 2))
marburg_total <- marburg_total %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant < 19, 1, 2))
marburg_total <- marburg_total %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 20, 1,  2))

marburg_total$strigiformesGP <-factor(marburg_total$strigiformesGP, 
                                      levels = c(1, 2))
marburg_total$carnivoraGP <-factor(marburg_total$carnivoraGP, 
                                   levels = c(1, 2))
marburg_total$colubridaeGP <-factor(marburg_total$colubridaeGP, 
                                    levels = c(1, 2))


### 1. 다중공선성만 제외한 포화모형. 
marburg_rev_1 <- glm(marburg_gp ~ chiroptera_Extant_Maxent + strigiformesGP + carnivoraGP +
                       colubridaeGP + hfp + precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                       proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                       proportion_14 
                     , 
                     data = marburg_total_maxent_IUCN, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)




