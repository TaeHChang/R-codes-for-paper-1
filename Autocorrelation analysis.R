########### Moran's I 구하고, Autocorrelation의 구조 파악하기.
library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)
### 먼저 Moran's I를 계산하기 위해서는 neighbor에 대한 정보가 필요하므로, 
# 지금까지 다루던 최종데이터셋들을 shp 파일로 변환하여 다시 불러올 필요가 있다. 
# 그러면 최종 데이터셋들을 불러오고, 거기에 base_map의 geometry를 삽입한 다음에 그걸 QGIS에 넣어서 shp로 변환해보자.

library(readr)
ebola_total <- read.csv("D:\\Environmental data\\data_final\\ebola_total.csv")
marburg_total <- read.csv("D:\\Environmental data\\data_final\\marburg_total.csv")
ebola_timescale_final <- read_csv("D:/Environmental data/data_final/ebola_timescale/ebola_timescale_final.csv")
marburg_timescale_final <- read_csv("D:/Environmental data/data_final/marburg_timescale/marburg_timescale_final.csv")


############################### 안씀
library(rgdal)
base_map <- readOGR(dsn = "D:\\Environmental data\\Base_map_numpoints_2000\\base_map.shp")

base_map <- as.data.frame(base_map@data)

base_map <- base_map %>% 
  dplyr::select("id", "left", "top", "right", "bottom")

colnames(base_map) <- c("ID","left", "top", "right", "bottom")

ebola_total <- ebola_total %>% 
  left_join(base_map, by ="ID")
marburg_total  <- marburg_total  %>% 
  left_join(base_map, by ="ID")
ebola_timescale_final <- ebola_timescale_final %>% 
  left_join(base_map, by ="ID")
marburg_timescale_final <- marburg_timescale_final %>% 
  left_join(base_map, by ="ID")

write.csv(ebola_total, "D:\\Environmental data\\data_final\\final_data_shp\\ebola_total_shp.csv")
write.csv(marburg_total, "D:\\Environmental data\\data_final\\final_data_shp\\marburg_total_shp.csv")
write.csv(ebola_timescale_final, "D:\\Environmental data\\data_final\\final_data_shp\\ebola_timescale_final_shp.csv")
write.csv(marburg_timescale_final, "D:\\Environmental data\\data_final\\final_data_shp\\marburg_timescale_final_shp.csv")
########################################



## QGIS에서 base_map.shp에다가 csv 합쳐서 만들어낸 것.
ebola_total_shp <- sf::st_read("D:\\Environmental data\\data_final\\final_data_shp\\ebola_total.shp")
marburg_total_shp <- sf::st_read("D:\\Environmental data\\data_final\\final_data_shp\\marburg_total.shp")

ebola_total_shp <- ebola_total_shp %>% 
  filter(ebola_tota != is.na(ebola_tota))
marburg_total_shp <-marburg_total_shp %>% 
  filter(marburg_to != is.na(marburg_to))

st_write(ebola_total_shp, "D:\\Environmental data\\data_final\\final_data_shp\\shp\\ebola_total", driver = "ESRI Shapefile")
st_write(marburg_total_shp, "D:\\Environmental data\\data_final\\final_data_shp\\shp\\marburg_total", driver = "ESRI Shapefile")


### spatial linking structure
#공간적인 연결을 시각화하려고 할 때. 
library(spdep)

### 이 코드 사용해서 잘 안 맞는 도형 고칠 수 있고,
yer_object$geometry <- yer_object$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

### 혹은 이 코드를 쓰면 그냥 에러가 있더라도 강행하게 할 수 있다.
sf::sf_use_s2(FALSE)

poly.nlist<- spdep::poly2nb(ebola_total_shp)

# spatial weight 그리고 위에서 만든 공간가중행렬에 가중치 설정
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island #이웃이 없는 경우를 고려해준다.
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)


### test / ebola_total
# Moran test for global spatial autocorrelation
#부산의 smr 값에 대해서 공간가중치 행렬을 활용하여 모란 통계량 산출.
moran.test(ebola_total$ebola, listw, zero.policy=T)
# Monte Carlo Moran Test
bperm <-moran.mc(ebola_total$ebola, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot 
spdep::moran.plot(ebola_total$ebola, listw = listw, zero.policy=T)


### test / ebola_timescale_final
# 이 경우에는 지금 가중행렬이 길이가 안 맞는다고 하는데, 이거 어떻게 수정할 수 있는지만 찾아보면 될듯.
# Moran test for global spatial autocorrelation



##### Marburg
poly.nlist<- spdep::poly2nb(marburg_total_shp)

# spatial weight 그리고 위에서 만든 공간가중행렬에 가중치 설정
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island #이웃이 없는 경우를 고려해준다.
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)


### test / ebola_total
# Moran test for global spatial autocorrelation
#부산의 smr 값에 대해서 공간가중치 행렬을 활용하여 모란 통계량 산출.
moran.test(marburg_total$marburg, listw, zero.policy=T)
# Monte Carlo Moran Test
bperm <-moran.mc(ebola_total$ebola, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot 
spdep::moran.plot(marburg_total$marburg, listw = listw, zero.policy=T)










################################################################################
##################### 본격적으로 spatial-temporal autocorrelation 분석 해보기
##############################################################################


######## Ebola 예비분석 데이터 spatial만.
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

ebola_total <- 
  ebola_total %>% rename(id = ID)
ebola_total <- ebola_total %>% 
  mutate(X2 = X)



# 1. uncorrelated spatial heterogeneity (UH effects)
#위에서 본거. only spatial UH model
formula1<- ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14 +total_mean_GDP + f(id, model="iid")

result1<-inla(formula1,family="binomial", 
              data=ebola_total, Ntrials = X, control.family = list(link = "logit"),
              control.predictor = list(compute=TRUE), verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result1)
result1$dic$dic
result1$waic$waic
result1$waic$p.eff
mean(result1$cpo$cpo)

# 2. spatial convolution model only (UH and CH effects)
# 이것도 아직 spatial effect만 고려함.
### Ebola 예비분석 데이터는 이 모델이 가장 적합도 높음. 
formula2<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14 +total_mean_GDP + f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.023,    0.016,      0.048))



### 2-2. 2번 모델이 가장 적합도가 높았으므로, 여기서 변수를 조금 더 줄일 수도 있을지 시도해보는 것.
#근데 희한하게 원래 했던게 제일 낫네 ㅎㅎ
formula2_2<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual +  proportion_10 + total_mean_GDP + proportion_2 + proportion_14 +
  f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2_2<-inla(formula2_2,family="binomial", Ntrials = X,
              data=ebola_total, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2_2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff



### 3. BYM2 model 
#convolution이랑 비슷한 메커니즘이고, 이를 보완하기 위해서 개발.
formula3<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14 +total_mean_GDP + f(X2, model="bym2",graph=g) 

result3<-inla(formula3,family="binomial", Ntrials = X,
              data=ebola_total, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result3)
exp(0.016) 
result3$dic$dic
result3$waic$waic
result3$waic$p.eff
mean(result3$cpo$cpo)










############## Ebola main dataset 분석. spatio-temporal 
#### 여기서 지금 X, X2, year, ind2를 넣는 이유는 다음과 같다.
#먼저 X를 넣는 이유는, 정확히 원인을 알 수 없으나 nb 객체를 만들 때 gird id를 지정해도 그걸 인식을 못하고
#자꾸 자기 마음대로 id를 row 순서대로 생성해서 넣기 때문. 그래서 그거 맞추려고 dataset에도 row 순서로 
#맞춰서 X를 생성하는 것이다.
#X2를 굳이 만드는 이유는, 한 formula에서 X가 두군데 들어가면 중복으로 인식해서, 그냥 column을 복붙해서 하나 더 만드는 것.
#year는 temporal 한 분석하려고 넣는 것이고, 
#ind2는 6번의 space-time interaction 항을 만들 때 필요하다. 
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()


ebola_timescale_final <- read_csv("D:/Environmental data/data_final/ebola_timescale/ebola_timescale_final.csv")
ebola_timescale_final$X <- rep(1:524, 22)
ebola_timescale_final$X2 <- rep(1:524, 22)
ebola_timescale_final$year <- rep(1:22, each = 524)
ebola_timescale_final <- ebola_timescale_final %>% 
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

ebola_timescale_final <- ebola_timescale_final %>% 
  left_join(base_map, by = "ID")

ebola_timescale_final$pop <- as.numeric(ebola_timescale_final$pop)
ebola_timescale_final$pop[is.na(ebola_timescale_final$pop)] <- mean(ebola_timescale_final$pop, na.rm = T)
ebola_timescale_final$pop <- round(as.numeric(ebola_timescale_final$pop), 2)

ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산


# 1. uncorrelated spatial heterogeneity (UH effects)
#위에서 본거. only spatial UH model
formula1<- ebola ~  accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  +  pop_dense + f(X, model="iid")

result1<-inla(formula1,family="binomial", 
              data=ebola_timescale_final, Ntrials = X, control.family = list(link = "logit"),
              control.predictor = list(compute=TRUE), verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result1)
result1$dic$dic
result1$waic$waic
result1$waic$p.eff
mean(result1$cpo$cpo)

# 2. spatial convolution model only (UH and CH effects)
# 이것도 아직 spatial effect만 고려함.
### Ebola 예비분석 데이터는 이 모델이 가장 적합도 높음. 
formula2<-ebola ~  accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  +  pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.023,    0.016,      0.048))



### 3. BYM2 model 
#convolution이랑 비슷한 메커니즘이고, 이를 보완하기 위해서 개발.
formula3<-ebola ~  accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  + pop_dense + f(X2, model="bym2",graph=g) 

result3<-inla(formula3,family="binomial", Ntrials = X,
              data=ebola_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result3)
exp(0.016) 
result3$dic$dic
result3$waic$waic
result3$waic$p.eff
mean(result3$cpo$cpo)


# 4. spatial convolution + time trend model
#13.2의 모델에다가, year를 넣어서 time trend 포함.
formula4<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  +  pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  year
result4<-inla(formula4,family="binomial", Ntrials = X,
              data=ebola_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result4)
result4$dic$dic
result4$waic$waic
result4$waic$p.eff
mean(result4$cpo$cpo)

# 5. spatial convolution + temporal random walk 

formula5<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  +  pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") #단순한 time trend가 아니고, time random walk 지정하기.

result5<-inla(formula5,family="binomial", Ntrials = X,
              data=ebola_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result5)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result5$cpo$cpo)


# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ accipitriformes_Extant + chiroptera_Extant + colubridae_Extant + 
  precipitation_annual + temp_annual + proportion_2 + proportion_10 + 
  proportion_14  + pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") +
  f(ind2,model="iid")
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-0.022,    0.004     , 0.027))













############## Ebola main dataset 분석. spatio-temporal 
#### 여기서 지금 X, X2, year, ind2를 넣는 이유는 다음과 같다.
#먼저 X를 넣는 이유는, 정확히 원인을 알 수 없으나 nb 객체를 만들 때 gird id를 지정해도 그걸 인식을 못하고
#자꾸 자기 마음대로 id를 row 순서대로 생성해서 넣기 때문. 그래서 그거 맞추려고 dataset에도 row 순서로 
#맞춰서 X를 생성하는 것이다.
#X2를 굳이 만드는 이유는, 한 formula에서 X가 두군데 들어가면 중복으로 인식해서, 그냥 column을 복붙해서 하나 더 만드는 것.
#year는 temporal 한 분석하려고 넣는 것이고, 
#ind2는 6번의 space-time interaction 항을 만들 때 필요하다. 
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()

marburg_total_shp <- sf::st_read("D:\\Environmental data\\data_final\\final_data_shp\\marburg_total.shp")
marburg_total_shp <-marburg_total_shp %>% 
  filter(marburg_to != is.na(marburg_to))

marburg_timescale_final <- read_csv("D:/Environmental data/data_final/marburg_timescale/marburg_timescale_final.csv")
marburg_timescale_final$X <- rep(1:197, 22)
marburg_timescale_final$X2 <- rep(1:197, 22)
marburg_timescale_final$year <- rep(1:22, each = 197)
marburg_timescale_final <- marburg_timescale_final %>% 
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

marburg_timescale_final <- marburg_timescale_final %>% 
  left_join(base_map, by = "ID")

marburg_timescale_final$pop <- as.numeric(marburg_timescale_final$pop)
marburg_timescale_final$pop[is.na(marburg_timescale_final$pop)] <- mean(marburg_timescale_final$pop, na.rm = T)
marburg_timescale_final$pop <- round(as.numeric(marburg_timescale_final$pop), 2)

marburg_timescale_final <- marburg_timescale_final %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산


library(INLA)
library(spdep)
marburg_total_shp$id <- as.numeric(marburg_total_shp$id)

marburg_total_shp <- marburg_total_shp %>% 
  dplyr::arrange(id)
?arrange
sf::sf_use_s2(FALSE)
nb <- spdep::poly2nb(as(marburg_total_shp, "Spatial"))
head(nb)
nb2INLA("map.adj",nb)
g <- inla.read.graph(filename ="map.adj")
is.na(g)
sum(is.na(marburg_total))
W <- nb2mat(nb, style = "B")


# 1. time trend model + year를 넣어서 time trend만 포함.
formula1<-marburg ~ colubridae_Extant + carnivora_Extant +  temp_annual + 
  GDP + proportion_10 + proportion_4 + proportion_14 + pop_dense +
  year
result1<-inla(formula1,family="binomial", Ntrials = X,
              data=marburg_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result1)
result4$dic$dic
result4$waic$waic
result4$waic$p.eff
mean(result1$cpo$cpo, na.rm= T)
exp(c( -0.040,   -0.036,     -0.033 ))


# 2. temporal random walk만 포함 

formula2<-marburg ~ colubridae_Extant+ carnivora_Extant +  temp_annual + 
  GDP + proportion_10 + proportion_4 + proportion_14 + pop_dense +
  f(year,model="rw1") #단순한 time trend가 아니고, time random walk 지정하기.

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=marburg_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result2)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result2$cpo$cpo, na.rm= T)



# 4. time trend model + year를 넣어서 time trend 포함.
formula4<-marburg ~ colubridae_Extant + carnivora_Extant + temp_annual + 
  GDP + proportion_10 + proportion_4 + proportion_14 + pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  year
result4<-inla(formula4,family="binomial", Ntrials = X,
              data=marburg_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result4)
result4$dic$dic
result4$waic$waic
result4$waic$p.eff
mean(result4$cpo$cpo, na.rm= T)

# 5. spatial convolution + temporal random walk 

formula5<-marburg ~ colubridae_Extant + carnivora_Extant + temp_annual + 
  GDP + proportion_10 + proportion_4 + proportion_14 + pop_dense  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") #단순한 time trend가 아니고, time random walk 지정하기.

result5<-inla(formula5,family="binomial", Ntrials = X,
              data=marburg_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result5)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result5$cpo$cpo, na.rm= T)


# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-marburg ~ chiroptera_Extant + colubridae_Extant + carnivora_Extant + temp_annual + 
  GDP + proportion_10 + proportion_4 + proportion_14 + pop_dense + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  year +
  f(ind2,model="iid")

result6<-inla(formula6,family="binomial", Ntrials = X,
              data=marburg_timescale_final, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo, na.rm= T)
exp(c(-0.016 ,   0.017   ,   0.046))




