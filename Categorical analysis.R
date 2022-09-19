library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)


#######################################
### 예비분석.
#######################################


############ Ebola
library(readr)
ebola_total <- read_csv("D:/Environmental data/data_final/ebola_total.csv", 
                        col_types = cols(...1 = col_skip()))

ebola_total %>% summary()

#accipitriformes - 17.00/21.00/24.00
#strigiformes - 8.00/9.00/11.00
#carnivora - 18.00/20.00/22.00
#colubridae - 12.00/19.00/24.00


ebola_total <- ebola_total %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant < 17, 1, 
                         ifelse(accipitriformes_Extant < 21, 2,
                         ifelse(accipitriformes_Extant < 24, 3,4))))

ebola_total <- ebola_total %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 8, 1, 
                                    ifelse(strigiformes_Extant < 9, 2,
                                           ifelse(strigiformes_Extant < 11, 3,4))))
ebola_total <- ebola_total %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant < 18, 1, 
                                    ifelse(carnivora_Extant < 20, 2,
                                           ifelse(carnivora_Extant < 22, 3,4))))
ebola_total <- ebola_total %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 19, 1, 
                                                     ifelse(colubridae_Extant < 24, 2, 3)))

ebola_total$accipitriformesGP <-factor(ebola_total$accipitriformesGP, 
                                    levels = c(1, 2, 3, 4))
ebola_total$strigiformesGP <-factor(ebola_total$strigiformesGP, 
                                       levels = c(1, 2, 3, 4))
ebola_total$carnivoraGP <-factor(ebola_total$carnivoraGP, 
                                       levels = c(1, 2, 3, 4))
ebola_total$colubridaeGP <-factor(ebola_total$colubridaeGP, 
                                       levels = c(1, 2, 3))



ebola_timescale_final <- read_csv("D:/Environmental data/data_final/ebola_timescale/ebola_timescale_final.csv")
ebola_timescale_final$X <- rep(1:524, 22)
ebola_timescale_final$X2 <- rep(1:524, 22)
ebola_timescale_final$year <- rep(1:22, each = 524)
ebola_timescale_final <- ebola_timescale_final %>% 
  rename(ind2 = ...1)

ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant < 17, 1, 
                                    ifelse(accipitriformes_Extant < 21, 2,
                                           ifelse(accipitriformes_Extant < 24, 3,4))))

ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 8, 1, 
                                 ifelse(strigiformes_Extant < 9, 2,
                                        ifelse(strigiformes_Extant < 11, 3,4))))
ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant < 18, 1, 
                              ifelse(carnivora_Extant < 20, 2,
                                     ifelse(carnivora_Extant < 22, 3,4))))
ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 19, 1, 
                               ifelse(colubridae_Extant < 24, 2, 3)))

ebola_timescale_final$accipitriformesGP <-factor(ebola_timescale_final$accipitriformesGP, 
                                       levels = c(1, 2, 3, 4))
ebola_timescale_final$strigiformesGP <-factor(ebola_timescale_final$strigiformesGP, 
                                    levels = c(1, 2, 3, 4))
ebola_timescale_final$carnivoraGP <-factor(ebola_timescale_final$carnivoraGP, 
                                 levels = c(1, 2, 3, 4))
ebola_timescale_final$colubridaeGP <-factor(ebola_timescale_final$colubridaeGP, 
                                  levels = c(1, 2, 3))

summary(ebola_total$accipitriformesGP)
summary(ebola_total$strigiformesGP)
summary(ebola_total$carnivoraGP)
summary(ebola_total$colubridaeGP)

ebola_total %>% 
  group_by(accipitriformesGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(strigiformesGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(carnivoraGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(colubridaeGP) %>% 
  summarize(n = sum(ebola))

ebola <- glm(ebola_gp ~ colubridaeGP
                   , 
                   data = ebola_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola)
round(exp(ebola$coefficients), 2) 
round(exp(confint(ebola)), 2)



### 1. 다중공선성만 제외한 포화모형. 
ebola_total <- ebola_total %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total$ebola_gp <-factor(ebola_total$ebola_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


ebola_rev_1 <- glm(ebola_gp ~ accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
                     hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14 
                   , 
                   data = ebola_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)


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


ebola_total <- ebola_total %>% 
  rename(id = ID)
ebola_total$X <- rep(1:524)
ebola_total <- ebola_total %>% 
  mutate(X2 = X)


# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~   accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
  hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 + f(X, model="iid") +
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
exp(c( 0.024  ,  0.094   ,   0.177))



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

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
  hfp + precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
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
exp(c(0.042  ,  0.099   ,   0.162))




library(ROCR)
ld_result <- predict(ebola_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, ebola_total$ebola_gp)
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]] 
AUC

###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(ebola_rev_1)



#### 잔차 및 진단통계량 분석
#데이터프레임에 스튜던트화잔차를 추가해준다.
#이를 통해서 자료의 이상치를 스크리닝 할 수 있다.
ebola_total$predicted.probabilities<-fitted(ebola_rev_1)
ebola_total$standardized.residuals<-rstandard(ebola_rev_1)
ebola_total$studentized.residuals<-rstudent(ebola_rev_1)
ebola_total$dfbeta<-dfbeta(ebola_rev_1)
ebola_total$dffit<-dffits(ebola_rev_1)
ebola_total$leverage<-hatvalues(ebola_rev_1)


#표준화 잔차가 절대값 2를 넘는 값이 5%이하여야 좋다. 
ebola_total$large.residual <- ebola_total$standardized.residuals > 2 | ebola_total$standardized.residuals < -2
#TRUE가 1의 값을 가진다는 것에 착안해서 sum 계산하기.
sum(ebola_total$large.residual)
#표준화잔차가 절대값 2를 넘는 값들의 다른 통계량들 소환하기
#표준화 잔차의 절대값이 3.29보다 큰건 없어야 하고, 2.58보다 큰 건 전체의 1%, 1.96보다 큰 것은 5% 보다 적어야 한다.
#모자값(leverage)는 표본의 평균 모자값의 두 배 또는 세 배를 넘지 않아야 한다. 
#평균 모자값 = 예측변수 개수 + 1 / 표본 수.
#공분산비는 1에 평균 모자 값의 세배를 더하거나 뺀 값을 넘지 않도록 한다.
ebola_total[ebola_total$large.residual , c("leverage", "standardized.residuals")]

ebola_total <- ebola_total %>% arrange(desc(standardized.residuals))
ebola_total <- ebola_total %>% arrange(desc(fitted)) ### 전반적으로 괜찮음.



#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(ebola_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(ebola_rev_1))



















############ Marburg
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                        col_types = cols(...1 = col_skip()))

marburg_total %>% summary()

#accipitriformes - 18.00/22.00/27.00
#strigiformes - 9.00/11.00/12.00
#carnivora - 19.00/21.00/26.00
#colubridae - 12.00/19.00/24.00


marburg_total <- marburg_total %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant < 22, 1, 
                                    ifelse(accipitriformes_Extant < 31, 2,3)))

marburg_total <- marburg_total %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 10, 1, 
                                 ifelse(strigiformes_Extant < 12, 2, 3)))
marburg_total <- marburg_total %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant < 21, 1, 
                              ifelse(carnivora_Extant < 26, 2,3)))
marburg_total <- marburg_total %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 16, 1, 
                               ifelse(colubridae_Extant < 24, 2, 3)))

marburg_total$accipitriformesGP <-factor(marburg_total$accipitriformesGP, 
                                       levels = c(1, 2, 3))
marburg_total$strigiformesGP <-factor(marburg_total$strigiformesGP, 
                                    levels = c(1, 2, 3))
marburg_total$carnivoraGP <-factor(marburg_total$carnivoraGP, 
                                 levels = c(1, 2, 3))
marburg_total$colubridaeGP <-factor(marburg_total$colubridaeGP, 
                                  levels = c(1, 2, 3))



summary(marburg_total$accipitriformesGP)
summary(marburg_total$strigiformesGP)
summary(marburg_total$carnivoraGP)
summary(marburg_total$colubridaeGP)

marburg_total %>% 
  group_by(accipitriformesGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(strigiformesGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(carnivoraGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(colubridaeGP) %>% 
  summarize(n = sum(marburg))

marburg <- glm(marburg_gp ~ colubridaeGP
             , 
             data = marburg_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg)
round(exp(marburg$coefficients), 2) 
round(exp(confint(marburg)), 2)



### 1. 다중공선성만 제외한 포화모형. 
marburg_total <- marburg_total %>% 
  mutate(marburg_gp = ifelse(marburg >0, 1, 0))

marburg_total$marburg_gp <-factor(marburg_total$marburg_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


marburg_rev_1 <- glm(marburg_gp ~ strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
                     hfp + precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                      proportion_9 + proportion_10  + proportion_14
                   , 
                   data = marburg_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)


vif(marburg_rev_1)














#################################################################
########################## Binary 변수로 구분하기
#################################################################

############ Ebola
library(readr)
ebola_total <- read_csv("D:/Environmental data/data_final/ebola_total.csv", 
                        col_types = cols(...1 = col_skip()))

ebola_total %>% summary()

#accipitriformes - 21.00
#strigiformes - 9.00
#carnivora - 20.00
#colubridae - 19.00


ebola_total <- ebola_total %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant  < 21, 1, 2))

ebola_total <- ebola_total %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 9, 1, 2))
ebola_total <- ebola_total %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant  < 20, 1, 2))
ebola_total <- ebola_total %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 19, 1,  2))

ebola_total$accipitriformesGP <-factor(ebola_total$accipitriformesGP, 
                                       levels = c(1, 2))
ebola_total$strigiformesGP <-factor(ebola_total$strigiformesGP, 
                                    levels = c(1, 2))
ebola_total$carnivoraGP <-factor(ebola_total$carnivoraGP, 
                                 levels = c(1, 2))
ebola_total$colubridaeGP <-factor(ebola_total$colubridaeGP, 
                                  levels = c(1, 2))



ebola_timescale_final <- read_csv("D:/Environmental data/data_final/ebola_timescale/ebola_timescale_final.csv")
ebola_timescale_final$X <- rep(1:524, 22)
ebola_timescale_final$X2 <- rep(1:524, 22)
ebola_timescale_final$year <- rep(1:22, each = 524)
ebola_timescale_final <- ebola_timescale_final %>% 
  rename(ind2 = ...1)

ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant  < 21, 1, 2))

ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 9, 1, 2))
ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant  < 20, 1, 2))
ebola_timescale_final <- ebola_timescale_final %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 19, 1,  2))

ebola_timescale_final$accipitriformesGP <-factor(ebola_timescale_final$accipitriformesGP, 
                                       levels = c(1, 2))
ebola_timescale_final$strigiformesGP <-factor(ebola_timescale_final$strigiformesGP, 
                                    levels = c(1, 2))
ebola_timescale_final$carnivoraGP <-factor(ebola_timescale_final$carnivoraGP, 
                                 levels = c(1, 2))
ebola_timescale_final$colubridaeGP <-factor(ebola_timescale_final$colubridaeGP, 
                                  levels = c(1, 2))

summary(ebola_total$accipitriformesGP)
summary(ebola_total$strigiformesGP)
summary(ebola_total$carnivoraGP)
summary(ebola_total$colubridaeGP)

ebola_total %>% 
  group_by(accipitriformesGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(strigiformesGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(carnivoraGP) %>% 
  summarize(n = sum(ebola))

ebola_total %>% 
  group_by(colubridaeGP) %>% 
  summarize(n = sum(ebola))

ebola <- glm(ebola_gp ~ colubridaeGP
             , 
             data = ebola_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola)
round(exp(ebola$coefficients), 2) 
round(exp(confint(ebola)), 2)



### 1. 다중공선성만 제외한 포화모형. 
ebola_total <- ebola_total %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total$ebola_gp <-factor(ebola_total$ebola_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


ebola_rev_1 <- glm(ebola_gp ~ accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
                     hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14 
                   , 
                   data = ebola_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)


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


ebola_total <- ebola_total %>% 
  rename(id = ID)
ebola_total$X <- rep(1:524)
ebola_total <- ebola_total %>% 
  mutate(X2 = X)


# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~   accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
  hfp + precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 + f(X, model="iid") +
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
exp(c( 0.024  ,  0.094   ,   0.177))



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

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ accipitriformesGP + strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
  hfp + precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
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
exp(c(0.042  ,  0.099   ,   0.162))




library(ROCR)
ld_result <- predict(ebola_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, ebola_total$ebola_gp)
AUC <- performance(pred, measure = "auc")
AUC <- AUC@y.values[[1]] 
AUC

###로지스틱 회귀모형에서 사용 가능한 측도를 계산하는 함수.
#선형회귀에서와 비슷하게 결정계수의 근사치로 활용할 수 있다.
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
logisticPseudoR2s(ebola_rev_1)



#### 잔차 및 진단통계량 분석
#데이터프레임에 스튜던트화잔차를 추가해준다.
#이를 통해서 자료의 이상치를 스크리닝 할 수 있다.
ebola_total$predicted.probabilities<-fitted(ebola_rev_1)
ebola_total$standardized.residuals<-rstandard(ebola_rev_1)
ebola_total$studentized.residuals<-rstudent(ebola_rev_1)
ebola_total$dfbeta<-dfbeta(ebola_rev_1)
ebola_total$dffit<-dffits(ebola_rev_1)
ebola_total$leverage<-hatvalues(ebola_rev_1)


#표준화 잔차가 절대값 2를 넘는 값이 5%이하여야 좋다. 
ebola_total$large.residual <- ebola_total$standardized.residuals > 2 | ebola_total$standardized.residuals < -2
#TRUE가 1의 값을 가진다는 것에 착안해서 sum 계산하기.
sum(ebola_total$large.residual)
#표준화잔차가 절대값 2를 넘는 값들의 다른 통계량들 소환하기
#표준화 잔차의 절대값이 3.29보다 큰건 없어야 하고, 2.58보다 큰 건 전체의 1%, 1.96보다 큰 것은 5% 보다 적어야 한다.
#모자값(leverage)는 표본의 평균 모자값의 두 배 또는 세 배를 넘지 않아야 한다. 
#평균 모자값 = 예측변수 개수 + 1 / 표본 수.
#공분산비는 1에 평균 모자 값의 세배를 더하거나 뺀 값을 넘지 않도록 한다.
ebola_total[ebola_total$large.residual , c("leverage", "standardized.residuals")]

ebola_total <- ebola_total %>% arrange(desc(standardized.residuals))
ebola_total <- ebola_total %>% arrange(desc(fitted)) ### 전반적으로 괜찮음.



#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(ebola_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(ebola_rev_1))



















############ Marburg
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                          col_types = cols(...1 = col_skip()))

marburg_total %>% summary()

#accipitriformes - 22.00
#strigiformes - 11.00
#carnivora - 21.00
#colubridae - 19.00


marburg_total <- marburg_total %>% 
  mutate(accipitriformesGP = ifelse(accipitriformes_Extant < 22, 1, 2))

marburg_total <- marburg_total %>% 
  mutate(strigiformesGP = ifelse(strigiformes_Extant < 11, 1, 2))
marburg_total <- marburg_total %>% 
  mutate(carnivoraGP = ifelse(carnivora_Extant < 21, 1, 2))
marburg_total <- marburg_total %>% 
  mutate(colubridaeGP = ifelse(colubridae_Extant < 19, 1,  2))

marburg_total$accipitriformesGP <-factor(marburg_total$accipitriformesGP, 
                                         levels = c(1, 2))
marburg_total$strigiformesGP <-factor(marburg_total$strigiformesGP, 
                                      levels = c(1, 2))
marburg_total$carnivoraGP <-factor(marburg_total$carnivoraGP, 
                                   levels = c(1, 2))
marburg_total$colubridaeGP <-factor(marburg_total$colubridaeGP, 
                                    levels = c(1, 2))



summary(marburg_total$accipitriformesGP)
summary(marburg_total$strigiformesGP)
summary(marburg_total$carnivoraGP)
summary(marburg_total$colubridaeGP)

marburg_total %>% 
  group_by(accipitriformesGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(strigiformesGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(carnivoraGP) %>% 
  summarize(n = sum(marburg))

marburg_total %>% 
  group_by(colubridaeGP) %>% 
  summarize(n = sum(marburg))

marburg <- glm(marburg_gp ~ colubridaeGP
               , 
               data = marburg_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg)
round(exp(marburg$coefficients), 2) 
round(exp(confint(marburg)), 2)



### 1. 다중공선성만 제외한 포화모형. 
marburg_total <- marburg_total %>% 
  mutate(marburg_gp = ifelse(marburg >0, 1, 0))

marburg_total$marburg_gp <-factor(marburg_total$marburg_gp, levels = c(0, 1),
                                  labels = c("Not_occured", "Occured"))


marburg_rev_1 <- glm(marburg_gp ~ strigiformesGP + chiroptera_Extant + colubridaeGP + carnivoraGP +
                       hfp + precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                       proportion_9 + proportion_10  + proportion_14
                     , 
                     data = marburg_total, family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)


vif(marburg_rev_1)


