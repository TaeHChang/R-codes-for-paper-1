library(car)
library(mlogit)
library(boot)
library(ggplot2)
library(tidyverse)

######################################
#데이터 다시 정리
######################################

library(readr)
chiroptera_count <- read_csv("D:\\Environmental data\\Maxent\\Species_mat\\chiroptera_count.csv",
                             col_types = cols(...1 = col_skip()))
accipitriformes_count <- read_csv("D:\\Environmental data\\Maxent\\Species_mat\\accipitriformes_count.csv",
                             col_types = cols(...1 = col_skip()))
strigiformes_count <- read_csv("D:\\Environmental data\\Maxent\\Species_mat\\strigiformes_count.csv",
                             col_types = cols(...1 = col_skip()))
carnivora_count <- read_csv("D:\\Environmental data\\Maxent\\Species_mat\\carnivora_count.csv",
                             col_types = cols(...1 = col_skip()))
colubridae_count <- read_csv("D:\\Environmental data\\Maxent\\Species_mat\\colubridae_count.csv",
                             col_types = cols(...1 = col_skip()))

ebola_total_maxent <- ebola_total %>% 
  left_join(chiroptera_count, by = 'ID')

ebola_total_maxent <- ebola_total_maxent %>% 
  left_join(accipitriformes_count, by = 'ID')

ebola_total_maxent <- ebola_total_maxent %>% 
  left_join(strigiformes_count, by = 'ID')

ebola_total_maxent <- ebola_total_maxent %>% 
  left_join(carnivora_count, by = 'ID')

ebola_total_maxent <- ebola_total_maxent %>% 
  left_join(colubridae_count, by = 'ID')

write.csv(ebola_total_maxent, "D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_total_maxent.csv")



ebola_timescale_maxent <- ebola_timescale_final %>% 
  left_join(chiroptera_count, by = 'ID')

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(accipitriformes_count, by = 'ID')

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(strigiformes_count, by = 'ID')

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(carnivora_count, by = 'ID')

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(colubridae_count, by = 'ID')

write.csv(ebola_timescale_maxent, "D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_timescale_maxent.csv")



#######################################
### 예비분석. Maxent로 pixel 하나만 있어도 있는 것으로 간주하는 경우.
#######################################


############ Ebola
library(readr)
ebola_total_maxent <- read_csv("D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_total_maxent.csv", 
                        col_types = cols(...1 = col_skip()))

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total_maxent$ebola_gp <-factor(ebola_total_maxent$ebola_gp, levels = c(0, 1),
                              labels = c("Not_occured", "Occured"))


library(corrplot)

whole <- ebola_total_maxent %>% 
  dplyr::select(hfp, elevation, precipitation_annual, temp_annual, total_mean_GDP, forestcover_total,
                proportion_2, proportion_4,proportion_5,proportion_8,proportion_9,proportion_10,
                proportion_11,proportion_12,proportion_13,proportion_14, pop_dense, count_chiroptera_prop50,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_carnivora_prop50, 
                count_colubridae_prop50) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


whole <- whole %>% 
  dplyr::select(-elevation, -proportion_2, -proportion_13, -forestcover_total,
                -count_accipitriformes_prop50, -count_chiroptera_prop50, -count_colubridae_prop50)

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})



#### Table 1 넣기 위한 분석 
##  Chiroptera 
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_chiroptera_existence),
            SD = sd(count_chiroptera_existence))

ttest <- t.test(count_chiroptera_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_chiroptera_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_accipitriformes_existence),
            SD = sd(count_accipitriformes_existence))

ttest <- t.test(count_accipitriformes_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_accipitriformes_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_strigiformes_existence),
            SD = sd(count_strigiformes_existence))

ttest <- t.test(count_strigiformes_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_strigiformes_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_carnivora_existence),
            SD = sd(count_carnivora_existence))

ttest <- t.test(count_carnivora_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_carnivora_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_colubridae_existence),
            SD = sd(count_colubridae_existence))

ttest <- t.test(count_colubridae_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_colubridae_existence ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

### 1. 다중공선성만 제외한 포화모형. 
ebola_rev_1 <- glm(ebola_gp ~ count_strigiformes_existence + count_carnivora_existence +hfp +
                     precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14
                   , 
                   data = ebola_total_maxent, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)


#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(ebola_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(ebola_rev_1))



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
sum(is.na(ebola_total_maxent))
W <- nb2mat(nb, style = "B") 


ebola_total_maxent <- ebola_total_maxent %>% 
  rename(id = ID)
ebola_total_maxent$X <- rep(1:524)
ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(X2 = X)



# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~  count_strigiformes_existence + count_carnivora_existence +hfp +
  precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 + f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.047  , -0.007   ,   0.031))




ebola_timescale_maxent$X <- rep(1:524, 22)
ebola_timescale_maxent$X2 <- rep(1:524, 22)
ebola_timescale_maxent$year <- rep(1:22, each = 524)
ebola_timescale_maxent <- ebola_timescale_maxent %>% 
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

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(base_map, by = "ID")

ebola_timescale_maxent$pop <- as.numeric(ebola_timescale_maxent$pop)
ebola_timescale_maxent$pop[is.na(ebola_timescale_maxent$pop)] <- mean(ebola_timescale_maxent$pop, na.rm = T)
ebola_timescale_maxent$pop <- round(as.numeric(ebola_timescale_maxent$pop), 2)

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~  count_strigiformes_existence + count_carnivora_existence +hfp +
  precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") +
  f(ind2,model="iid")
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_timescale_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-0.058 ,  -0.025  ,    0.007))








############ Marburg
library(readr)
marburg_total <- read_csv("D:/Environmental data/data_final/marburg_total.csv", 
                          col_types = cols(...1 = col_skip()))

marburg_total_maxent <- marburg_total %>% 
  left_join(chiroptera_count, by = 'ID')

marburg_total_maxent <- marburg_total_maxent %>% 
  left_join(accipitriformes_count, by = 'ID')

marburg_total_maxent <- marburg_total_maxent %>% 
  left_join(strigiformes_count, by = 'ID')

marburg_total_maxent <- marburg_total_maxent %>% 
  left_join(carnivora_count, by = 'ID')

marburg_total_maxent <- marburg_total_maxent %>% 
  left_join(colubridae_count, by = 'ID')

write.csv(marburg_total_maxent, "D:\\Environmental data\\data_final\\Maxent\\Marburg\\marburg_total_maxent.csv")


marburg_total_maxent <- marburg_total_maxent %>% 
  mutate(marburg_gp = ifelse(marburg >0, 1, 0))

marburg_total_maxent$marburg_gp <-factor(marburg_total_maxent$marburg_gp, levels = c(0, 1),
                                  labels = c("Not_occured", "Occured"))




#### Table 1 넣기 위한 분석 
##  Chiroptera 
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_chiroptera_existence),
            SD = sd(count_chiroptera_existence))

ttest <- t.test(count_chiroptera_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_chiroptera_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_accipitriformes_existence),
            SD = sd(count_accipitriformes_existence))

ttest <- t.test(count_accipitriformes_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_accipitriformes_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_strigiformes_existence),
            SD = sd(count_strigiformes_existence))

ttest <- t.test(count_strigiformes_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_strigiformes_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_carnivora_existence),
            SD = sd(count_carnivora_existence))

ttest <- t.test(count_carnivora_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_carnivora_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_colubridae_existence),
            SD = sd(count_colubridae_existence))

ttest <- t.test(count_colubridae_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_colubridae_existence ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)





### 1. 다중공선성만 제외한 포화모형. 
marburg_rev_1 <- glm(marburg_gp ~ count_strigiformes_existence + count_carnivora_existence +hfp +
                       precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                       proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                       proportion_14 
                     , 
                     data = marburg_total_maxent , family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)


library(ROCR)
ld_result <- predict(marburg_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, marburg_total$marburg_gp)
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
logisticPseudoR2s(marburg_rev_1)



#### 잔차 및 진단통계량 분석
#데이터프레임에 스튜던트화잔차를 추가해준다.
#이를 통해서 자료의 이상치를 스크리닝 할 수 있다.
marburg_total$predicted.probabilities<-fitted(marburg_rev_1)
marburg_total$standardized.residuals<-rstandard(marburg_rev_1)
marburg_total$studentized.residuals<-rstudent(marburg_rev_1)
marburg_total$dfbeta<-dfbeta(marburg_rev_1)
marburg_total$dffit<-dffits(marburg_rev_1)
marburg_total$leverage<-hatvalues(marburg_rev_1)


#표준화 잔차가 절대값 2를 넘는 값이 5%이하여야 좋다. 
marburg_total$large.residual <- marburg_total$standardized.residuals > 2 | marburg_total$standardized.residuals < -2
#TRUE가 1의 값을 가진다는 것에 착안해서 sum 계산하기.
sum(marburg_total$large.residual)
#표준화잔차가 절대값 2를 넘는 값들의 다른 통계량들 소환하기
#표준화 잔차의 절대값이 3.29보다 큰건 없어야 하고, 2.58보다 큰 건 전체의 1%, 1.96보다 큰 것은 5% 보다 적어야 한다.
#모자값(leverage)는 표본의 평균 모자값의 두 배 또는 세 배를 넘지 않아야 한다. 
#평균 모자값 = 예측변수 개수 + 1 / 표본 수.
#공분산비는 1에 평균 모자 값의 세배를 더하거나 뺀 값을 넘지 않도록 한다.
marburg_total[marburg_total$large.residual , c("leverage", "standardized.residuals")]

marburg_total <- marburg_total %>% arrange(desc(standardized.residuals))
marburg_total <- marburg_total %>% arrange(desc(fitted)) ### 전반적으로 괜찮음.



#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(marburg_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(marburg_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(marburg_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(marburg_rev_1))














#######################################
### 예비분석. Maxent로 pixel이 grid 면적의 50% 이상 있어야 있다고 간주하는 경우.
#######################################


############ Ebola
library(readr)
ebola_total_maxent <- read_csv("D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_total_maxent.csv", 
                               col_types = cols(...1 = col_skip()))

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total_maxent$ebola_gp <-factor(ebola_total_maxent$ebola_gp, levels = c(0, 1),
                                     labels = c("Not_occured", "Occured"))


library(corrplot)

whole <- ebola_total_maxent %>% 
  dplyr::select(hfp, elevation, precipitation_annual, temp_annual, total_mean_GDP, forestcover_total,
                proportion_2, proportion_4,proportion_5,proportion_8,proportion_9,proportion_10,
                proportion_11,proportion_12,proportion_13,proportion_14, pop_dense, count_chiroptera_prop50,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_carnivora_prop50, 
                count_colubridae_prop50) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})


whole <- whole %>% 
  dplyr::select(-elevation, -proportion_2, -proportion_13, -forestcover_total,
                -count_accipitriformes_prop50, -count_chiroptera_prop50)

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})



#### Table 1 넣기 위한 분석 
##  Chiroptera 
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_chiroptera_prop50),
            SD = sd(count_chiroptera_prop50))

ttest <- t.test(count_chiroptera_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_chiroptera_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_accipitriformes_prop50),
            SD = sd(count_accipitriformes_prop50))

ttest <- t.test(count_accipitriformes_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_accipitriformes_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_strigiformes_prop50),
            SD = sd(count_strigiformes_prop50))

ttest <- t.test(count_strigiformes_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_strigiformes_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_carnivora_prop50),
            SD = sd(count_carnivora_prop50))

ttest <- t.test(count_carnivora_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_carnivora_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
ebola_total_maxent %>% 
  group_by(ebola_gp) %>% 
  summarise(mean = mean(count_colubridae_prop50),
            SD = sd(count_colubridae_prop50))

ttest <- t.test(count_colubridae_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_colubridae_prop50 ~ ebola_gp, data = ebola_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

### 1. 다중공선성만 제외한 포화모형. 
ebola_rev_1 <- glm(ebola_gp ~ count_strigiformes_prop50 + count_carnivora_prop50 + colubridaeGP + hfp +
                     precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14
                   , 
                   data = ebola_total_maxent, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)


#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(ebola_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(ebola_rev_1))



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
sum(is.na(ebola_total_maxent))
W <- nb2mat(nb, style = "B") 


ebola_total_maxent <- ebola_total_maxent %>% 
  rename(id = ID)
ebola_total_maxent$X <- rep(1:524)
ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(X2 = X)



# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~ count_strigiformes_prop50 + colubridaeGP + count_carnivora_prop50 + hfp +
  precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14+ f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-0.084 ,  -0.002  ,    0.065))




ebola_timescale_maxent$X <- rep(1:524, 22)
ebola_timescale_maxent$X2 <- rep(1:524, 22)
ebola_timescale_maxent$year <- rep(1:22, each = 524)
ebola_timescale_maxent <- ebola_timescale_maxent %>% 
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

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(base_map, by = "ID")

ebola_timescale_maxent$pop <- as.numeric(ebola_timescale_maxent$pop)
ebola_timescale_maxent$pop[is.na(ebola_timescale_maxent$pop)] <- mean(ebola_timescale_maxent$pop, na.rm = T)
ebola_timescale_maxent$pop <- round(as.numeric(ebola_timescale_maxent$pop), 2)

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ count_strigiformes_prop50 + count_carnivora_prop50 + colubridaeGP + hfp +
  precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") 
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_timescale_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-0.090  , -0.017   ,   0.042))


ebola_total_maxent %>% summary()



########### categorical로 해보기. colubidae 유의하게 나옴.

#accipitriformes_prop50 1.00/3.00/5.00
#strigiformes_prop50 1.00/2.00/3.00
#carnivora_prop50 4.00/7.00/15.00
#colubridae_prop50 - 3.00/5.00/9.00

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop50 < 1, 1, 
                               ifelse(count_colubridae_prop50 < 3, 2,
                                      ifelse(count_colubridae_prop50 < 5, 3, 4))))

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop50 < 1, 1, 
                               ifelse(count_strigiformes_prop50 < 2, 2,
                                      ifelse(count_strigiformes_prop50 < 3, 3, 4))))

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop50 < 3, 1, 
                               ifelse(count_colubridae_prop50 < 5, 2,
                                      ifelse(count_colubridae_prop50 < 9, 3, 4))))

ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop50 < 4, 1, 
                               ifelse(count_carnivora_prop50 < 7, 2,
                                      ifelse(count_carnivora_prop50 < 15, 3, 4))))


ebola_total_maxent$accipitriformesGP <-factor(ebola_total_maxent$accipitriformesGP, 
                                         levels = c(1, 2, 3, 4))
ebola_total_maxent$strigiformesGP <-factor(ebola_total_maxent$strigiformesGP, 
                                         levels = c(1, 2, 3, 4))
ebola_total_maxent$colubridaeGP <-factor(ebola_total_maxent$colubridaeGP, 
                                         levels = c(1, 2, 3, 4))
ebola_total_maxent$carnivoraGP <-factor(ebola_total_maxent$carnivoraGP, 
                                  levels = c(1, 2, 3, 4))


ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop50 < 1, 1, 
                                    ifelse(count_colubridae_prop50 < 3, 2,
                                           ifelse(count_colubridae_prop50 < 5, 3, 4))))

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop50 < 1, 1, 
                                 ifelse(count_strigiformes_prop50 < 2, 2,
                                        ifelse(count_strigiformes_prop50 < 3, 3, 4))))

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop50 < 3, 1, 
                               ifelse(count_colubridae_prop50 < 5, 2,
                                      ifelse(count_colubridae_prop50 < 9, 3, 4))))

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop50 < 4, 1, 
                              ifelse(count_carnivora_prop50 < 7, 2,
                                     ifelse(count_carnivora_prop50 < 15, 3, 4))))


ebola_timescale_maxent$accipitriformesGP <-factor(ebola_timescale_maxent$accipitriformesGP, 
                                         levels = c(1, 2, 3, 4))
ebola_timescale_maxent$strigiformesGP <-factor(ebola_timescale_maxent$strigiformesGP, 
                                         levels = c(1, 2, 3, 4))
ebola_timescale_maxent$colubridaeGP <-factor(ebola_timescale_maxent$colubridaeGP, 
                                         levels = c(1, 2, 3, 4))
ebola_timescale_maxent$carnivoraGP <-factor(ebola_timescale_maxent$carnivoraGP, 
                                         levels = c(1, 2, 3, 4))




### 1. 다중공선성만 제외한 포화모형. 
ebola_rev_1 <- glm(ebola_gp ~ strigiformesGP + carnivoraGP + colubridaeGP + hfp +
                     precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
                     proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                     proportion_14
                   , 
                   data = ebola_total_maxent, family = binomial(link = 'logit'), na.action = na.exclude)
summary(ebola_rev_1)
round(exp(ebola_rev_1$coefficients), 2) 
round(exp(confint(ebola_rev_1)), 2)


#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(ebola_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(ebola_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(ebola_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(ebola_rev_1))



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
sum(is.na(ebola_total_maxent))
W <- nb2mat(nb, style = "B") 


ebola_total_maxent <- ebola_total_maxent %>% 
  rename(id = ID)
ebola_total_maxent$X <- rep(1:524)
ebola_total_maxent <- ebola_total_maxent %>% 
  mutate(X2 = X)



# 2. spatial convolution model only (UH and CH effects)
# 포화모형. 아주 좋아. carnivora가 유의하다. 
formula2<-ebola ~strigiformesGP + carnivoraGP + colubridaeGP  + hfp +
  precipitation_annual + temp_annual + total_mean_GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14+ f(X, model="iid") +
  f(X2, model="besag",graph=g)

result2<-inla(formula2,family="binomial", Ntrials = X,
              data=ebola_total_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))
summary(result2)

result2$dic$dic
result2$waic$waic
result2$waic$p.eff
mean(result2$cpo$cpo)
exp(c(-2.239 ,  -0.803   ,   0.567 ))




ebola_timescale_maxent$X <- rep(1:524, 22)
ebola_timescale_maxent$X2 <- rep(1:524, 22)
ebola_timescale_maxent$year <- rep(1:22, each = 524)
ebola_timescale_maxent <- ebola_timescale_maxent %>% 
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

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  left_join(base_map, by = "ID")

ebola_timescale_maxent$pop <- as.numeric(ebola_timescale_maxent$pop)
ebola_timescale_maxent$pop[is.na(ebola_timescale_maxent$pop)] <- mean(ebola_timescale_maxent$pop, na.rm = T)
ebola_timescale_maxent$pop <- round(as.numeric(ebola_timescale_maxent$pop), 2)

ebola_timescale_maxent <- ebola_timescale_maxent %>% 
  mutate(pop_dense = pop / area *1000000) # 1 제곱키로미터 당 인구밀도 계산

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula6<-ebola ~ strigiformesGP + carnivoraGP + colubridaeGP  + hfp +
  precipitation_annual + temp_annual + GDP +  pop_dense +
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14  + f(X, model="iid") +
  f(X2, model="besag",graph=g) +
  f(year,model="rw1") +
  f(ind2,model="iid")
result6<-inla(formula6,family="binomial", Ntrials = X,
              data=ebola_timescale_maxent, control.predictor = list(compute=TRUE), 
              verbose=TRUE,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE, return.marginals.predictor=TRUE))

summary(result6)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
mean(result6$cpo$cpo)
exp(c(-2.102 ,  -0.814   ,   -0.418))











#### Table 1 넣기 위한 분석 
##  Chiroptera 
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_chiroptera_prop50),
            SD = sd(count_chiroptera_prop50))

ttest <- t.test(count_chiroptera_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_chiroptera_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Accipitriformes
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_accipitriformes_prop50),
            SD = sd(count_accipitriformes_prop50))

ttest <- t.test(count_accipitriformes_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_accipitriformes_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Strigiformes
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_strigiformes_prop50),
            SD = sd(count_strigiformes_prop50))

ttest <- t.test(count_strigiformes_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_strigiformes_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Carnivora
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_carnivora_prop50),
            SD = sd(count_carnivora_prop50))

ttest <- t.test(count_carnivora_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_carnivora_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)


##  Colubridae
marburg_total_maxent %>% 
  group_by(marburg_gp) %>% 
  summarise(mean = mean(count_colubridae_prop50),
            SD = sd(count_colubridae_prop50))

ttest <- t.test(count_colubridae_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)
wilcox <- wilcox.test(count_colubridae_prop50 ~ marburg_gp, data = marburg_total_maxent, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)

ttest 
wilcox

##effect size
t<-ttest$statistic[[1]]
df<-ttest$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)





### 1. 다중공선성만 제외한 포화모형. 
marburg_rev_1 <- glm(marburg_gp ~ strigiformesGP + carnivoraGP + colubridaeGP +hfp +
                       precipitation_annual.x + temp_annual + total_mean_GDP +  pop_dense +
                       proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
                       proportion_14 
                     , 
                     data = marburg_total_maxent , family = binomial(link = 'logit'), na.action = na.exclude)
summary(marburg_rev_1)
round(exp(marburg_rev_1$coefficients), 2) 
round(exp(confint(marburg_rev_1)), 2)


library(ROCR)
ld_result <- predict(marburg_rev_1) #학습 데이터의 예측값
pred <- prediction(ld_result, marburg_total$marburg_gp)
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
logisticPseudoR2s(marburg_rev_1)



#### 잔차 및 진단통계량 분석
#데이터프레임에 스튜던트화잔차를 추가해준다.
#이를 통해서 자료의 이상치를 스크리닝 할 수 있다.
marburg_total$predicted.probabilities<-fitted(marburg_rev_1)
marburg_total$standardized.residuals<-rstandard(marburg_rev_1)
marburg_total$studentized.residuals<-rstudent(marburg_rev_1)
marburg_total$dfbeta<-dfbeta(marburg_rev_1)
marburg_total$dffit<-dffits(marburg_rev_1)
marburg_total$leverage<-hatvalues(marburg_rev_1)


#표준화 잔차가 절대값 2를 넘는 값이 5%이하여야 좋다. 
marburg_total$large.residual <- marburg_total$standardized.residuals > 2 | marburg_total$standardized.residuals < -2
#TRUE가 1의 값을 가진다는 것에 착안해서 sum 계산하기.
sum(marburg_total$large.residual)
#표준화잔차가 절대값 2를 넘는 값들의 다른 통계량들 소환하기
#표준화 잔차의 절대값이 3.29보다 큰건 없어야 하고, 2.58보다 큰 건 전체의 1%, 1.96보다 큰 것은 5% 보다 적어야 한다.
#모자값(leverage)는 표본의 평균 모자값의 두 배 또는 세 배를 넘지 않아야 한다. 
#평균 모자값 = 예측변수 개수 + 1 / 표본 수.
#공분산비는 1에 평균 모자 값의 세배를 더하거나 뺀 값을 넘지 않도록 한다.
marburg_total[marburg_total$large.residual , c("leverage", "standardized.residuals")]

marburg_total <- marburg_total %>% arrange(desc(standardized.residuals))
marburg_total <- marburg_total %>% arrange(desc(fitted)) ### 전반적으로 괜찮음.



#오차의 독립성 가정을 평가하는 더빈-왓슨 검정.
durbinWatsonTest(marburg_rev_1) #양의 자기상관이 존재함.

#다중공선성 평가를 위한 VIF(분산팽창인자) 통계량. 10보다 작아야 한다.
vif(marburg_rev_1)
#VIF의 역수로 tolerence를 구한다. 0.2보다 커야 한다.
1/vif(marburg_rev_1)
#평균 VIF를 구한다. 1에서 멀면 좋지 않다.
mean(vif(marburg_rev_1))





marburg_total_maxent %>% summary()



########### categorical로 해보기. colubidae 유의하게 나옴.

#accipitriformes_prop50 1.00/2.00/8.00
#strigiformes_prop50 1.00/2.00/3.00
#carnivora_prop50 1.00/3.00/5.00
#colubridae_prop50 - 1.00/3.00/6.00

marburg_total_maxent <- marburg_total_maxent %>% 
  mutate(accipitriformesGP = ifelse(count_accipitriformes_prop50 < 1, 1, 
                                    ifelse(count_colubridae_prop50 < 3, 2,
                                           ifelse(count_colubridae_prop50 < 8, 3, 4))))

marburg_total_maxent <- marburg_total_maxent %>% 
  mutate(strigiformesGP = ifelse(count_strigiformes_prop50 < 1, 1, 
                                 ifelse(count_strigiformes_prop50 < 2, 2,
                                        ifelse(count_strigiformes_prop50 < 3, 3, 4))))

marburg_total_maxent <- marburg_total_maxent %>% 
  mutate(colubridaeGP = ifelse(count_colubridae_prop50 < 1, 1, 
                               ifelse(count_colubridae_prop50 < 3, 2,
                                      ifelse(count_colubridae_prop50 < 6, 3, 4))))

marburg_total_maxent <- marburg_total_maxent %>% 
  mutate(carnivoraGP = ifelse(count_carnivora_prop50 < 1, 1, 
                              ifelse(count_carnivora_prop50 < 3, 2,
                                     ifelse(count_carnivora_prop50 < 5, 3, 4))))


marburg_total_maxent$accipitriformesGP <-factor(marburg_total_maxent$accipitriformesGP, 
                                              levels = c(1, 2, 3, 4))
marburg_total_maxent$strigiformesGP <-factor(marburg_total_maxent$strigiformesGP, 
                                           levels = c(1, 2, 3, 4))
marburg_total_maxent$colubridaeGP <-factor(marburg_total_maxent$colubridaeGP, 
                                         levels = c(1, 2, 3, 4))
marburg_total_maxent$carnivoraGP <-factor(marburg_total_maxent$carnivoraGP, 
                                        levels = c(1, 2, 3, 4))



library(readr)
marburg_total_maxent <- read.csv("D:\\Environmental data\\data_final\\Maxent\\Marburg\\marburg_total_maxent.csv")

library(corrplot)

whole <- marburg_total_maxent %>% 
  dplyr::select(hfp, elevation, precipitation_annual.x, temp_annual, total_mean_GDP, forestcover_total,
                proportion_2, proportion_4,proportion_5,proportion_8,proportion_9,proportion_10,
                proportion_11,proportion_12,proportion_13,proportion_14, pop_dense, count_chiroptera_prop50,
                count_accipitriformes_prop50, count_strigiformes_prop50, count_carnivora_prop50, 
                count_colubridae_prop50) 

mwhole <- cor(whole, use="pairwise.complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mwhole, method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(10), addCoef.col = "black", cl.pos = "n", order = "AOE", type = {"lower"})
