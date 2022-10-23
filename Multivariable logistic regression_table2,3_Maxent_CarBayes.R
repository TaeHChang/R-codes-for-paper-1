library(car)
library(tidyverse)
library(CARBayes)
library(CARBayesST)

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

ebola_timescale_maxent <- read.csv("D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_timescale_maxent.csv")

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

M.burnin <- 90000       # Number of burn-in iterations (discarded)
M <- 100000  
nrow(ebola_total_maxent)

# 1. spatial convolution model only (UH )

formula <-ebola ~  strigiformesGP + carnivoraGP + colubridaeGP  + hfp +
  precipitation_annual + temp_annual + 
  proportion_4 + proportion_5 + proportion_9 + proportion_10 + 
  proportion_14 

result1<-S.CARleroux(formula, family="binomial", trials = ebola_total_maxent$X,
              data=ebola_total_maxent,
              rho = 0,
              W = W,
              burnin = M.burnin,
              n.sample = M)
result1
result1$summary.results
result1$modelfit

exp(c(-0.4063 , -3.0631 , 2.3565))

# 2. spatial convolution model only (CH )
result2<-S.CARleroux(formula, family="binomial", trials = ebola_total_maxent$X,
                     data=ebola_total_maxent,
                     rho = 1,
                     W = W,
                     burnin = M.burnin,
                     n.sample = M)
result2

result2$summary.results
result2$modelfit

exp(c(-6.0048 ,-14.0607 ,-0.6940 ))


# 3. Convolution BYM
result3<-S.CARbym(formula, family="binomial", trials = ebola_total_maxent$X,
                     data=ebola_total_maxent,
                     W = W,
                     burnin = M.burnin,
                     n.sample = M)
result3
result3$summary.results
result3$modelfit

exp(c(0.0283 , -0.0804 , 0.1842))




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


formula <-ebola ~  strigiformesGP + carnivoraGP + colubridaeGP  + hfp +
  precipitation_annual + temp_annual + 
  proportion_4 + proportion_5 + proportion_10 

M.burnin <- 95000       # Number of burn-in iterations (discarded)
M <- 100000 

# model 3. convolution + timetrend
result4 <- ST.CARlinear(formula, family="binomial", trials = ebola_timescale_maxent$X, 
                        data=ebola_timescale_maxent,
                        W = W,
                        burnin = M.burnin,
                        rho.slo = 1,
                        rho.int = 1,
                        MALA = TRUE,
                        n.sample = M)



chain1 <- ST.CARar(formula, family="poisson", data=dat.ordered, W=W, 
                   burnin=2000, n.sample=22000, thin=1000, verbose=FALSE, AR=1)
chain2 <- ST.CARar(formula, family="poisson", data=dat.ordered, W=W, 
                   burnin=2000, n.sample=22000, thin=1000, verbose=FALSE, AR=1)
chain3 <- ST.CARar(formula, family="poisson", data=dat.ordered, W=W, 
                   burnin=2000, n.sample=22000, thin=1000, verbose=FALSE, AR=1)


#### Check convergence - traceplot
install.packages("coda")
library(coda)
beta.samples <- mcmc.list(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta)
plot(beta.samples)

result4
result4$summary.results
result4$modelfit

exp(c(-1.3807 , -3.8441 , 0.8492))

# model 4. convolution + randomwalk
result5 <- ST.CARanova(formula, family="binomial", trials = ebola_timescale_maxent$X, 
                        data=ebola_timescale_maxent,
                        W = W,
                        burnin = M.burnin,
                        interaction = FALSE,
                        MALA = FALSE,
                        n.sample = M)

result5
result5$summary.results
result5$modelfit

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
result6 <- ST.CARanova(formula, family="binomial", trials = ebola_timescale_maxent$X, 
                       data=ebola_timescale_maxent,
                       W = W,
                       burnin = M.burnin,
                       interaction = TRUE,
                       MALA = FALSE,
                       n.sample = M)

result6
result6$summary.results
result6$modelfit
exp(c(-0.3350 , -0.5139 , -0.1651))


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













################ IUCN_Maxent filtered

library(readr)

ebola_total_maxent_IUCN <- read.csv("D:\\Environmental data\\Species richness\\Maxent_IUCN\\ebola_total_maxent_IUCN.csv")
ebola_total_maxent_IUCN <- ebola_total_maxent_IUCN %>% 
  mutate(ebola_gp = ifelse(ebola >0, 1, 0))

ebola_total_maxent_IUCN$ebola_gp <-factor(ebola_total_maxent_IUCN$ebola_gp, levels = c(0, 1),
                                          labels = c("Not_occured", "Occured"))


ebola_total_maxent_IUCN_timescale <- read.csv("D:\\Environmental data\\Species richness\\Maxent_IUCN\\ebola_total_maxent_IUCN_timescale.csv")



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





# 1. spatial convolution model only (UH )
M.burnin <- 95000
M <- 100000

formula <-ebola ~  strigiformesGP + carnivoraGP + colubridaeGP  + hfp +
  precipitation_annual + temp_annual + 
  proportion_4 + proportion_5 + proportion_10 

result1<-S.CARleroux(formula, family="binomial", trials = ebola_total_maxent_IUCN$X,
                     data=ebola_total_maxent_IUCN,
                     rho = 0,
                     W = W,
                     burnin = M.burnin,
                     n.sample = M)
result1
result1$summary.results
result1$modelfit

exp(c(-0.3341 , -0.5921 ,-0.0300))

# 2. spatial convolution model only (CH )
result2<-S.CARleroux(formula, family="binomial", trials = ebola_total_maxent_IUCN$X,
                     data=ebola_total_maxent_IUCN,
                     rho = 1,
                     W = W,
                     burnin = M.burnin,
                     n.sample = M)
result2

result2$summary.results
result2$modelfit

exp(c(-0.9079 , -1.1592, -0.5798))


# 3. Convolution BYM
result3<-S.CARbym(formula, family="binomial", trials = ebola_total_maxent$X,
                  data=ebola_total_maxent,
                  W = W,
                  burnin = M.burnin,
                  n.sample = M)
result3
result3$summary.results
result3$modelfit

exp(c(0.0283 , -0.0804 , 0.1842))



# model 3. convolution + timetrend
result4 <- ST.CARlinear(formula, family="binomial", trials = ebola_total_maxent_IUCN_timescale$X, 
                        data=ebola_total_maxent_IUCN_timescale,
                        W = W,
                        burnin = M.burnin,
                        rho.slo = 1,
                        rho.int = 1,
                        MALA = TRUE,
                        n.sample = M)



result4
result4$summary.results
result4$modelfit

exp(c(-0.7484 , -0.9111 , -0.5603))

# model 4. convolution + randomwalk
result5 <- ST.CARanova(formula, family="binomial", trials = ebola_total_maxent_IUCN_timescale$X, 
                       data=ebola_total_maxent_IUCN_timescale,
                       W = W,
                       burnin = M.burnin,
                       interaction = FALSE,
                       MALA = FALSE,
                       n.sample = M)

result5
result5$summary.results
result5$modelfit

# 6. spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
result6 <- ST.CARanova(formula, family="binomial", trials = ebola_total_maxent_IUCN_timescale$X, 
                       data=ebola_total_maxent_IUCN_timescale,
                       W = W,
                       burnin = M.burnin,
                       interaction = TRUE,
                       MALA = FALSE,
                       n.sample = M)

result6
result6$summary.results
result6$modelfit
exp(c(-0.5594 , -0.6615 , -0.4366))









########## Maxent - only predators filtered
ebola_total_maxent_filtered <- read.csv("D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_total_maxent_filtered.csv")
ebola_timescale_maxent_filtered <- read.csv("D:\\Environmental data\\data_final\\Maxent\\Ebola\\ebola_timescale_maxent_filtered.csv")

