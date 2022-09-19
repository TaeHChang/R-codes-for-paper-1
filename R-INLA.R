#### 시공간역학 실습 시간에 했던 것들 코드 가져와서 넣어두기.

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# INLA 불러오기 

library("INLA")



############ 11장

library(INLA); library(dplyr); library(ggplot2)

# distribution 에 대해서 공부한다.
?dnorm #normal distribution
?dpois  #poisson distribution
?dbinom #binomial distribution


# Plot normal distribution
x = seq(-5,5,0.001)
plot(x,dnorm(x),type = "l")

#여기서 지금 0.025가 아니고 0.05여서 1.96이 아닌 1.64가 나오는거다.
# 누적분포함수 pnorm 
pnorm(-1.64)
pnorm(1.64)

# 분위수함수 qnorm
qnorm(0.05)
qnorm(0.95)

# bulid dataframe
x <- seq(-5,5,0.01)
y <- dnorm(x)
data.frame(x,y)

# ggplot for pnorm & qnorm 
data.frame(x,y) %>% ggplot(aes(x,y)) + 
  geom_line()+ theme_bw() + labs(title = "Normal Distribution")

data.frame(x,y) %>% ggplot(aes(x,y)) + geom_line() +
  geom_vline(xintercept = -1.64, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(x,y),x < -1.64),fill = "black") + 
  labs(title = "pnorm(-1.64)")

data.frame(x,y) %>% ggplot(aes(x,y)) + geom_line() +
  geom_vline(xintercept = 1.64, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(x,y),x < 1.64),fill = "black") + 
  labs(title = "pnorm(1.64)")


# inla model (10주차 실습)
#그때 실습할 때 사용한 모델 가져와서 다시 실습하는거.
prior.prec <- list(prec = list(prior="pc.prec", 
                               param = c(1,0.01)))

formula <- r ~ f(hospital, model="iid", hyper =prior.prec)

res <- inla(formula,
            data = Surg,
            family = "binomial", Ntrials = n,
            control.predictor = list(compute=TRUE),
            control.compute=list(dic=TRUE))

# Fixed effect.여기서 mean 값 사용해서 지도화 했었음. 
res$summary.fitted.values

### 1.2.2 Posterior marginals of inla()
res$marginals.fixed 

res$marginals.random 


### 1.2.3 posterior distribution of parameter intercept(alpha)
#logit(p) = alpha + u 
# 모형의 절편을 alpha에 넣어준다.  
alpha <- res$marginals.fixed[[1]]
alpha %>% head(5)

ggplot(data.frame(inla.smarginal(alpha)), aes(x,y)) + geom_line()+theme_bw() 


# 분위수
# alpha의 분포에서 0.05의 누적확률을 갖도록 하는 alpha의 값을 구한다.
inla.qmarginal(0.05,alpha)

# 누적확률
#반대로, alpha의 값이 -2.781792일 때 가지는 누적 확률 분포는 0.05이다.
inla.pmarginal(-2.781792,alpha)
#위의 값들 그래프로 나타내기 
quant <- inla.qmarginal(0.05,alpha)
data.frame(inla.smarginal(alpha)) %>% 
  ggplot(aes(x,y)) + 
  geom_line() + theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(alpha)),x < quant),fill = "black")

#res$summary.fixed에서 fixed effect에 속하는 절편 알파의 
#각 분위수에 해당하는 값인 것을 확인할 수 있다.
inla.qmarginal(0.025,alpha)
inla.qmarginal(0.5,alpha)
inla.qmarginal(0.975,alpha)

#### 3) Alpha가 -2.5일때 density는? 

#inla.smarginal(alpha) 이거는 알파의 밀도함수
inla.dmarginal(-2.5, alpha)

data.frame(inla.smarginal(alpha)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  geom_vline(xintercept = -2.5, linetype = "dashed")


#### 1) u_i의 variance
res$marginals.hyperpar %>% str() #여기에는 precision이라는 값이 담겨 있다.

#그리고, 위의 값을 사용해서 역수를 취하고, Variance를 구한다. 
#Precision = inversion of Vriance
marg.variance <-inla.tmarginal(function(x) 1/x, 
                               res$marginals.hyperpar$`Precision for hospital`)
marg.variance %>% str()

#Precision의 그래프
data.frame(inla.smarginal(res$marginals.hyperpar$`Precision for hospital`)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  labs(title = "Precision for hospital")
#그 역수를 취한 Vriance 그래프
data.frame(inla.smarginal(marg.variance)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw()  + 
  labs(title = "Variance of u_i")


#### 2) u_i의 (variance의) 평균과 표준편차는? 

# mean posterior of the variance, E(X)
#emarginal은 기댓값, 즉 평균 구하는 함수.
m <-inla.emarginal(function(x) x, marg.variance)
m

#그러니까 지금 이 그래프에서 살펴보면, 위에서 구한 variance의 그래프에, 
#emarginal로 구한 기댓값을 표시하였다. 
#이 점 추정치를 사용하면 된다.
data.frame(inla.smarginal(marg.variance)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  geom_vline(xintercept = m, linetype = "dashed")

# standard deviation, Var(X) = E(X^2) - E(X)^2
#variance의 기댓값의 표준편차
mm <-inla.emarginal(function(x) x^2, marg.variance)
sqrt(mm-m^2)

#Variance의 분위수에 해당하는 값들 반환.
# Quantiles 2.5%, 50%, 97.5% 
Q <- inla.qmarginal(c(0.025,0.5,0.975), marg.variance)
Q

library(gridExtra)
q1<- data.frame(inla.smarginal(marg.variance)) %>%
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(marg.variance)),x < Q[1]),fill = "black") 

q2<-data.frame(inla.smarginal(marg.variance)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(marg.variance)),x < Q[2]),fill = "black") 

q3<-data.frame(inla.smarginal(marg.variance)) %>% 
  ggplot(aes(x,y)) + geom_line() + theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(marg.variance)),x < Q[3]),fill = "black") 

grid.arrange(q1,q2,q3, ncol = 1)


#요약 통계량 한번에 구하기.
#summary statistics of marginal
inla.zmarginal(marg.variance)


### 1.2.5 posterior distribution of mortality rates of each hospital

# inla model
prior.prec <- list(prec = list(prior="pc.prec", 
                               param = c(1,0.01)))
formula <- r~ f(hospital, model="iid", hyper =prior.prec)

res1 <- inla(formula,
             data = Surg,
             family = "binomial", Ntrials = n,
             control.predictor = list(compute=TRUE),
             control.compute=list(dic=TRUE,return.marginals.predictor=TRUE))
#return.marginals.predictor=TRUE 이 옵션 추가하면 사후분포 결과까지 얻을 수 있다.
res1$marginals.fitted.values %>% str()

#### 1) 병원별 사후분포 그래프 
#저번 시간에 그린 것은 사후 분포 전체를 활용하지 않고 
#평균값만 사용한 것이다. 
#이번에는 사후 분포를 모두 활용할 예정.

list_marginals <- res1$marginals.fitted.values #사후 분포가 들어 있는 객체 생성
list_marginals

#각 병원 마다 따로 리스트의 형태로 저장된 객체를 
#rbind 해서 데이터프레임으로 합친다.
marginals <- data.frame(do.call(rbind,list_marginals))
marginals

names(list_marginals)
#객체에 병원 ID와 식별자 추가.
marginals$hospital <- rep(names(list_marginals), 
                          times=sapply(list_marginals, nrow))
marginals$ID <- rep(Surg$hospital, 
                    times=sapply(list_marginals, nrow))
marginals

# plot for posterior distribution of mortality rates
#각 병원의 사망률 사후분포 값을 나타낸다.
marginals %>% ggplot(aes(x,y)) + 
  geom_line() + facet_wrap(~hospital) + 
  labs(x ="", y= "Density")  + theme_bw() 

marginals %>% ggplot(aes(x,y)) + 
  geom_line() + facet_wrap(~ID) + 
  labs(x ="", y= "Density")  + theme_bw() + lims(x=c(0,0.5))


### 1.2.5 Exceedance probabilities

#pmarginal은 특정 값이 주어지면, 해당 확률분포 하에서 해당 값이 나타날 누적확률을 반환한다. 
#따라서 
?inla.pmarginal()

# 병원A = fitted.Predictor.01 수술 후 사망률이 0.1보다 클 확률 
marg_A <- res1$marginals.fitted.values[[1]]
#일단 이렇게 하면 marg_A라는 사후확률 분포가 주어졌을 때
#0.1이하일 확률이고, 그걸 1에서 빼니까 0.1보다 클 확률을 구하는 것이다. 
#확률분포 에서 수직으로 선 긋고, 왼쪽 영역에 해당하는 넓이.
1 - inla.pmarginal(q = 0.1, marg_A)

#여기에 그려진 그래프에 개념이 잘 나와 있다.
data.frame(inla.smarginal(marg_A)) %>% 
  ggplot(aes(x,y)) + geom_line() + labs(x ="", y= "Density") + 
  geom_vline(xintercept=0.1, col = "grey")+
  geom_area(data = subset(data.frame(inla.smarginal(marg_A)),x > 0.1),fill = "black")  + 
  theme_bw() + labs(title = "hosptial A")


# 병원별 수술 후 사망률이 0.1보다 클 확률을 각 리스트 구성 요소에 대해서 넣음.
sapply(res1$marginals.fitted.values, function(marg){1-inla.pmarginal(0.1,marg)})


marginals %>% ggplot(aes(x,y)) + geom_line() + 
  facet_wrap(~hospital) + labs(x ="", y= "Density") + 
  geom_vline(xintercept=0.1, col = "grey") + theme_bw() 

marginals %>% ggplot(aes(x,y)) + geom_line() + 
  facet_wrap(~ID) + labs(x ="", y= "Density") + 
  geom_vline(xintercept=0.1, col = "grey") + theme_bw() + lims(x=c(0,0.5)) 



##### 저번 실습내용에서는 결과변수 값을 베이지안 추정으로 좀 더 smooth하게 나타낸 것이고,
# 이번 내용은 본격적으로 설명변수를 포함한 공간분석 모델을 그려본다.
# 패키지 설치 
install.packages(c("SpatialEpi","cowplot"))

# 패키지 불러오기
library(SpatialEpi); library(cowplot);
library(INLA); library(spdep); library(sf); library(sp); library(dplyr); library(ggplot2)

# 예시: 스코틀랜드 구순암 자료 (scotland) 

## 1. scotland 데이터 살펴보기 

?scotland

scotland$data

str(scotland$data) # 56개 카운티 
# cases : 카운티별 남성 구순암 환자 발생 수 
# expected : 카운티별 남성 구순암 환자 발생 기대값
# AFF : 농업,수산업,산림업에 종사하는 인구비율 

# spdataframe인 객체에서 데이터프레임만 빼와서 변수명 변경 
d <- scotland$data[, c("county.names","cases","expected","AFF")]
names(d) <- c("county","Y","E","AFF")
d

# SIR 계산. 이 경우에는 자료에 이미 기댓값과 관찰값이 들어가 있어서 계산이 가능했다.
d$SIR <- d$Y/d$E
d

summary(d)


## 2. scotland 지도 그리기 

#폴리곤의 모양을 잘 나타낼 수 있다.
map <- scotland$spatial.polygon
plot(map)


### 2.1 tmap and leaflet

# 세계지도에서 scotland 위치 확인 
#지도에서 폴리곤은 나타나지만, 좌표계가 설정되어 있지 않다.
library(tmap)
tmap_mode("view") + qtm(map)


### 2.2 Specify CRS 

#https://epsg.io/ 여기 사이트 들어가서 국가 이름 검색하기.
#https://spatialreference.org/ref/epsg/27700/proj4/ 

map #이거 쳐보면, 위도 경도 좌표계에 0만 들어가 있는 것을 알 수 있다.
st_crs(scotland$spatial.polygon) 

#https://epsg.io/ PROJ.4 항목, 사이트에서 긁어온 EPSG 27700의 코드값을 지정해준다
proj4string(map) <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.06,0.15,0.247,0.842,-20.489 +units=m +no_defs" 
st_crs(map)

library(rgdal)
codes <- rgdal::make_EPSG()
codes[which(codes$code == "27700"),]


qtm(map)


## leaflet 패키지 사용시에는 EPSG가 아니라 WGS 좌표계를 사용하는 것이 더 좋다.
map_w <- spTransform(map,CRS("+proj=longlat +datum=WGS84 +no_defs"))
st_crs(map_w)

#d는 위에서 spdataframe에서 dataframe만 빼와서 넣은 객체고, SIR 값도 들어 있다.
#d라는 attribute를 가진 polygon을 만들기 위해서 map 객체와 합친다.
rownames(d) <- names(map)
map2<- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)

library(tmap)
tmap_mode("plot")
tm_shape(map2) + tm_polygons(col = "SIR")

#순서 정렬하기
map2@data %>% arrange(desc(SIR))


### \leaflet 패키지로 동일한 작업 수행.
library(leaflet)
map_w2<- SpatialPolygonsDataFrame(map_w, d, match.ID = TRUE) #WGS 좌표계로 설정했던 거

l <- leaflet(map_w2) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = map2$SIR)

#여기 단계에선 일단 지도상에 폴리곤만 나타내준다. 
l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )
#바로 위에서 leaflet으로 폴리곤만 나타낸다고 했던 단계를 tmap으로 보여줌. 동일함.
library(tmap)
tm_shape(map2) + 
  tm_polygons(col = "SIR", palette = "YlOrRd", alpha = 0.5, border.col = "grey")

#이거는 leaflet으로 그린 지도 위에 마우스를 얹으면 라벨이 나타나도록 하는 것이다.
labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s",
                  map_w2$county, map_w2$Y, round(map_w2$E, 2),
                  map_w2$AFF, round(map_w2$SIR, 2)) %>% 
  lapply(htmltools::HTML) #html 형식으로 라벨 입력

l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
      ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )


## 1. INLA 공간분석모형. 공간상관을 고려한 SIR 추정하기  

### 1.1 이웃하는 카운티 목록 -> 매트릭스로 변경
nb <- poly2nb(map_w2)
nb
head(nb)
#INLA 패키지가 이해할 수 있게 넣어주기.
nb2INLA("map.adj",nb)
g <- inla.read.graph(filename ="map.adj")

### 1.2 Random effect : u_i, v_i
map_w2$idareau <- 1:nrow(map_w2@data) #u_i
map_w2$idareav <- 1:nrow(map_w2@data) #v_i

### 1.3 Model 결정 
#fixed effect로 AFF 변수를 넣고 random effects 추가.
formula <- Y ~ AFF + 
  f(idareau, model = "besag", graph = g, scale.model = TRUE) + 
  f(idareav, model = "iid")

### 1.4 최종 분석식 
res <- inla(formula,
            family = "poisson", #푸아송 분포.
            data = map_w2@data,
            E = E, #기댓값 열 지정.
            control.predictor = list(compute = TRUE),
            control.compute = list(return.marginals.predictor = TRUE)
)
summary(res) 



## 2. 결과 확인 

### 2.1 Fixed effect 
#fixed effect, 그러니까 회귀계수가 부여되는 절편과 AFF만 보여준다.
res$summary.fixed # log(theta_i) = -0.305 + 4.330 AFF + u_i + v_i


### 2.2 AFF의 posterior distribution(Posterior distribution of beta_1)
#AFF의 사후분포그래프를 시각화하기
# 간단한 버전 
marginal<- inla.smarginal(res$marginals.fixed$AFF)
plot(marginal, type = "l" )

# ggplot2 
marginal<- inla.smarginal(res$marginals.fixed$AFF) #AFF의 사후분포.
marginal <- data.frame(marginal)

marginal %>% ggplot(aes(x,y)) + geom_line() +
  labs(x= expression(beta[1]), y = "Density") + theme_bw()

# AFF의 95% CI
marginal<- inla.smarginal(res$marginals.fixed$AFF)
inla.qmarginal(0.025,marginal) #여기서는 각 분위수에 속하는 AFF 값을 따로따로 출력해봄.
inla.qmarginal(0.975,marginal)

# AFF 요약통계량으로 한번에 보기.
marginal<- inla.smarginal(res$marginals.fixed$AFF)
inla.zmarginal(marginal)



## 3. 추정값 지도화
### 3.1 ggplot2
#이건 저번에 실습때 한거랑 같다.
res$summary.fitted.values

map_w2$RR <- res$summary.fitted.values[, "mean"] 
map_w2$LL <- res$summary.fitted.values[, "0.025quant"] 
map_w2$UL <- res$summary.fitted.values[, "0.975quant"] 

map_w2@data

# SIR과 RR, 95%CI와 함께 확인 
map2sf <- st_as_sf(map_w2)

gSIR <- ggplot(map2sf) + geom_sf(aes(fill = SIR)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red") + 
  theme_bw() 

gRR <- ggplot(map2sf) + geom_sf(aes(fill = RR)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red") + 
  theme_bw() 

gLL <- ggplot(map2sf) + geom_sf(aes(fill = LL)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red") + 
  theme_bw()

gUL <- ggplot(map2sf) + geom_sf(aes(fill = UL)) + 
  scale_fill_gradient2(midpoint = 1, low = "blue", mid = "white", high = "red") + 
  theme_bw() 

plot_grid(gSIR, gRR, ncol=2) #raw data로 그린 SIR과 추정값으로 그린 RR.
plot_grid(gRR,gLL,gUL, ncol=3) #RR과 95% CI들



### 3.2 추정값 지도화(leaflet)

map <- map_w2
pal <- colorNumeric(palette = "YlOrRd", domain = map$RR)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s)",
                  map$county, map$Y, round(map$E, 2),
                  map$AFF, round(map$SIR, 2), round(map$RR, 2),
                  round(map$LL, 2), round(map$UL, 2)) %>% 
  lapply(htmltools::HTML)

lRR <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(RR),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~RR, opacity = 0.5, title = "RR",
    position = "bottomright"
  )
lRR



## 4. Exceedance probabilities 

res <- inla(formula,
            family = "poisson", 
            data = map_w2@data,
            E = E, 
            control.predictor = list(compute = TRUE),
            control.compute = list(return.marginals.predictor = TRUE)
            # return.marginals.predictor = TRUE 옵션 추가 
)

res$marginals.fitted.values %>% str()

#카운티 번호와 이름 확인  
map_w2@data %>% select(ID, county) %>% arrange(ID)


### 4.1 County 1의 사후분포 그래프 
#county 1의 RR 값이 가지는 사후확률 분포이다. 
plot(res$marginals.fitted.values[[1]], type ="l", main = "County 1")

#1번 카운티의 RR > 2 일 확률.
#vline을 기준으로 오른쪽에 해당하는 영역을 지칭하게 된다. 
data.frame(res$marginals.fitted.values[[1]]) %>% ggplot(aes(x,y)) + 
  geom_line() +
  labs(x= expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 2, col = "black") + theme_bw() + lims(x =c(0,20))
#이거는 바로 위 그래프에다가 영역에 색칠까지 해서 나타낸거.
data.frame(inla.smarginal(res$marginals.fitted.values[[1]])) %>% 
  ggplot(aes(x,y)) +   geom_line() + geom_vline(xintercept = 2, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(res$marginals.fitted.values[[1]])),x>2),fill = "black") + lims(x=c(0,20)) + labs(title = "P(RR>2)")


#P(RR > 2). 분위수를 2라고 했을 때 누적확률값.
1- inla.pmarginal(q = 2, marginal=res$marginals.fitted.values[[1]])


### 4.2 County 1~4의 사후분포 그래프 

par(mfrow = c(2,2)) #사후분포 그래프만 그리기.
plot(res$marginals.fitted.values[[1]], type ="l", main = "County 1")
plot(res$marginals.fitted.values[[2]], type ="l", main = "County 2")
plot(res$marginals.fitted.values[[3]], type ="l", main = "County 3")
plot(res$marginals.fitted.values[[4]], type ="l", main = "County 4")

#이건 위와 동일한데, 영역표시까지 해준거.
c1<-data.frame(inla.smarginal(res$marginals.fitted.values[[1]])) %>% 
  ggplot(aes(x,y)) +   geom_line() + geom_vline(xintercept = 2, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(res$marginals.fitted.values[[1]])),x>2),fill = "black") + lims(x=c(0,20), y=c(0,1)) + labs(title="County 1")

c2<-data.frame(inla.smarginal(res$marginals.fitted.values[[2]])) %>% 
  ggplot(aes(x,y)) +   geom_line() + geom_vline(xintercept = 2, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(res$marginals.fitted.values[[2]])),x>2), fill = "black") + lims(x=c(0,20), y=c(0,1)) + labs(title="County 2")

c3<-data.frame(inla.smarginal(res$marginals.fitted.values[[3]])) %>% 
  ggplot(aes(x,y)) +   geom_line() + geom_vline(xintercept = 2, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(res$marginals.fitted.values[[3]])),x>2),fill = "black") + lims(x=c(0,20), y=c(0,1)) + labs(title="County 3")

c4<-data.frame(inla.smarginal(res$marginals.fitted.values[[4]])) %>% 
  ggplot(aes(x,y)) +   geom_line() + geom_vline(xintercept = 2, col = "black") + 
  theme_bw() + 
  geom_area(data = subset(data.frame(inla.smarginal(res$marginals.fitted.values[[4]])),x>2),fill = "black") + lims(x=c(0,20), y=c(0,1))  + labs(title="County 4")

plot_grid(c1,c2,c3,c4)

#각 county의 확률들을 각각 계산.
1- inla.pmarginal(q = 2,marginal=res$marginals.fitted.values[[1]])
1- inla.pmarginal(q = 2,marginal=res$marginals.fitted.values[[2]])
1- inla.pmarginal(q = 2,marginal=res$marginals.fitted.values[[3]])
1- inla.pmarginal(q = 2,marginal=res$marginals.fitted.values[[4]])


# 모든 카운티에 대해서 exceedance probabilies 계산 
#?sapply
exc<- sapply(res$marginals.fitted.values, 
             FUN = function(marg){1-inla.pmarginal(q=2, marginal = marg)})
exc %>% head(4)

map$exc <- exc #map에다가 넣어서 exceedence 확률을 지도화 할 수 있도록 한다.


### 4.3 최종 지도화 
#leaflet 패키지 사용해서 방금 구한거 지도화하기. 

pal <- colorNumeric(palette = "YlOrRd", domain = map$exc)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s) <br/> P(RR>2): %s",
                  map$county, map$Y, round(map$E, 2),
                  map$AFF, round(map$SIR, 2), round(map$RR, 2),
                  round(map$LL, 2), round(map$UL, 2), round(map$exc, 2)) %>% 
  lapply(htmltools::HTML)

lexc <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(exc),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~exc, opacity = 0.5, title = "P(RR>2)",
    position = "bottomright"
  )





################### 12장
library(INLA); library(spdep); library(maptools); library(CARBayes); library(tidyverse)

x1<-c(1.1,2.3,3.4,4.5,5.4)
x2<-c(-2.3,4.5,3.6,6.8,12.7)
y<-c(1.2,1.4,2.3,3.2,1.2)
As<-data.frame(x1,x2,y)
formula1<-y~1+x1
res1<-inla(formula1,family="gaussian",data=As,control.compute=list(dic=TRUE,cpo=TRUE))                                            
summary(res1)

SClist<-list(Y1998=c( 18,90,10,120,12,14,76,96,10,256,37,23,40,29,36,
                      55,21,63,15,19,129,47,260,60,10,184,22,45,43,44,10,171,11,
                      34,22,34,51,63,90,201,10,202,87,25,25,91 ),
             Exp98=c(19.334642001146, 105.221991510865, 8.9954123633133, 126.211287025262, 
                     12.9499400671852, 17.0850039703209, 85.5262771111914, 107.178846922884, 
                     11.0291918950188, 248.419380066852, 38.5954996425929, 27.0027208298727, 
                     32.2453350684913, 24.1871410613557, 29.3284980403873, 52.0933278275436, 
                     23.3496100847714, 69.1791167378613, 15.7011547559647, 17.5779462883105, 
                     98.0421453601469, 42.1724712080047, 277.747093167242, 49.9402374163248, 
                     15.0708479385354, 137.177683720537, 13.3400552455942, 38.1425892644401, 
                     46.222761591486, 49.646669857522, 16.011990994697, 161.116783742905, 
                     7.49225226944375, 27.1667732892036, 23.2255895652772, 27.0506021696774, 
                     50.282471254929, 68.9687528187193, 84.0568694371842, 241.020535657027, 
                     13.3636034454982, 194.239681727817, 84.0882670370562, 23.9367452023769, 
                     29.1377576211652, 121.126445726))

SCresp<-data.frame(SClist)
SCresp <- SCresp %>% 
  mutate(RR = Y1998/Exp98)

library(spdep)
library(maptools)
library(CARBayes)
library(sf)
library(sp)
remotes::install_github("carrollrm/fillmap")
library(fillmap)

setwd("C:/Users/TaeheeCH/Desktop/week 12")
SCpoly <-st_read("SC_county_alphasort.shp")
SCmap<-as_Spatial(SCpoly)
W.nb <- poly2nb(SCmap)
W.mat <- nb2mat(W.nb, style="B") #matrix


### 10.1 Uncorrelated Heterogeneity model
#UH models
#미상관 가우시안 임의효과 모형을 단순회귀로 적합함
region<-seq(1:46)
#결과 변수를 각 region의 임의효과로만 추정한다.
#여기선 아마 vi를 말하는 것 같음. 
#그리고 각 vi는 정규분포를 가정한다는 것이다. 
formulaUH<- Y1998~f(region, model = "iid", param=c(2,0.5)) #여기서 model에 정규분포 가정했으므로 가우시안인 것을 알 수 있다. 
resUH <- inla(formulaUH, family ="poisson", data = SCresp , 
              control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE),E=Exp98)

#resUH$summary.random$region
summary(resUH)
resUH$summary.random #여기선 random 효과를 통해서 결과변수를 예측한 것이니까 summary.random을 보는게 맞지
head(resUH$summary.random$region) #이유는 정확히 모르지만 $region이 붙어야 head가 작동함.
resUH$summary.random$region["sd"]
resUH$summary.random$region["mean"]


#UH model 지도 그리기.
SCpoly <- readSplus("SC_geobugsSPlus.txt")   # SC counties 46# spatialpolygons 객체
plot(SCpoly)
class(SCpoly)
SCpoly <- st_as_sf(SCpoly) #sp를 sf 객체로 변환
class(SCpoly) 
plot(SCpoly)

#posterior mean map of UH effect를 지도화한다.
#여기서는 그러면,  mean이라는 것은 각 region 별로 구해지는 vi 값이네.
UHeffect<-resUH$summary.random$region["mean"]
UHeffect

fillmap(SCpoly,"",UHeffect$mean,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
fillmap(SCpoly,"",SCresp$Y1998,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
fillmap(SCpoly,"",SCresp$RR,n.col=5,leg.loc="bottomleft",leg.cex=0.8)

cpo<-resUH$cpo$cpo
locdic<-resUH$dic$local.dic
LMPL <- sum(log(cpo))
LMPL

#CPO는 0~1 사이의 값을 가지고, 1에 가까우면 좋은 적합도.
#DIC는 작을수록 좋은 적합도.
#그래서 둘은 서로 inverse하다.
par(mfrow=c(1,2))
fillmap(SCpoly,"",cpo,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
fillmap(SCpoly,"",locdic,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
summary(resUH)


### 10.2 Correlated Heterogeneity model
#
### (1)  ICAR model Intrinsic Conditional Autoregressive 모형.
#위에서 봤던 UH model에서의 vi 값을 인접지역 정보 사용해서 수정한 것.
m=46
region<-seq(1:m)
region2<-region
idx<-region

#모형 적합.
#ICAR니까 besag로.
#graph는 poly2nb로 그렸던 것과 같은거.
formICAR <- Y1998~ 1 + f(region,model="besag", param=c(2,0.5),graph="SCgraph.txt") 
resICAR <- inla(formICAR,family="poisson",data=SCresp,
                control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE),E=Exp98)

cpo<-resICAR$cpo$cpo
dic<-resICAR$dic$dic
pD<-resICAR$dic$p.eff
CHeffect<-resICAR$summary.random$region["mean"]
LPML<-sum(log(resICAR$cpo$cpo))

summary(resICAR)

dic
pD #Effective number of parameters 
LPML

par(mfrow=c(1,2))
#Posterior mean ICAR effect 
fillmap(SCpoly,"",CHeffect$mean,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
#cpo map for the ICAR model
fillmap(SCpoly,"",cpo,n.col=5,leg.loc="bottomleft",leg.cex=0.8)



### (2) the convolution model 
#UH에서 썼던 가우시안 절편과, ICAR항을 모두 사용한다.
## 계산이 더 복잡하지만 단일 공간 효과에 대한 파악이 좀 더 유용하다.
region<-seq(1:m)
region2<-region
formCONV = Y1998~1+f(region, model = "iid",param=c(2,0.5))+f(region2, model = "besag",param=c(2,0.5),graph="SCgraph.txt")
resCONV = inla(formCONV,family="poisson",data=SCresp,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE),E=Exp98)

summary(resCONV)

cpo_conv<-resCONV$cpo$cpo
resCONV$dic$dic #DIC
resCONV$dic$p.eff #pD
sum(log(resCONV$cpo$cpo)) #LPMD
CONVeffct<-resCONV$summary.random$region2["mean"]

#Posterior mean map of the ICAR component 
fillmap(SCpoly,"",CONVeffct$mean,n.col=5,leg.loc="bottomleft",leg.cex=0.8)
# the cpo map for the convolution model
fillmap(SCpoly,"",cpo_conv,n.col=5,leg.loc="bottomleft",leg.cex=0.8)



#### (3) Leroux model 
library(INLABMA) #INLA아니고 여기서 돌려야한다.
library(spdep)
library(maptools)
W.nb <- poly2nb(SCpoly)
W.mat <- nb2mat(W.nb, style="B")
rlambda <- seq(0.03, 0.8, length.out = 20)
errorhyper <- list(prec = list(prior = "loggamma",param = c(1, 0.01), #hyperparameter
                               initial = log(1), fixed = FALSE))

form2<-Y1998~1+offset(log(Exp98)) #절편 같은 다른 요소는 없이 임의 효과만 offset으로 넣고 모델링한다.
lerouxmodels <- mclapply(rlambda, function(lambda) {  
  leroux.inla(form2, d =SCresp, W = W.mat,
              lambda = lambda, improve = TRUE,
              family = "poisson",
              control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE, cpo = TRUE))})
resLER<- INLABMA(lerouxmodels, rlambda, 0, impacts = FALSE) #오래걸림 

cpo<-resLER$cpo
resLER$dic$dic
resLER$dic$p.eff
LPML<-sum(log(cpo$cpo))
LPML


### (4) BYM2 model 
#convolution이랑 비슷한 메커니즘이고, 이를 보완하기 위해서 개발.
region2<-region
formBYM2 = Y1998~1+f(region2, model = "bym2",scale.model=TRUE,param=c(2,0.5),
                     graph="SCgraph.txt")
resBYM2 = inla(formBYM2,family="poisson",data=SCresp,
               control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE),E=Exp98)
summary(resBYM2)

cpo<-resBYM2$cpo
LMPL<-sum(log(cpo$cpo))
LMPL
resBYM2$dic$dic
resBYM2$dic$p.eff

rand<-resBYM2$summary.random$region2["mean"]
UH<-rand$mean[1:46]
CH<-rand$mean[47:92]


#graphics....using data.frame and SCmap. 지금까지 얻은 데이터 지도로 그려보기.

library(tmap)
library(sf)
library(sp)
library(spdep)

SCpoly<-st_read("SC_county_alphasort.shp")
SCmap<-as_Spatial(SCpoly)
plot(SCmap)
LERM <- cbind(UH,CH)
SCcon<-data.frame(LERM)
#이렇게 해서 polygon 객체 SCmap이랑, 데이터프레임 LERM 생성됨.

areaID<-as.character(seq(1:46))
attr<-data.frame(SCcon,row.names=areaID)
spg<-SpatialPolygonsDataFrame(SCmap, attr, match.ID = TRUE) #폴리곤이랑 데이터프레임 합쳐주고.

qtm(spg,fill=c("UH","CH"),fill.palette="Blues")+
  tm_layout(title="",panel.labels=labels,legend.position=c("LEFT","BOTTOM"),
            legend.height=0.5)


#################################################
#CHAPTER 13. Saptio temporal modeling with INLA #
#################################################

asd<-list(Y11=c( 31,129,10,183,13,20,148,148,13,290,58,37,60,42,53,70,38,86,23,13,126,56,346,53,20,302,11,60,75,70,21,187,17,35,44,30,91,81,105,232,15,267,93,42,33,174 ),
          Y12=c( 26,129,10,200,19,15,152,160,20,287,63,40,37,39,56,75,44,95,18,22,133,61,343,61,22,307,12,75,63,81,12,221,17,38,30,43,89,81,121,265,14,289,86,39,34,166 ),
          Y13=c( 30,128,13,157,14,13,149,167,20,288,67,33,47,60,50,73,34,109,16,30,134,60,376,63,26,327,25,58,78,73,17,198,12,40,34,28,64,75,110,283,16,233,109,37,38,185 ),
          Y14=c( 27,144,10,170,25,23,162,152,17,286,62,44,62,37,33,78,29,113,24,24,122,60,346,73,25,324,21,57,89,81,20,218,12,35,27,40,92,83,112,235,22,309,128,41,37,188 ), 
          Y15=c( 27,133,10,193,13,28,150,199,17,305,46,38,53,31,44,83,40,100,21,30,142,72,350,78,14,372,23,72,87,74,20,235,15,28,38,25,92,66,101,269,21,274,108,43,37,191 ),
          Y16=c( 33,134,10,174,13,20,123,152,23,287,57,34,44,34,59,55,32,126,27,23,120,75,324,53,11,352,17,83,100,88,24,250,23,34,26,33,80,80,93,235,22,254,82,28,26,209 ) ,
          Exp11=c( 22.10992,141.1974,8.94643,165.6315,14.03784,19.64682,144.7141,161.2704,13.30849,314.3281,48.8051,28.92454,40.9114,30.51505,33.92895,60.01692,27.90696,123.8071,23.43594,20.71273,121.1446,52.71637,405.3609,61.36666,18.2962,242.8304,22.1398,54.72165,68.46071,58.46067,16.67493,234.7364,8.818134,28.86302,25.05194,33.14687,65.39391,80.7648,105.0742,341.931,17.47107,252.0818,94.42917,25.20132,29.9509,202.5737 ),
          Exp12=c( 22.0572,143.0691,8.776834,166.3934,13.85155,19.51853,147.671,166.7677,13.10198,320.8817,48.91231,28.5994,40.51245,30.1908,33.52649,59.87632,27.63279,125.2166,23.15211,20.52995,121.2201,52.89035,410.9022,61.29724,18.21272,248.0545,22.70044,54.78316,69.4985,58.19266,16.39198,237.616,8.73729,28.52119,24.73208,33.01945,65.57757,80.38342,105.1586,346.0733,17.48073,253.7312,94.94939,24.8261,29.54317,206.1827 ),
          Exp13=c( 21.9746,144.2677,8.645902,167.5235,13.55893,19.4368,151.0006,170.4927,13.2294,327.5961,49.10826,28.62752,40.59505,30.18904,33.20575,59.69706,27.4421,127.7659,23.23031,20.30675,121.5523,53.11092,416.7555,61.26824,17.93328,254.5264,23.39991,54.93518,70.70149,58.19793,16.1222,240.5563,8.740806,28.18288,24.6073,32.97112,65.94489,79.91418,105.2983,350.8414,17.65472,255.6855,95.01177,24.63102,29.05723,210.3373 ),
          Exp14=c( 21.93769,144.7747,8.519363,169.4294,13.341,19.29621,154.5278,174.1702,13.07386,334.8123,49.23041,28.41575,40.53178,29.97639,33.19081,59.57755,27.35247,130.4653,23.33313,20.18988,122.3476,53.40354,424.2125,61.08986,17.93065,262.595,23.87531,55.50196,73.07584,58.46507,16.11869,244.1907,8.652053,28.06074,24.53788,33.20135,66.07406,79.1655,105.7719,352.8712,17.5976,257.9465,94.83251,24.4957,28.73034,215.5948 ),
          Exp15=c( 21.90869,145.7202,8.289134,171.0832,13.07562,19.09058,157.8117,178.1957,12.98862,342.0593,49.3798,28.35423,40.43688,29.67937,33.15566,59.35699,27.4465,133.9882,23.29886,19.98865,122.0567,53.86488,432.2187,61.3693,17.61782,271.7049,24.45,55.89037,75.43262,58.54416,15.72589,247.6573,8.52903,27.89729,24.16002,33.40258,66.53188,78.39045,106.9345,357.6911,17.62133,261.2505,94.44675,24.4087,28.58974,220.7346 ),
          Exp16=c( 21.85597,147.1517,7.948184,172.7326,12.6837,18.87793,160.94,185.3241,13.00181,348.4055,49.77699,28.27866,40.43336,29.83403,33.32438,59.08106,27.11609,135.1262,23.16177,19.90605,121.9179,53.95363,438.2846,61.62852,17.50622,283.2541,25.01327,56.32446,78.72964,58.67948,15.49654,251.4913,8.473669,27.87884,23.67759,33.46146,67.09603,77.2437,107.9644,359.8862,17.74787,264.907,94.37293,24.31731,28.08007,227.1766 ) )
attach(asd)


count<-matrix(nrow=46,ncol=6)
expe<-matrix(nrow=46,ncol=6)
sir<-matrix(nrow=46,ncol=6)

for (j in 1:46){
  count[j,1]<-Y11[j]
  count[j,2]<-Y12[j]
  count[j,3]<-Y13[j]
  count[j,4]<-Y14[j]
  count[j,5]<-Y15[j]
  count[j,6]<-Y16[j]
  expe[j,1]<-Exp11[j]
  expe[j,2]<-Exp12[j]
  expe[j,3]<-Exp13[j]
  expe[j,4]<-Exp14[j]
  expe[j,5]<-Exp15[j]
  expe[j,6]<-Exp16[j]
}

t=c(1,2,3,4,5,6)
num = c(5, 5, 4, 5, 5, 4, 3, 6, 5, 4, 
        3, 4, 5, 6, 7, 5, 4, 4, 4, 6, 
        8, 5, 5, 6, 5, 3, 2, 7, 5, 7, 
        5, 6, 3, 5, 4, 7, 2, 9, 3, 6, 
        5, 4, 6, 7, 5, 4
)

adj = c(
  33, 30, 24, 23, 4, 
  41, 38, 32, 19, 6, 
  25, 15, 6, 5, 
  39, 37, 30, 23, 1, 
  38, 25, 15, 6, 3, 
  38, 5, 3, 2, 
  27, 25, 15, 
  45, 38, 22, 18, 14, 10, 
  43, 40, 38, 32, 14, 
  22, 18, 15, 8, 
  46, 44, 42, 
  46, 44, 29, 20, 
  35, 31, 29, 28, 16, 
  45, 43, 38, 21, 9, 8, 
  38, 25, 18, 10, 7, 5, 3, 
  35, 31, 28, 21, 13, 
  35, 34, 26, 21, 
  38, 15, 10, 8, 
  41, 33, 24, 2, 
  44, 40, 36, 29, 28, 12, 
  45, 43, 35, 34, 31, 17, 16, 14, 
  45, 34, 26, 10, 8, 
  42, 39, 30, 4, 1, 
  41, 36, 33, 30, 19, 1, 
  27, 15, 7, 5, 3, 
  34, 22, 17, 
  25, 7, 
  43, 40, 31, 29, 20, 16, 13, 
  46, 28, 20, 13, 12, 
  44, 42, 36, 24, 23, 4, 1, 
  43, 28, 21, 16, 13, 
  41, 40, 38, 36, 9, 2, 
  24, 19, 1, 
  45, 26, 22, 21, 17, 
  21, 17, 16, 13, 
  44, 41, 40, 32, 30, 24, 20, 
  39, 4, 
  32, 18, 15, 14, 9, 8, 6, 5, 2, 
  37, 23, 4, 
  43, 36, 32, 28, 20, 9, 
  36, 32, 24, 19, 2, 
  44, 30, 23, 11, 
  40, 31, 28, 21, 14, 9, 
  46, 42, 36, 30, 20, 12, 11, 
  34, 22, 21, 14, 8, 
  44, 29, 12, 11
)


library(maptools)
library(sp)
library(spdep)
library(INLA)
m=46
T=6

##### long form ###########################################
countL<-rep(0,276)
expL<-rep(0,276)
for (i in 1:m){
  for (j in 1:T){
    k<-j+T*(i-1)
    countL[k]<-count[i,j]
    expL[k]<-expe[i,j]
  }}

######  models 

t=c(1,2,3,4,5,6)
year<-rep(1:6,len=276)
region<-rep(1:46,each=6)
region2<-region
ind2<-rep(1:276)
STdata<-data.frame(countL,expL,year,t,region,region2,ind2)

library(maptools)
library(fillmap)


# 13.1 uncorrelated spatial heterogeneity (UH effects)
#위에서 본거. only spatial UH model
formula1<-countL~1+f(region,model="iid",param=c(2,0.5))
result1<-inla(formula1,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))
result1$dic$dic
result1$waic$waic
result1$waic$p.eff

# 13.2 spatial convolution model only (UH and CH effects)
# 이것도 아직 spatial effect만 고려함.
formula2<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))
result2<-inla(formula2,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))
result2$dic$dic
result2$waic$waic
result2$waic$p.eff

# 13.3 spatial convolution + time trend model
#13.2의 모델에다가, year를 넣어서 time trend 포함.
formula3<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  year
result3<-inla(formula3,family="poisson",data=STdata,E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

result3$dic$dic
result3$waic$waic
result3$waic$p.eff


# 13.4 spatial convolution + temporal random walk 

formula4<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  f(year,model="rw1",param=c(1,0.01)) #단순한 time trend가 아니고, time random walk 지정하기.
result4<-inla(formula4,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

result4$dic$dic
result4$waic$waic
result4$waic$p.eff

# 13.5 spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula5<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  f(year,model="rw1",param=c(2,0.5))+
  f(ind2,model="iid",param=c(2,0.5))
result5<-inla(formula5,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

result5$dic$dic
result5$waic$waic
result5$waic$p.eff

install.packages("Epi")
library(Epi)
yearM<-cbind(yearR,yearRLL,yearRUL)
matshade(t,yearM,xlab="year",ylab="temporal effect")

#####m   mapping effects  
class(SCpoly)
SCpoly=st_as_sf(SCpoly)

UH<-result5$summary.random$region[,2]
CH<-result5$summary.random$region2[,2]

fillmap(SCpoly,"",UH,n.col=5,leg.loc="bottomleft",leg.cex=0.8)#;x11()



########## 12장 과제

library(INLA); library(spdep); library(maptools); library(CARBayes); library(tidyverse)


asd<-list(Y11=c( 31,129,10,183,13,20,148,148,13,290,58,37,60,42,53,70,38,86,23,13,126,56,346,53,20,302,11,60,75,70,21,187,17,35,44,30,91,81,105,232,15,267,93,42,33,174 ),
          Y12=c( 26,129,10,200,19,15,152,160,20,287,63,40,37,39,56,75,44,95,18,22,133,61,343,61,22,307,12,75,63,81,12,221,17,38,30,43,89,81,121,265,14,289,86,39,34,166 ),
          Y13=c( 30,128,13,157,14,13,149,167,20,288,67,33,47,60,50,73,34,109,16,30,134,60,376,63,26,327,25,58,78,73,17,198,12,40,34,28,64,75,110,283,16,233,109,37,38,185 ),
          Y14=c( 27,144,10,170,25,23,162,152,17,286,62,44,62,37,33,78,29,113,24,24,122,60,346,73,25,324,21,57,89,81,20,218,12,35,27,40,92,83,112,235,22,309,128,41,37,188 ), 
          Y15=c( 27,133,10,193,13,28,150,199,17,305,46,38,53,31,44,83,40,100,21,30,142,72,350,78,14,372,23,72,87,74,20,235,15,28,38,25,92,66,101,269,21,274,108,43,37,191 ),
          Y16=c( 33,134,10,174,13,20,123,152,23,287,57,34,44,34,59,55,32,126,27,23,120,75,324,53,11,352,17,83,100,88,24,250,23,34,26,33,80,80,93,235,22,254,82,28,26,209 ) ,
          Exp11=c( 22.10992,141.1974,8.94643,165.6315,14.03784,19.64682,144.7141,161.2704,13.30849,314.3281,48.8051,28.92454,40.9114,30.51505,33.92895,60.01692,27.90696,123.8071,23.43594,20.71273,121.1446,52.71637,405.3609,61.36666,18.2962,242.8304,22.1398,54.72165,68.46071,58.46067,16.67493,234.7364,8.818134,28.86302,25.05194,33.14687,65.39391,80.7648,105.0742,341.931,17.47107,252.0818,94.42917,25.20132,29.9509,202.5737 ),
          Exp12=c( 22.0572,143.0691,8.776834,166.3934,13.85155,19.51853,147.671,166.7677,13.10198,320.8817,48.91231,28.5994,40.51245,30.1908,33.52649,59.87632,27.63279,125.2166,23.15211,20.52995,121.2201,52.89035,410.9022,61.29724,18.21272,248.0545,22.70044,54.78316,69.4985,58.19266,16.39198,237.616,8.73729,28.52119,24.73208,33.01945,65.57757,80.38342,105.1586,346.0733,17.48073,253.7312,94.94939,24.8261,29.54317,206.1827 ),
          Exp13=c( 21.9746,144.2677,8.645902,167.5235,13.55893,19.4368,151.0006,170.4927,13.2294,327.5961,49.10826,28.62752,40.59505,30.18904,33.20575,59.69706,27.4421,127.7659,23.23031,20.30675,121.5523,53.11092,416.7555,61.26824,17.93328,254.5264,23.39991,54.93518,70.70149,58.19793,16.1222,240.5563,8.740806,28.18288,24.6073,32.97112,65.94489,79.91418,105.2983,350.8414,17.65472,255.6855,95.01177,24.63102,29.05723,210.3373 ),
          Exp14=c( 21.93769,144.7747,8.519363,169.4294,13.341,19.29621,154.5278,174.1702,13.07386,334.8123,49.23041,28.41575,40.53178,29.97639,33.19081,59.57755,27.35247,130.4653,23.33313,20.18988,122.3476,53.40354,424.2125,61.08986,17.93065,262.595,23.87531,55.50196,73.07584,58.46507,16.11869,244.1907,8.652053,28.06074,24.53788,33.20135,66.07406,79.1655,105.7719,352.8712,17.5976,257.9465,94.83251,24.4957,28.73034,215.5948 ),
          Exp15=c( 21.90869,145.7202,8.289134,171.0832,13.07562,19.09058,157.8117,178.1957,12.98862,342.0593,49.3798,28.35423,40.43688,29.67937,33.15566,59.35699,27.4465,133.9882,23.29886,19.98865,122.0567,53.86488,432.2187,61.3693,17.61782,271.7049,24.45,55.89037,75.43262,58.54416,15.72589,247.6573,8.52903,27.89729,24.16002,33.40258,66.53188,78.39045,106.9345,357.6911,17.62133,261.2505,94.44675,24.4087,28.58974,220.7346 ),
          Exp16=c( 21.85597,147.1517,7.948184,172.7326,12.6837,18.87793,160.94,185.3241,13.00181,348.4055,49.77699,28.27866,40.43336,29.83403,33.32438,59.08106,27.11609,135.1262,23.16177,19.90605,121.9179,53.95363,438.2846,61.62852,17.50622,283.2541,25.01327,56.32446,78.72964,58.67948,15.49654,251.4913,8.473669,27.87884,23.67759,33.46146,67.09603,77.2437,107.9644,359.8862,17.74787,264.907,94.37293,24.31731,28.08007,227.1766 ) )
attach(asd)


count<-matrix(nrow=46,ncol=6)
expe<-matrix(nrow=46,ncol=6)
sir<-matrix(nrow=46,ncol=6)

for (j in 1:46){
  count[j,1]<-Y11[j]
  count[j,2]<-Y12[j]
  count[j,3]<-Y13[j]
  count[j,4]<-Y14[j]
  count[j,5]<-Y15[j]
  count[j,6]<-Y16[j]
  expe[j,1]<-Exp11[j]
  expe[j,2]<-Exp12[j]
  expe[j,3]<-Exp13[j]
  expe[j,4]<-Exp14[j]
  expe[j,5]<-Exp15[j]
  expe[j,6]<-Exp16[j]
}

t=c(1,2,3,4,5,6)
num = c(5, 5, 4, 5, 5, 4, 3, 6, 5, 4, 
        3, 4, 5, 6, 7, 5, 4, 4, 4, 6, 
        8, 5, 5, 6, 5, 3, 2, 7, 5, 7, 
        5, 6, 3, 5, 4, 7, 2, 9, 3, 6, 
        5, 4, 6, 7, 5, 4
)

adj = c(
  33, 30, 24, 23, 4, 
  41, 38, 32, 19, 6, 
  25, 15, 6, 5, 
  39, 37, 30, 23, 1, 
  38, 25, 15, 6, 3, 
  38, 5, 3, 2, 
  27, 25, 15, 
  45, 38, 22, 18, 14, 10, 
  43, 40, 38, 32, 14, 
  22, 18, 15, 8, 
  46, 44, 42, 
  46, 44, 29, 20, 
  35, 31, 29, 28, 16, 
  45, 43, 38, 21, 9, 8, 
  38, 25, 18, 10, 7, 5, 3, 
  35, 31, 28, 21, 13, 
  35, 34, 26, 21, 
  38, 15, 10, 8, 
  41, 33, 24, 2, 
  44, 40, 36, 29, 28, 12, 
  45, 43, 35, 34, 31, 17, 16, 14, 
  45, 34, 26, 10, 8, 
  42, 39, 30, 4, 1, 
  41, 36, 33, 30, 19, 1, 
  27, 15, 7, 5, 3, 
  34, 22, 17, 
  25, 7, 
  43, 40, 31, 29, 20, 16, 13, 
  46, 28, 20, 13, 12, 
  44, 42, 36, 24, 23, 4, 1, 
  43, 28, 21, 16, 13, 
  41, 40, 38, 36, 9, 2, 
  24, 19, 1, 
  45, 26, 22, 21, 17, 
  21, 17, 16, 13, 
  44, 41, 40, 32, 30, 24, 20, 
  39, 4, 
  32, 18, 15, 14, 9, 8, 6, 5, 2, 
  37, 23, 4, 
  43, 36, 32, 28, 20, 9, 
  36, 32, 24, 19, 2, 
  44, 30, 23, 11, 
  40, 31, 28, 21, 14, 9, 
  46, 42, 36, 30, 20, 12, 11, 
  34, 22, 21, 14, 8, 
  44, 29, 12, 11
)


library(maptools)
library(sp)
library(spdep)
library(INLA)
m=46
T=6

##### long form ###########################################
countL<-rep(0,276)
expL<-rep(0,276)
for (i in 1:m){
  for (j in 1:T){
    k<-j+T*(i-1)
    countL[k]<-count[i,j]
    expL[k]<-expe[i,j]
  }}

######  models 

t=c(1,2,3,4,5,6)
year<-rep(1:6,len=276)
region<-rep(1:46,each=6)
region2<-region
ind2<-rep(1:276)
STdata<-data.frame(countL,expL,year,t,region,region2,ind2)

library(maptools)
library(fillmap)

library(spdep)
library(maptools)
library(CARBayes)
library(sf)
library(sp)
x1<-c(1.1,2.3,3.4,4.5,5.4)
x2<-c(-2.3,4.5,3.6,6.8,12.7)
y<-c(1.2,1.4,2.3,3.2,1.2)
As<-data.frame(x1,x2,y)
formula1<-y~1+x1
res1<-inla(formula1,family="gaussian",data=As,control.compute=list(dic=TRUE,cpo=TRUE))                                            
summary(res1)

SClist<-list(Y1998=c( 18,90,10,120,12,14,76,96,10,256,37,23,40,29,36,
                      55,21,63,15,19,129,47,260,60,10,184,22,45,43,44,10,171,11,
                      34,22,34,51,63,90,201,10,202,87,25,25,91 ),
             Exp98=c(19.334642001146, 105.221991510865, 8.9954123633133, 126.211287025262, 
                     12.9499400671852, 17.0850039703209, 85.5262771111914, 107.178846922884, 
                     11.0291918950188, 248.419380066852, 38.5954996425929, 27.0027208298727, 
                     32.2453350684913, 24.1871410613557, 29.3284980403873, 52.0933278275436, 
                     23.3496100847714, 69.1791167378613, 15.7011547559647, 17.5779462883105, 
                     98.0421453601469, 42.1724712080047, 277.747093167242, 49.9402374163248, 
                     15.0708479385354, 137.177683720537, 13.3400552455942, 38.1425892644401, 
                     46.222761591486, 49.646669857522, 16.011990994697, 161.116783742905, 
                     7.49225226944375, 27.1667732892036, 23.2255895652772, 27.0506021696774, 
                     50.282471254929, 68.9687528187193, 84.0568694371842, 241.020535657027, 
                     13.3636034454982, 194.239681727817, 84.0882670370562, 23.9367452023769, 
                     29.1377576211652, 121.126445726))

SCresp<-data.frame(SClist)
SCresp <- SCresp %>% 
  mutate(RR = Y1998/Exp98)
setwd("C:/Users/TaeheeCH/Desktop/week 12")
SCpoly <-st_read("SC_county_alphasort.shp")

SCmap<-as_Spatial(SCpoly)
W.nb <- poly2nb(SCmap)
W.mat <- nb2mat(W.nb, style="B") 

# 13.1 uncorrelated spatial heterogeneity (UH effects)
#위에서 본거. only spatial UH model
formula1<-countL~1+f(region,model="iid",param=c(2,0.5))
result1<-inla(formula1,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

summary(result1)
result1$dic$dic
result1$waic$waic
result1$waic$p.eff

# 13.2 spatial convolution model only (UH and CH effects)
# 이것도 아직 spatial effect만 고려함.
formula2<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))
result2<-inla(formula2,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

summary(result2)
result2$dic$dic
result2$waic$waic
result2$waic$p.eff

# 13.3 spatial convolution + time trend model
#13.2의 모델에다가, year를 넣어서 time trend 포함.
formula3<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  year
result3<-inla(formula3,family="poisson",data=STdata,E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

summary(result3)
result3$dic$dic
result3$waic$waic
result3$waic$p.eff


# 13.4 spatial convolution + temporal random walk 

formula4<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  f(year,model="rw1",param=c(1,0.01)) #단순한 time trend가 아니고, time random walk 지정하기.
result4<-inla(formula4,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

summary(result4)
result4$dic$dic
result4$waic$waic
result4$waic$p.eff

# 13.4 spatial convolution + temporal random walk 
#  UH +CH + year RW1 + INT IID 
#그리고 여기에 더해서 시간과 공간의 interaction 고려. 
formula5<-countL~1+f(region,model="iid",param=c(2,0.5))+
  f(region2,model="besag",graph="SCgraph.txt",param=c(2,0.5))+
  f(year,model="rw1",param=c(2,0.5))+
  f(ind2,model="iid",param=c(2,0.5))
result5<-inla(formula5,family="poisson",data=STdata,
              E=expL,control.compute=list(dic=TRUE,cpo=TRUE,waic=TRUE))

summary(result5)
result5$dic$dic
result5$waic$waic
result5$waic$p.eff
