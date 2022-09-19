### dismo 패키지를 활용한 Maxent 모델로 종 다양성 추정하기.

install.packages('doParallel')
options(repos = c(CRAN = "http://cran.rstudio.com"))
library(raster)
library(rgdal)
library(maps)
library(dismo)  
#library(rJava)  
library(maptools)
library(jsonlite)
library(tidyverse)
options(repos = c(CRAN = "http://cran.rstudio.com"))
writeLines('PATH="${RTOOLS40_HOME}/usr/bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
library(rJava)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_341')
install.packages("rJava")
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()


### 먼저 chiroptera

library(readr)
chiroptera <- read_delim("D:/Environmental data/Maxent/Species/chiroptera.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
chiroptera %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
}
  ) -> sp_chiroptera_species

class(sp_chiroptera_species)
sp_chiroptera_species

#list 안에 총 228 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_chiroptera_species <- sp_chiroptera_species[unlist(lapply(sp_chiroptera_species, FUN = length)) > 10]
#point 숫자가 최소한 5보다는 큰 종만 골라서 다시 subset -> 176종

  
chiroptera_species <- as.data.frame(table(chiroptera$species))
chiroptera_species_over10 <- chiroptera_species %>% 
  dplyr::filter(Freq > 10)

write.csv(chiroptera_species, "D:\\Environmental data\\Maxent\\Species_list\\chiroptera_species.csv")
write.csv(chiroptera_species_over10, "D:\\Environmental data\\Maxent\\Species_list\\chiroptera_species_over10.csv")

# load base map
setwd("D:\\Environmental data\\Base_map_numpoints")
terrestrial.grid <- readOGR(dsn = "Base_map_numpoints.shp")

#####loading world climate data #####
world_clm <- list.files(path = "D:\\Environmental data\\Maxent\\wc2.1_5m_bio\\", 
                       pattern = ".tif$", full.names  = TRUE)

st_clm <- stack(world_clm)
cr_st_clm <- crop(st_clm, terrestrial.grid)
plot(cr_st_clm[[1]])

#####loading elevation data #####

elevation <- raster("D:\\Environmental data\\Maxent\\wc2.1_5m_elev\\wc2.1_5m_elev.tif") 
elevation <- crop(elevation, terrestrial.grid)

plot(elevation)


#### making representative raster #### maxent 결과 담을 객체
rep_raster <- mask(raster( crs = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"), ext = extent(terrestrial.grid), resolution = res(cr_st_clm), vals = 1 ),terrestrial.grid)
plot(rep_raster)

list(cr_st_clm, elevation) ->list_env
lapply(list_env, FUN = raster::resample, y = rep_raster, method = "ngb") -> re_list_env





### parameter preparation
{
  prepPara <- function(userfeatures=NULL, #NULL=autofeature, could be any combination of # c("L", "Q", "H", "H", "P")
                       responsecurves=TRUE,
                       jackknife=TRUE,      
                       outputformat="logistic",
                       outputfiletype="asc", 
                       projectionlayers=NULL,
                       randomseed=FALSE,
                       removeDuplicates=TRUE,
                       betamultiplier=NULL,
                       biasfile=NULL,
                       testsamplesfile=NULL,
                       replicates=1,
                       replicatetype="crossvalidate",
                       writeplotdata=TRUE,
                       extrapolate=TRUE,
                       doclamp=TRUE,
                       beta_threshold=NULL,
                       beta_categorical=NULL,
                       beta_lqp=NULL,
                       beta_hinge=NULL,
                       applythresholdrule=NULL,
                       maximumiterations = 500,
                       convergencethreshold = 1.0E-5,
                       number_core = 1 
  ){
    #20 & 29-33 features, default is autofeature
    if(is.null(userfeatures)){
      args_out <- c("autofeature")
    } else {
      args_out <- c("noautofeature")
      if(grepl("L",userfeatures)) args_out <- c(args_out,"linear") else args_out <- c(args_out,"nolinear")
      if(grepl("Q",userfeatures)) args_out <- c(args_out,"quadratic") else args_out <- c(args_out,"noquadratic")
      if(grepl("H",userfeatures)) args_out <- c(args_out,"hinge") else args_out <- c(args_out,"nohinge")
      if(grepl("P",userfeatures)) args_out <- c(args_out,"product") else args_out <- c(args_out,"noproduct")
      if(grepl("T",userfeatures)) args_out <- c(args_out,"threshold") else args_out <- c(args_out,"nothreshold")
    }
    
    #1 
    if(responsecurves) args_out <- c(args_out,"responsecurves") else args_out <- c(args_out,"noresponsecurves")
    #2
    #if(picture) args_out <- c(args_out,"pictures") else args_out <- c(args_out,"nopictures")
    #3
    if(jackknife) args_out <- c(args_out,"jackknife") else args_out <- c(args_out,"nojackknife")
    #4
    args_out <- c(args_out,paste0("outputformat=",outputformat))
    #5
    args_out <- c(args_out,paste0("outputfiletype=",outputfiletype))
    #7
    if(!is.null(projectionlayers))    args_out <- c(args_out,paste0("projectionlayers=",projectionlayers))
    #10
    if(randomseed) args_out <- c(args_out,"randomseed") else args_out <- c(args_out,"norandomseed")
    #16
    if(removeDuplicates) args_out <- c(args_out,"removeduplicates") else args_out <- c(args_out,"noremoveduplicates")
    #20 & 53-56
    # check if negative
    betas <- c( betamultiplier,beta_threshold,beta_categorical,beta_lqp,beta_hinge)
    if(! is.null(betas) ){
      for(i in 1:length(betas)){
        if(betas[i] <0) stop("betamultiplier has to be positive")
      }
    }
    if (  !is.null(betamultiplier)  ){
      args_out <- c(args_out,paste0("betamultiplier=",betamultiplier))
    } else {
      if(!is.null(beta_threshold)) args_out <- c(args_out,paste0("beta_threshold=",beta_threshold))
      if(!is.null(beta_categorical)) args_out <- c(args_out,paste0("beta_categorical=",beta_categorical))
      if(!is.null(beta_lqp)) args_out <- c(args_out,paste0("beta_lqp=",beta_lqp))
      if(!is.null(beta_hinge)) args_out <- c(args_out,paste0("beta_hinge=",beta_hinge))
    }
    #22
    if(!is.null(biasfile))    args_out <- c(args_out,paste0("biasfile=",biasfile))
    #23
    if(!is.null(testsamplesfile))    args_out <- c(args_out,paste0("testsamplesfile=",testsamplesfile))
    #24&25
    replicates <- as.integer(replicates)
    if(replicates>1 ){
      args_out <- c(args_out,
                    paste0("replicates=",replicates),
                    paste0("replicatetype=",replicatetype) )
    }
    #37
    if(writeplotdata) args_out <- c(args_out,"writeplotdata") else args_out <- c(args_out,"nowriteplotdata")
    #39
    if(extrapolate) args_out <- c(args_out,"extrapolate") else args_out <- c(args_out,"noextrapolate")
    #42
    if(doclamp) args_out <- c(args_out,"doclamp") else args_out <- c(args_out,"nodoclamp")
    #60
    if(!is.null(applythresholdrule))    args_out <- c(args_out,paste0("applythresholdrule=",applythresholdrule))
    
    maximumiterations <- as.integer(maximumiterations)
    args_out <- c(args_out, paste0("maximumiterations=", maximumiterations))
    
    convergencethreshold <- as.integer(convergencethreshold)
    args_out <- c(args_out, paste0("convergencethreshold=", convergencethreshold))
    
    #number_core <- as.integer(number_core)
    #args_out <- c(args_out, paste0("threads=", number_core))
    
    return(args_out)
  }
}


stack(unlist(re_list_env)) ->st_env_r
names(st_env_r)

saveRDS(st_env_r, file = "D:\\Environmental data\\Maxent\\stack_env_raster.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\stack_env_raster.rds") -> st_env_r


#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(sp_chiroptera_species)
for(i in 1:length(sp_chiroptera_species)){set.seed(270887*i);fold.list[[i]]<-kfold(sp_chiroptera_species[[i]],k=5)
}


chiropteraTest <- list()
for(i in 1:length(sp_chiroptera_species)){
  chiropteraTest[i] <- sp_chiroptera_species[fold.list[[1]] == 1]
}
chiropteraTrain <- list()
for(i in 1:length(sp_chiroptera_species)){
  chiropteraTrain[i] <- sp_chiroptera_species[fold.list[[1]] != 1]
}



### 설명변수 중에서 correlation 있는거 제거하기.
corst_env_r <- raster::layerStats(st_env_r, 'pearson', na.rm = TRUE) #피어스 상관분석
corst_env_r <- corst_env_r[[1]] #상관분석 매트릭스만 뽑아 ..
corst_env_r <- as.data.frame(corst_env_r)
write.csv(corst_env_r, "D:\\Environmental data\\Maxent\\cor_2.csv")

st_env_r <- dropLayer(st_env_r, c("wc2.1_5m_bio_1", "wc2.1_5m_bio_10", "wc2.1_5m_bio_11", "wc2.1_5m_bio_12",
                                  "wc2.1_5m_bio_13", "wc2.1_5m_bio_14", "wc2.1_5m_bio_15", "wc2.1_5m_bio_17",
                                  "wc2.1_5m_bio_7", "wc2.1_5m_bio_6", "wc2.1_5m_bio_8", "wc2.1_5m_bio_9", 
                                  "wc2.1_5m_bio_3"))

saveRDS(st_env_r, file = "D:\\Environmental data\\Maxent\\stack_env_raster.rds")
st_env_r <- readRDS(file = "D:\\Environmental data\\Maxent\\stack_env_raster.rds")
## 최종적으로 bio layer 2, 4, 5, 16, 18, 19, elevation만 남았음.


list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_chiroptera_species)) {
  
  sp_point_species = sp_chiroptera_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="D:\\Environmental data\\Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="D:\\Environmental data\\Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}

list_maxent_model
list_remain_var

saveRDS(list_maxent_model, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_chiroptera.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_chiroptera.rds") -> list_maxent_model_chiroptera
saveRDS(list_remain_var, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_chiroptera.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_chiroptera.rds") -> list_remain_var_chiroptera


list_chiroptera_habitat =list(NA)
for (i in 1:length(list_maxent_model)) { 
  list_chiroptera_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_chiroptera_habitat[[160]])

saveRDS(list_chiroptera_habitat, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_chiroptera_habitat.rds")
list_chiroptera_habitat <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_chiroptera_habitat.rds") 


######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_chiroptera_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_chiroptera_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)

  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat
 
#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_chiroptera_habitat_filtered <- list_chiroptera_habitat
for(i in 1:length(list_chiroptera_habitat_filtered)){
  list_chiroptera_habitat_filtered[[i]][list_chiroptera_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_chiroptera_habitat_filtered[[i]][list_chiroptera_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_chiroptera_habitat_filtered[[4]]) 


saveRDS(list_chiroptera_habitat_filtered, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds")
list_chiroptera_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds") 





##### 동일하게 accipitriformes


library(readr)
accipitriformes <- read_delim("D:/Environmental data/Maxent/Species/accipitriformes.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
accipitriformes %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_accipitriformes_species

class(sp_accipitriformes_species)
sp_accipitriformes_species
plot(sp_accipitriformes_species[[1]])


accipitriformes_species <- as.data.frame(table(accipitriformes$species))
accipitriformes_species_over10 <- accipitriformes_species %>% 
  dplyr::filter(Freq > 10)

write.csv(accipitriformes_species, "D:\\Environmental data\\Maxent\\Species_list\\accipitriformes_species.csv")
write.csv(accipitriformes_species_over10, "D:\\Environmental data\\Maxent\\Species_list\\accipitriformes_species_over10.csv")

#list 안에 총 97 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_accipitriformes_species <- sp_accipitriformes_species[unlist(lapply(sp_accipitriformes_species, FUN = length)) > 5]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> kfold 해야해서 그런 것.


list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_accipitriformes_species)) {
  
  sp_point_species = sp_accipitriformes_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="D:\\Environmental data\\Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="D:\\Environmental data\\Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}

list_maxent_model
list_remain_var
threshold(list_maxent_model)


saveRDS(list_maxent_model, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_accipitriformes.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_accipitriformes.rds") -> list_maxent_model_accipitriformes
saveRDS(list_remain_var, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_accipitriformes.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_accipitriformes.rds") -> list_remain_var_accipitriformes


list_accipitriformes_habitat =list()
for (i in 1:length(list_maxent_model)) { 
  list_accipitriformes_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_accipitriformes_habitat[[4]])

saveRDS(list_accipitriformes_habitat, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_accipitriformes_habitat.rds")
list_accipitriformes_habitat <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_accipitriformes_habitat.rds") 




######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_accipitriformes_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_accipitriformes_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_accipitriformes_habitat_filtered <- list_accipitriformes_habitat
for(i in 1:length(list_accipitriformes_habitat_filtered)){
  list_accipitriformes_habitat_filtered[[i]][list_accipitriformes_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_accipitriformes_habitat_filtered[[i]][list_accipitriformes_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_accipitriformes_habitat_filtered[[5]]) 


saveRDS(list_accipitriformes_habitat_filtered, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds")
list_accipitriformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds") 






##### strigiformes


library(readr)
strigiformes <- read_delim("D:/Environmental data/Maxent/Species/strigiformes.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
strigiformes %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_strigiformes_species

class(sp_strigiformes_species)
sp_strigiformes_species
plot(sp_strigiformes_species[[1]])


#list 안에 총 97 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_strigiformes_species <- sp_strigiformes_species[unlist(lapply(sp_strigiformes_species, FUN = length)) > 5]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> kfold 해야해서 그런 것.


strigiformes_species <- as.data.frame(table(strigiformes$species))
strigiformes_species_over10 <- strigiformes_species %>% 
  dplyr::filter(Freq > 10)

write.csv(strigiformes_species, "D:\\Environmental data\\Maxent\\Species_list\\strigiformes_species.csv")
write.csv(strigiformes_species_over10, "D:\\Environmental data\\Maxent\\Species_list\\strigiformes_species_over10.csv")



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_strigiformes_species)) {
  
  sp_point_species = sp_strigiformes_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="D:\\Environmental data\\Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="D:\\Environmental data\\Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}

list_maxent_model
list_remain_var



saveRDS(list_maxent_model, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_strigiformes.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_strigiformes.rds") -> list_maxent_model_strigiformes
saveRDS(list_remain_var, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_strigiformes.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_strigiformes.rds") -> list_remain_var_strigiformes


list_strigiformes_habitat =list()
for (i in 1:length(list_maxent_model)) { 
  list_strigiformes_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_strigiformes_habitat[[4]])

saveRDS(list_strigiformes_habitat, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_strigiformes_habitat.rds")
list_strigiformes_habitat <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_strigiformes_habitat.rds") 




######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_strigiformes_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_strigiformes_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_strigiformes_habitat_filtered <- list_strigiformes_habitat
for(i in 1:length(list_strigiformes_habitat_filtered)){
  list_strigiformes_habitat_filtered[[i]][list_strigiformes_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_strigiformes_habitat_filtered[[i]][list_strigiformes_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_strigiformes_habitat_filtered[[4]]) 


saveRDS(list_strigiformes_habitat_filtered, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds")
list_strigiformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds") 






##### carnivora


library(readr)
carnivora <- read_delim("D:/Environmental data/Maxent/Species/carnivora.csv", 
                           delim = "\t", escape_double = FALSE, 
                           trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
carnivora %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_carnivora_species

class(sp_carnivora_species)
sp_carnivora_species
plot(sp_carnivora_species[[1]])


#list 안에 총 97 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_carnivora_species <- sp_carnivora_species[unlist(lapply(sp_carnivora_species, FUN = length)) > 5]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> kfold 해야해서 그런 것.


carnivora_species <- as.data.frame(table(carnivora$species))
carnivora_species_over10 <- carnivora_species %>% 
  dplyr::filter(Freq > 10)

write.csv(carnivora_species, "D:\\Environmental data\\Maxent\\Species_list\\carnivora_species.csv")
write.csv(carnivora_species_over10, "D:\\Environmental data\\Maxent\\Species_list\\carnivora_species_over10.csv")



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_carnivora_species)) {
  
  sp_point_species = sp_carnivora_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="D:\\Environmental data\\Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="D:\\Environmental data\\Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}

list_maxent_model
list_remain_var



saveRDS(list_maxent_model, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_carnivora.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_carnivora.rds") -> list_maxent_model_carnivora
saveRDS(list_remain_var, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_carnivora.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_carnivora.rds") -> list_remain_var_carnivora


list_carnivora_habitat =list()
for (i in 1:length(list_maxent_model)) { 
  list_carnivora_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_carnivora_habitat[[5]])

saveRDS(list_carnivora_habitat, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat.rds")
list_carnivora_habitat <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat.rds") 




######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_carnivora_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_carnivora_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_carnivora_habitat_filtered <- list_carnivora_habitat
for(i in 1:length(list_carnivora_habitat_filtered)){
  list_carnivora_habitat_filtered[[i]][list_carnivora_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_carnivora_habitat_filtered[[i]][list_carnivora_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_carnivora_habitat_filtered[[5]]) 


saveRDS(list_carnivora_habitat_filtered, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds")
list_carnivora_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds") 







##### colubridae


library(readr)
colubridae <- read_delim("D:/Environmental data/Maxent/Species/colubridae.csv", 
                        delim = "\t", escape_double = FALSE, 
                        trim_ws = TRUE)

#각 species 별로 구분된 spatialpoints 객체를 모아 놓은 list이다. 
colubridae %>% group_by(species) %>% group_map(~.x) %>%
  lapply(FUN = function(X) {
    SpatialPoints(coords = X[c("decimalLongitude", "decimalLatitude")], proj4string  = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }
  ) -> sp_colubridae_species

class(sp_colubridae_species)
sp_colubridae_species
plot(sp_colubridae_species[[1]])


#list 안에 총 97 종의 species가 있고, 각 species 별로 몇 개의 point가 찍혀 있는지 확인.
sp_colubridae_species <- sp_colubridae_species[unlist(lapply(sp_colubridae_species, FUN = length)) > 5]
#point 숫자가 최소한 10보다는 큰 종만 골라서 다시 subset -> kfold 해야해서 그런 것.


colubridae_species <- as.data.frame(table(colubridae$species))
colubridae_species_over10 <- colubridae_species %>% 
  dplyr::filter(Freq > 10)

write.csv(colubridae_species, "D:\\Environmental data\\Maxent\\Species_list\\colubridae_species.csv")
write.csv(colubridae_species_over10, "D:\\Environmental data\\Maxent\\Species_list\\colubridae_species_over10.csv")



list_maxent_model =list(NA)
list_remain_var = list(NA)
for (i in 1:length(sp_colubridae_species)) {
  
  sp_point_species = sp_colubridae_species[[i]]
  
  exclude_var = NA
  var_num = 1:nlayers(st_env_r)
  min_contr_var = 0
  
  model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                 args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                 removeDuplicates = FALSE,
                 path ="D:\\Environmental data\\Maxent\\data" )
  min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
  min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
  var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
  exclude_var = substr(var_name,1, nchar(var_name)-13)
  var_num = which(!names(st_env_r) %in% exclude_var)
  while(min_contr_var <= 10){
    
    model0= maxent(st_env_r[[var_num]], sp_point_species,  silent = TRUE,
                   args = prepPara(jackknife=TRUE, betamultiplier =1, replicates = 1),
                   removeDuplicates = FALSE,
                   path ="D:\\Environmental data\\Maxent\\data" )
    
    min_var = which.min(model0@results[grepl("contribution$",rownames(model0@results))])
    var_name = rownames(model0@results)[grepl("contribution$",rownames(model0@results))][min_var]
    min_contr_var = model0@results[grepl("contribution$",rownames(model0@results))][min_var]
    exclude_var = c(exclude_var, substr(var_name,1, nchar(var_name)-13))
    var_num = which(!names(st_env_r) %in% exclude_var)
  } 
  list_maxent_model[[i]]= model0
  list_remain_var[[i]] = names(st_env_r[[var_num]])
  
}

list_maxent_model
list_remain_var



saveRDS(list_maxent_model, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_colubridae.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_maxent_model_colubridae.rds") -> list_maxent_model_colubridae
saveRDS(list_remain_var, file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_colubridae.rds")
readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_models\\list_remain_var_colubridae.rds") -> list_remain_var_colubridae


list_colubridae_habitat =list()
for (i in 1:length(list_maxent_model)) { 
  list_colubridae_habitat[[i]] <- predict(list_maxent_model[[i]], st_env_r)
}

plot(list_colubridae_habitat[[5]])

saveRDS(list_colubridae_habitat, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat.rds")
list_colubridae_habitat <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat.rds") 




######## evaluate/threshold

#### k-fold를 5로 잡아서 train과 test dateset 분리하기.
fold.list<-list()
length(list_maxent_model)
for(i in 1:length(list_maxent_model)){
  fold.list[[i]]<-kfold(sp_colubridae_species[[i]],k=5)
}

fold.list

#### 각 모델 evaluate 하기
evaluate.mat<-list()
for(i in 1:length(list_maxent_model)){
  
  loc.test<-sp_colubridae_species[[i]][fold.list[[i]] == 1,]
  
  bg <- randomPoints(st_env_r, length(loc.test))
  e1 <- evaluate(list_maxent_model[[i]], p=loc.test, a=bg, x=st_env_r)
  
  evaluate.mat[[i]]<-e1
}
evaluate.mat

#### evaluate 한 객체들에서 threshold 값만 빼내기
threshold.mat <- list()
for(i in 1:length(evaluate.mat)){
  t <- threshold(evaluate.mat[[i]])
  threshold.mat[[i]] <- t[["prevalence"]]
}
threshold.mat

#### 각 raster의 픽셀에 나타난 확률값들을 위에서 구한 threshold 기준으로 threshold를 넘으면 1, 넘지 않으면 0으로 하여 출현/비출현 구분.
list_colubridae_habitat_filtered <- list_colubridae_habitat
for(i in 1:length(list_colubridae_habitat_filtered)){
  list_colubridae_habitat_filtered[[i]][list_colubridae_habitat_filtered[[i]] < threshold.mat[[i]]] <- 0 
  list_colubridae_habitat_filtered[[i]][list_colubridae_habitat_filtered[[i]] >= threshold.mat[[i]]] <- 1
}

plot(list_colubridae_habitat_filtered[[6]]) 


saveRDS(list_colubridae_habitat_filtered, file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds")
list_colubridae_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds") 











#################### Raster count
setwd("D:\\Environmental data\\Base_map_numpoints")
terrestrial.grid <- readOGR(dsn = "Base_map_numpoints.shp")



##################### Chiroptera

list_chiroptera_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_chiroptera_habitat_filtered.rds") 

rm(chiroptera.mat)
a<-Sys.time()
chiroptera.mat<- foreach(i = 1:length(list_chiroptera_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_chiroptera_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    chiroptera.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(chiroptera.temp)
  }
b<-Sys.time()
b-a

chiroptera.mat <- as.data.frame(chiroptera.mat)
chiroptera.mat <- cbind(ID, chiroptera.mat)

area <- accipitriformes_int_area_mat %>% 
  dplyr::select(id, area)
area <- unique(area)

chiroptera.mat_1 <- chiroptera.mat %>% 
  left_join(area, by = c("ID" = "id"))

chiroptera.mat_1 <- apply(chiroptera.mat_1,2,as.character)


write.csv(chiroptera.mat_1,"D:\\Environmental data\\Maxent\\Species_mat\\chiroptera_mat.csv")
chiroptera_mat_1<- read.csv("D:\\Environmental data\\Maxent\\Species_mat\\chiroptera_mat.csv")

for (i in 3:178) {
    chiroptera_mat_1[i + 178] <- (chiroptera_mat_1[i] > (chiroptera_mat_1[180] / 2))
}

chiroptera_mat_1$count_chiroptera_prop50 <- rowSums(chiroptera_mat_1[,181:356])

chiroptera_existence <- chiroptera_mat_1 %>% 
  dplyr::select(ID, count_chiroptera_existence)

chiroptera_prop50 <- chiroptera_mat_1 %>% 
  dplyr::select(ID, count_chiroptera_prop50)

chiroptera_count <- chiroptera_existence %>% 
  left_join(chiroptera_prop50, by = "ID")
  
write.csv(chiroptera_count,"D:\\Environmental data\\Maxent\\Species_mat\\chiroptera_count.csv")


##################### Accipitriformes

list_accipitriformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_accipitriformes_habitat_filtered.rds") 


a<-Sys.time()
accipitriformes.mat<- foreach(i = 1:length(list_accipitriformes_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_accipitriformes_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    accipitriformes.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(accipitriformes.temp)
  }
b<-Sys.time()
b-a

accipitriformes.mat <- as.data.frame(accipitriformes.mat)
accipitriformes.mat <- cbind(ID, accipitriformes.mat)


accipitriformes.mat_1 <- accipitriformes.mat %>% 
  left_join(area, by = c("ID" = "id"))

accipitriformes.mat_1 <- apply(accipitriformes.mat_1,2,as.character)


write.csv(accipitriformes.mat_1,"D:\\Environmental data\\Maxent\\Species_mat\\accipitriformes_mat.csv")

accipitriformes_mat_1<- read.csv("D:\\Environmental data\\Maxent\\Species_mat\\accipitriformes_mat.csv")

for (i in 3:70) {
  accipitriformes_mat_1[i + 70] <- (accipitriformes_mat_1[i] > 0)
}


for (i in 3:70) {
  accipitriformes_mat_1[i + 70] <- (accipitriformes_mat_1[i] > (accipitriformes_mat_1[72] / 2))
}

accipitriformes_mat_1$count_accipitriformes_existence <- rowSums(accipitriformes_mat_1[,73:140])

accipitriformes_mat_1$count_accipitriformes_prop50 <- rowSums(accipitriformes_mat_1[,73:140])

accipitriformes_existence <- accipitriformes_mat_1 %>% 
  dplyr::select(ID, count_accipitriformes_existence)

accipitriformes_prop50 <- accipitriformes_mat_1 %>% 
  dplyr::select(ID, count_accipitriformes_prop50)

accipitriformes_count <- accipitriformes_existence %>% 
  left_join(accipitriformes_prop50, by = "ID")

write.csv(accipitriformes_count,"D:\\Environmental data\\Maxent\\Species_mat\\accipitriformes_count.csv")




##################### Strigiformes

list_strigiformes_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_strigiformes_habitat_filtered.rds") 


a<-Sys.time()
strigiformes.mat<- foreach(i = 1:length(list_strigiformes_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_strigiformes_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    strigiformes.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(strigiformes.temp)
  }
b<-Sys.time()
b-a

strigiformes.mat <- as.data.frame(strigiformes.mat)
strigiformes.mat <- cbind(ID, strigiformes.mat)


strigiformes.mat_1 <- strigiformes.mat %>% 
  left_join(area, by = c("ID" = "id"))

strigiformes.mat_1 <- apply(strigiformes.mat_1,2,as.character)


write.csv(strigiformes.mat_1,"D:\\Environmental data\\Maxent\\Species_mat\\strigiformes_mat.csv")


strigiformes_mat_1<- read.csv("D:\\Environmental data\\Maxent\\Species_mat\\strigiformes_mat.csv")

for (i in 3:23) {
  strigiformes_mat_1[i + 23] <- (strigiformes_mat_1[i] > 0)
}


for (i in 3:23) {
  strigiformes_mat_1[i + 23] <- (strigiformes_mat_1[i] > (strigiformes_mat_1[25] / 2))
}

strigiformes_mat_1$count_strigiformes_existence <- rowSums(strigiformes_mat_1[,26:46])

strigiformes_mat_1$count_strigiformes_prop50 <- rowSums(strigiformes_mat_1[,26:46])

strigiformes_existence <- strigiformes_mat_1 %>% 
  dplyr::select(ID, count_strigiformes_existence)

strigiformes_prop50 <- strigiformes_mat_1 %>% 
  dplyr::select(ID, count_strigiformes_prop50)

strigiformes_count <- strigiformes_existence %>% 
  left_join(strigiformes_prop50, by = "ID")

write.csv(strigiformes_count,"D:\\Environmental data\\Maxent\\Species_mat\\strigiformes_count.csv")




##################### Carnivora

list_carnivora_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_carnivora_habitat_filtered.rds") 


a<-Sys.time()
carnivora.mat<- foreach(i = 1:length(list_carnivora_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_carnivora_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    carnivora.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(carnivora.temp)
  }
b<-Sys.time()
b-a

carnivora.mat <- as.data.frame(carnivora.mat)
carnivora.mat <- cbind(ID, carnivora.mat)


carnivora.mat_1 <- carnivora.mat %>% 
  left_join(area, by = c("ID" = "id"))

carnivora.mat_1 <- apply(carnivora.mat_1,2,as.character)


write.csv(carnivora.mat_1,"D:\\Environmental data\\Maxent\\Species_mat\\carnivora_mat.csv")


carnivora_mat_1<- read.csv("D:\\Environmental data\\Maxent\\Species_mat\\carnivora_mat.csv")

len <- length(colnames(carnivora_mat_1)) - 2

for (i in 3:len) {
  carnivora_mat_1[i + len] <- (carnivora_mat_1[i] > 0)
}


for (i in 3:len) {
  carnivora_mat_1[i + len] <- (carnivora_mat_1[i] > (carnivora_mat_1[len + 2] / 2))
}

carnivora_mat_1$count_carnivora_existence <- rowSums(carnivora_mat_1[,(len + 3):122])

carnivora_mat_1$count_carnivora_prop50 <- rowSums(carnivora_mat_1[,(len + 3):122])

carnivora_existence <- carnivora_mat_1 %>% 
  dplyr::select(ID, count_carnivora_existence)

carnivora_prop50 <- carnivora_mat_1 %>% 
  dplyr::select(ID, count_carnivora_prop50)

carnivora_count <- carnivora_existence %>% 
  left_join(carnivora_prop50, by = "ID")

write.csv(carnivora_count,"D:\\Environmental data\\Maxent\\Species_mat\\carnivora_count.csv")



.

##################### colubridae

list_colubridae_habitat_filtered <- readRDS(file = "D:\\Environmental data\\Maxent\\Maxent_raster\\list_colubridae_habitat_filtered.rds") 


a<-Sys.time()
colubridae.mat<- foreach(i = 1:length(list_colubridae_habitat_filtered),.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(list_colubridae_habitat_filtered[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    colubridae.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(colubridae.temp)
  }
b<-Sys.time()
b-a

colubridae.mat <- as.data.frame(colubridae.mat)
colubridae.mat <- cbind(ID, colubridae.mat)


colubridae.mat_1 <- colubridae.mat %>% 
  left_join(area, by = c("ID" = "id"))

colubridae.mat_1 <- apply(colubridae.mat_1,2,as.character)


write.csv(colubridae.mat_1,"D:\\Environmental data\\Maxent\\Species_mat\\colubridae_mat.csv")


colubridae_mat_1<- read.csv("D:\\Environmental data\\Maxent\\Species_mat\\colubridae_mat.csv")

len <- length(colnames(colubridae_mat_1)) - 2

for (i in 3:len) {
  colubridae_mat_1[i + len] <- (colubridae_mat_1[i] > 0)
}


for (i in 3:len) {
  colubridae_mat_1[i + len] <- (colubridae_mat_1[i] > (colubridae_mat_1[len + 2] / 2))
}

colubridae_mat_1$count_colubridae_existence <- rowSums(colubridae_mat_1[,(len + 3):146])

colubridae_mat_1$count_colubridae_prop50 <- rowSums(colubridae_mat_1[,(len + 3):146])

colubridae_existence <- colubridae_mat_1 %>% 
  dplyr::select(ID, count_colubridae_existence)

colubridae_prop50 <- colubridae_mat_1 %>% 
  dplyr::select(ID, count_colubridae_prop50)

colubridae_count <- colubridae_existence %>% 
  left_join(colubridae_prop50, by = "ID")

write.csv(colubridae_count,"D:\\Environmental data\\Maxent\\Species_mat\\colubridae_count.csv")

