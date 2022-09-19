rm(list=ls())
rm=(list=LS())
gc(reset=TRUE)

###################################################
### Data extraction from global map ###############
###################################################


## terrestrial grid
library(sf)
library(rgdal)
library(raster)
library(rgeos)
library(dismo)


setwd("D:\\Environmental data\\Base_map_numpoints")
terrestrial.grid <- readOGR(dsn = "Base_map_numpoints.shp")
terrestrial.grid_1 <- st_read("Base_map_numpoints.shp")
plot(terrestrial.grid)
crs(terrestrial.grid) #4326




############################################################################################################################
## For polygon ###############################################
############################################################################################################################

##############################################################
# Avian (Accipitriformes) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Accipitriformes")
time.1<-Sys.time()

polygon.accipitriformes <- readOGR(dsn = "data_0.shp")
crs(polygon.accipitriformes) #4326


#intersect를 계산한다.
library(rgeos)
intsct.accipitriformes <- gIntersection(polygon.accipitriformes, terrestrial.grid, byid=TRUE, checkValidity)
intsct.accipitriformes <- gIntersects(polygon.accipitriformes, terrestrial.grid, byid=TRUE)
intsct.accipitriformes<-intsct.accipitriformes+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.accipitriformes)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.accipitriformes)<-paste0("accipitriformes_",polygon.accipitriformes@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.accipitriformes))   #218
length(unique(colnames(intsct.accipitriformes))) #90

dim(intsct.accipitriformes)
head(intsct.accipitriformes)

#unique만 계수할 수 있도록 정리.
intsct.accipitriformes.final<-intsct.accipitriformes[ ,1:length(unique(colnames(intsct.accipitriformes)))]
for(i in 1:length(unique(colnames(intsct.accipitriformes)))){ 
  if(sum(colnames(intsct.accipitriformes)==unique(colnames(intsct.accipitriformes))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.accipitriformes.final[,i]<-intsct.accipitriformes[,colnames(intsct.accipitriformes)==unique(colnames(intsct.accipitriformes))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.accipitriformes.final[,i]<-apply(intsct.accipitriformes[,colnames(intsct.accipitriformes)==unique(colnames(intsct.accipitriformes))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.accipitriformes.final)<-unique(colnames(intsct.accipitriformes))

length(colnames(intsct.accipitriformes.final)) #90으로 줄어들었음
length(unique(colnames(intsct.accipitriformes.final))) #동일하게 90

accipitriformes.book<-cbind(paste0('accipitriformes_',1:90) ,unique(colnames(intsct.accipitriformes)))


write.csv(intsct.accipitriformes.final,'accipitriformes_presences.csv' )
write.csv(accipitriformes.book,'accipitriformes_ID_species.csv' )


sum(is.na(intsct.accipitriformes.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.accipitriformes@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.accipitriformes@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.accipitriformes@data$PRESENCE, polygon.accipitriformes@data$SEASONAL)

which(polygon.accipitriformes@data$PRESENCE %in% c(1,2))   # 184
which(polygon.accipitriformes@data$PRESENCE %in% c(4,5))   # 28
which(polygon.accipitriformes@data$SEASONAL ==1)           # 113 resident인 경우만

Extant_resident<-which(polygon.accipitriformes@data$PRESENCE %in% c(1,2) & polygon.accipitriformes@data$SEASONAL ==1)    # 89 (Extant & resident)
Extinct_resident<-which(polygon.accipitriformes@data$PRESENCE %in% c(4,5) & polygon.accipitriformes@data$SEASONAL ==1)   # 19  (Extinct & resident)

#Resident만 골라서,
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.accipitriformes.extant <-intsct.accipitriformes[,Extant_resident] 
intsct.accipitriformes.extinct<-intsct.accipitriformes[,Extinct_resident]


length(colnames(intsct.accipitriformes))         #218
length(unique(colnames(intsct.accipitriformes))) #90

length(colnames(intsct.accipitriformes.extant))         #89
length(unique(colnames(intsct.accipitriformes.extant))) #82

length(colnames(intsct.accipitriformes.extinct))         #19
length(unique(colnames(intsct.accipitriformes.extinct))) #13


dim(intsct.accipitriformes) # rows 635, columns 218
head(intsct.accipitriformes)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.accipitriformes.extant.final<-intsct.accipitriformes.extant[,1:length(unique(colnames(intsct.accipitriformes.extant)))]
for(i in 1:length(unique(colnames(intsct.accipitriformes.extant)))){
  if(sum(colnames(intsct.accipitriformes.extant)==unique(colnames(intsct.accipitriformes.extant))[i])==1){
    intsct.accipitriformes.extant.final[,i]<-intsct.accipitriformes.extant[,colnames(intsct.accipitriformes.extant)==unique(colnames(intsct.accipitriformes.extant))[i] ]
  }else{
    intsct.accipitriformes.extant.final[,i]<-apply(intsct.accipitriformes.extant[,colnames(intsct.accipitriformes.extant)==unique(colnames(intsct.accipitriformes.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.accipitriformes.extant.final)<-unique(colnames(intsct.accipitriformes.extant))

length(colnames(intsct.accipitriformes.extant.final)) #82
length(unique(colnames(intsct.accipitriformes.extant.final))) #82 둘이 같으니까 잘 나온 것.

accipitriformes.extant.book<-cbind(paste0('accipitriformes_',1:82) ,unique(colnames(intsct.accipitriformes.extant)))
colnames(intsct.accipitriformes.extant.final)<-paste0('accipitriformes_',1:82)

write.csv(intsct.accipitriformes.extant.final,'accipitriformes_presences_Extant.csv' )
write.csv(accipitriformes.extant.book,'accipitriformes_ID_species_Extant.csv' )


intsct.accipitriformes.extinct.final<-intsct.accipitriformes.extinct[,1:length(unique(colnames(intsct.accipitriformes.extinct)))]
for(i in 1:length(unique(colnames(intsct.accipitriformes.extinct)))){
  if(sum(colnames(intsct.accipitriformes.extinct)==unique(colnames(intsct.accipitriformes.extinct))[i])==1){
    intsct.accipitriformes.extinct.final[,i]<-intsct.accipitriformes.extinct[,colnames(intsct.accipitriformes.extinct)==unique(colnames(intsct.accipitriformes.extinct))[i] ]
  }else{
    intsct.accipitriformes.extinct.final[,i]<-apply(intsct.accipitriformes.extinct[,colnames(intsct.accipitriformes.extinct)==unique(colnames(intsct.accipitriformes.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.accipitriformes.extinct.final)<-unique(colnames(intsct.accipitriformes.extinct))

length(colnames(intsct.accipitriformes.extinct.final)) # 13
length(unique(colnames(intsct.accipitriformes.extinct.final))) # 13

accipitriformes.extinct.book<-cbind(paste0('accipitriformes_',1:13) ,unique(colnames(intsct.accipitriformes.extinct)))
colnames(intsct.accipitriformes.extinct.final)<-paste0('accipitriformes_',1:13)


write.csv(intsct.accipitriformes.extinct.final,'accipitriformes_presences_extinct.csv' )
write.csv(accipitriformes.extinct.book,'accipitriformes_ID_species_extinct.csv' )


sum(is.na(intsct.accipitriformes.final))






##############################################################
# Avian (Strigiformes) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Strigiformes")
time.1<-Sys.time()

polygon.strigiformes <- readOGR(dsn = "data_0.shp")
crs(polygon.strigiformes) #4326

#intersect를 계산한다.
library(rgeos)
intsct.strigiformes <- gIntersects(polygon.strigiformes, terrestrial.grid, byid=TRUE)
intsct.strigiformes<-intsct.strigiformes+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.strigiformes)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.strigiformes)<-paste0("strigiformes_",polygon.strigiformes@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.strigiformes))   #320
length(unique(colnames(intsct.strigiformes))) #244

dim(intsct.strigiformes)
head(intsct.strigiformes)

#unique만 계수할 수 있도록 정리.
intsct.strigiformes.final<-intsct.strigiformes[ ,1:length(unique(colnames(intsct.strigiformes)))]
for(i in 1:length(unique(colnames(intsct.strigiformes)))){ 
  if(sum(colnames(intsct.strigiformes)==unique(colnames(intsct.strigiformes))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.strigiformes.final[,i]<-intsct.strigiformes[,colnames(intsct.strigiformes)==unique(colnames(intsct.strigiformes))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.strigiformes.final[,i]<-apply(intsct.strigiformes[,colnames(intsct.strigiformes)==unique(colnames(intsct.strigiformes))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.strigiformes.final)<-unique(colnames(intsct.strigiformes))

length(colnames(intsct.strigiformes.final)) #244으로 줄어들었음
length(unique(colnames(intsct.strigiformes.final))) #동일하게 244

strigiformes.book<-cbind(paste0('strigiformes_',1:244) ,unique(colnames(intsct.strigiformes)))


write.csv(intsct.strigiformes.final,'strigiformes_presences.csv' )
write.csv(strigiformes.book,'strigiformes_ID_species.csv' )


sum(is.na(intsct.strigiformes.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.strigiformes@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.strigiformes@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.strigiformes@data$PRESENCE, polygon.strigiformes@data$SEASONAL)

which(polygon.strigiformes@data$PRESENCE %in% c(1,2))   # 289
which(polygon.strigiformes@data$PRESENCE %in% c(4,5))   # 13
which(polygon.strigiformes@data$SEASONAL ==1)           # 282 resident인 경우만

Extant_resident<-which(polygon.strigiformes@data$PRESENCE %in% c(1,2) & polygon.strigiformes@data$SEASONAL ==1)    # 251 (Extant & resident)
Extinct_resident<-which(polygon.strigiformes@data$PRESENCE %in% c(4,5) & polygon.strigiformes@data$SEASONAL ==1)   # 13  (Extinct & resident)

#Resident만 골라서, 
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.strigiformes.extant <-intsct.strigiformes[,Extant_resident] 
intsct.strigiformes.extinct<-intsct.strigiformes[,Extinct_resident]


length(colnames(intsct.strigiformes))         #320
length(unique(colnames(intsct.strigiformes))) #244

length(colnames(intsct.strigiformes.extant))         #251
length(unique(colnames(intsct.strigiformes.extant))) #237

length(colnames(intsct.strigiformes.extinct))         #13
length(unique(colnames(intsct.strigiformes.extinct))) #12


dim(intsct.strigiformes) # rows 635, columns 320
head(intsct.strigiformes)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.strigiformes.extant.final<-intsct.strigiformes.extant[,1:length(unique(colnames(intsct.strigiformes.extant)))]
for(i in 1:length(unique(colnames(intsct.strigiformes.extant)))){
  if(sum(colnames(intsct.strigiformes.extant)==unique(colnames(intsct.strigiformes.extant))[i])==1){
    intsct.strigiformes.extant.final[,i]<-intsct.strigiformes.extant[,colnames(intsct.strigiformes.extant)==unique(colnames(intsct.strigiformes.extant))[i] ]
  }else{
    intsct.strigiformes.extant.final[,i]<-apply(intsct.strigiformes.extant[,colnames(intsct.strigiformes.extant)==unique(colnames(intsct.strigiformes.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.strigiformes.extant.final)<-unique(colnames(intsct.strigiformes.extant))

length(colnames(intsct.strigiformes.extant.final)) #237
length(unique(colnames(intsct.strigiformes.extant.final))) #237 둘이 같으니까 잘 나온 것.

strigiformes.extant.book<-cbind(paste0('strigiformes_',1:237) ,unique(colnames(intsct.strigiformes.extant)))
colnames(intsct.strigiformes.extant.final)<-paste0('strigiformes_',1:237)

write.csv(intsct.strigiformes.extant.final,'strigiformes_presences_Extant.csv' )
write.csv(strigiformes.extant.book,'strigiformes_ID_species_Extant.csv' )


intsct.strigiformes.extinct.final<-intsct.strigiformes.extinct[,1:length(unique(colnames(intsct.strigiformes.extinct)))]
for(i in 1:length(unique(colnames(intsct.strigiformes.extinct)))){
  if(sum(colnames(intsct.strigiformes.extinct)==unique(colnames(intsct.strigiformes.extinct))[i])==1){
    intsct.strigiformes.extinct.final[,i]<-intsct.strigiformes.extinct[,colnames(intsct.strigiformes.extinct)==unique(colnames(intsct.strigiformes.extinct))[i] ]
  }else{
    intsct.strigiformes.extinct.final[,i]<-apply(intsct.strigiformes.extinct[,colnames(intsct.strigiformes.extinct)==unique(colnames(intsct.strigiformes.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.strigiformes.extinct.final)<-unique(colnames(intsct.strigiformes.extinct))

length(colnames(intsct.strigiformes.extinct.final)) # 12
length(unique(colnames(intsct.strigiformes.extinct.final))) # 12

strigiformes.extinct.book<-cbind(paste0('strigiformes_',1:12) ,unique(colnames(intsct.strigiformes.extinct)))
colnames(intsct.strigiformes.extinct.final)<-paste0('strigiformes_',1:12)


write.csv(intsct.strigiformes.extinct.final,'strigiformes_presences_extinct.csv' )
write.csv(strigiformes.extinct.book,'strigiformes_ID_species_extinct.csv' )


sum(is.na(intsct.strigiformes.final))



##############################################################
# Avian (Falconiformes) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Falconiformes")

polygon.falconiformes <- readOGR(dsn = "data_0.shp")
crs(polygon.falconiformes) #4326

#intersect를 계산한다.
library(rgeos)
intsct.falconiformes <- gIntersects(polygon.falconiformes, terrestrial.grid, byid=TRUE)
intsct.falconiformes<-intsct.falconiformes+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.falconiformes)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.falconiformes)<-paste0("falconiformes_",polygon.falconiformes@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.falconiformes))   #56
length(unique(colnames(intsct.falconiformes))) #24

dim(intsct.falconiformes)
head(intsct.falconiformes)

#unique만 계수할 수 있도록 정리.
intsct.falconiformes.final<-intsct.falconiformes[ ,1:length(unique(colnames(intsct.falconiformes)))]
for(i in 1:length(unique(colnames(intsct.falconiformes)))){ 
  if(sum(colnames(intsct.falconiformes)==unique(colnames(intsct.falconiformes))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.falconiformes.final[,i]<-intsct.falconiformes[,colnames(intsct.falconiformes)==unique(colnames(intsct.falconiformes))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.falconiformes.final[,i]<-apply(intsct.falconiformes[,colnames(intsct.falconiformes)==unique(colnames(intsct.falconiformes))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.falconiformes.final)<-unique(colnames(intsct.falconiformes))

length(colnames(intsct.falconiformes.final)) #24으로 줄어들었음
length(unique(colnames(intsct.falconiformes.final))) #동일하게 24

falconiformes.book<-cbind(paste0('falconiformes_',1:24) ,unique(colnames(intsct.falconiformes)))


write.csv(intsct.falconiformes.final,'falconiformes_presences.csv' )
write.csv(falconiformes.book,'falconiformes_ID_species.csv' )


sum(is.na(intsct.falconiformes.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.falconiformes@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.falconiformes@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.falconiformes@data$PRESENCE, polygon.falconiformes@data$SEASONAL)

which(polygon.falconiformes@data$PRESENCE %in% c(1,2))   # 52
which(polygon.falconiformes@data$PRESENCE %in% c(4,5))   # 3
which(polygon.falconiformes@data$SEASONAL ==1)           # 23 resident인 경우만

Extant_resident<-which(polygon.falconiformes@data$PRESENCE %in% c(1,2) & polygon.falconiformes@data$SEASONAL ==1)    # 251 (Extant & resident)
Extinct_resident<-which(polygon.falconiformes@data$PRESENCE %in% c(4,5) & polygon.falconiformes@data$SEASONAL ==1)   # 13  (Extinct & resident)

#Resident만 골라서, 
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.falconiformes.extant <-intsct.falconiformes[,Extant_resident] 
intsct.falconiformes.extinct<-intsct.falconiformes[,Extinct_resident]


length(colnames(intsct.falconiformes))         #56
length(unique(colnames(intsct.falconiformes))) #24

length(colnames(intsct.falconiformes.extant))         #19
length(unique(colnames(intsct.falconiformes.extant))) #19

length(colnames(intsct.falconiformes.extinct))         #3
length(unique(colnames(intsct.falconiformes.extinct))) #3


dim(intsct.falconiformes) # rows 635, columns 56
head(intsct.falconiformes)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.falconiformes.extant.final<-intsct.falconiformes.extant[,1:length(unique(colnames(intsct.falconiformes.extant)))]
for(i in 1:length(unique(colnames(intsct.falconiformes.extant)))){
  if(sum(colnames(intsct.falconiformes.extant)==unique(colnames(intsct.falconiformes.extant))[i])==1){
    intsct.falconiformes.extant.final[,i]<-intsct.falconiformes.extant[,colnames(intsct.falconiformes.extant)==unique(colnames(intsct.falconiformes.extant))[i] ]
  }else{
    intsct.falconiformes.extant.final[,i]<-apply(intsct.falconiformes.extant[,colnames(intsct.falconiformes.extant)==unique(colnames(intsct.falconiformes.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.falconiformes.extant.final)<-unique(colnames(intsct.falconiformes.extant))

length(colnames(intsct.falconiformes.extant.final)) #19
length(unique(colnames(intsct.falconiformes.extant.final))) #19 둘이 같으니까 잘 나온 것.

falconiformes.extant.book<-cbind(paste0('falconiformes_',1:19) ,unique(colnames(intsct.falconiformes.extant)))
colnames(intsct.falconiformes.extant.final)<-paste0('falconiformes_',1:19)

write.csv(intsct.falconiformes.extant.final,'falconiformes_presences_Extant.csv' )
write.csv(falconiformes.extant.book,'falconiformes_ID_species_Extant.csv' )


intsct.falconiformes.extinct.final<-intsct.falconiformes.extinct[,1:length(unique(colnames(intsct.falconiformes.extinct)))]
for(i in 1:length(unique(colnames(intsct.falconiformes.extinct)))){
  if(sum(colnames(intsct.falconiformes.extinct)==unique(colnames(intsct.falconiformes.extinct))[i])==1){
    intsct.falconiformes.extinct.final[,i]<-intsct.falconiformes.extinct[,colnames(intsct.falconiformes.extinct)==unique(colnames(intsct.falconiformes.extinct))[i] ]
  }else{
    intsct.falconiformes.extinct.final[,i]<-apply(intsct.falconiformes.extinct[,colnames(intsct.falconiformes.extinct)==unique(colnames(intsct.falconiformes.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.falconiformes.extinct.final)<-unique(colnames(intsct.falconiformes.extinct))

length(colnames(intsct.falconiformes.extinct.final)) # 3
length(unique(colnames(intsct.falconiformes.extinct.final))) # 3

falconiformes.extinct.book<-cbind(paste0('falconiformes_',1:3) ,unique(colnames(intsct.falconiformes.extinct)))
colnames(intsct.falconiformes.extinct.final)<-paste0('falconiformes_',1:3)


write.csv(intsct.falconiformes.extinct.final,'falconiformes_presences_extinct.csv' )
write.csv(falconiformes.extinct.book,'falconiformes_ID_species_extinct.csv' )


sum(is.na(intsct.falconiformes.final))



##############################################################
# Bats (Chiroptera) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Bat_species")

polygon.chiroptera <- readOGR(dsn = "data_0.shp")
crs(polygon.chiroptera) #4326

#intersect를 계산한다.
library(rgeos)
intsct.chiroptera <- gIntersects(polygon.chiroptera, terrestrial.grid, byid=TRUE)
intsct.chiroptera<-intsct.chiroptera+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.chiroptera)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.chiroptera)<-paste0("chiroptera_",polygon.chiroptera@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.chiroptera))   #507
length(unique(colnames(intsct.chiroptera))) #289

dim(intsct.chiroptera)
head(intsct.chiroptera)

#unique만 계수할 수 있도록 정리.
intsct.chiroptera.final<-intsct.chiroptera[ ,1:length(unique(colnames(intsct.chiroptera)))]
for(i in 1:length(unique(colnames(intsct.chiroptera)))){ 
  if(sum(colnames(intsct.chiroptera)==unique(colnames(intsct.chiroptera))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.chiroptera.final[,i]<-intsct.chiroptera[,colnames(intsct.chiroptera)==unique(colnames(intsct.chiroptera))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.chiroptera.final[,i]<-apply(intsct.chiroptera[,colnames(intsct.chiroptera)==unique(colnames(intsct.chiroptera))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.chiroptera.final)<-unique(colnames(intsct.chiroptera))

length(colnames(intsct.chiroptera.final)) #289으로 줄어들었음
length(unique(colnames(intsct.chiroptera.final))) #동일하게 289

chiroptera.book<-cbind(paste0('chiroptera_',1:289) ,unique(colnames(intsct.chiroptera)))


write.csv(intsct.chiroptera.final,'chiroptera_presences.csv' )
write.csv(chiroptera.book,'chiroptera_ID_species.csv' )


sum(is.na(intsct.chiroptera.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.chiroptera@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.chiroptera@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.chiroptera@data$PRESENCE, polygon.chiroptera@data$SEASONAL)

length(which(polygon.chiroptera@data$PRESENCE %in% c(1,2)))   # 445
length(which(polygon.chiroptera@data$PRESENCE %in% c(4,5)))   # 4
length(which(polygon.chiroptera@data$SEASONAL ==1))           # 507 resident인 경우만

Extant_resident<-which(polygon.chiroptera@data$PRESENCE %in% c(1,2) & polygon.chiroptera@data$SEASONAL ==1)    # 251 (Extant & resident)
Extinct_resident<-which(polygon.chiroptera@data$PRESENCE %in% c(4,5) & polygon.chiroptera@data$SEASONAL ==1)   # 13  (Extinct & resident)

#Resident만 골라서, 
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.chiroptera.extant <-intsct.chiroptera[,Extant_resident] 
intsct.chiroptera.extinct<-intsct.chiroptera[,Extinct_resident]


length(colnames(intsct.chiroptera))         #507
length(unique(colnames(intsct.chiroptera))) #289

length(colnames(intsct.chiroptera.extant))         #445
length(unique(colnames(intsct.chiroptera.extant))) #287

length(colnames(intsct.chiroptera.extinct))         #4
length(unique(colnames(intsct.chiroptera.extinct))) #4


dim(intsct.chiroptera) # rows 635, columns 507
head(intsct.chiroptera)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.chiroptera.extant.final<-intsct.chiroptera.extant[,1:length(unique(colnames(intsct.chiroptera.extant)))]
for(i in 1:length(unique(colnames(intsct.chiroptera.extant)))){
  if(sum(colnames(intsct.chiroptera.extant)==unique(colnames(intsct.chiroptera.extant))[i])==1){
    intsct.chiroptera.extant.final[,i]<-intsct.chiroptera.extant[,colnames(intsct.chiroptera.extant)==unique(colnames(intsct.chiroptera.extant))[i] ]
  }else{
    intsct.chiroptera.extant.final[,i]<-apply(intsct.chiroptera.extant[,colnames(intsct.chiroptera.extant)==unique(colnames(intsct.chiroptera.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.chiroptera.extant.final)<-unique(colnames(intsct.chiroptera.extant))

length(colnames(intsct.chiroptera.extant.final)) #287
length(unique(colnames(intsct.chiroptera.extant.final))) #287 둘이 같으니까 잘 나온 것.

chiroptera.extant.book<-cbind(paste0('chiroptera_',1:287) ,unique(colnames(intsct.chiroptera.extant)))
colnames(intsct.chiroptera.extant.final)<-paste0('chiroptera_',1:287)

write.csv(intsct.chiroptera.extant.final,'chiroptera_presences_Extant.csv' )
write.csv(chiroptera.extant.book,'chiroptera_ID_species_Extant.csv' )


intsct.chiroptera.extinct.final<-intsct.chiroptera.extinct[,1:length(unique(colnames(intsct.chiroptera.extinct)))]
for(i in 1:length(unique(colnames(intsct.chiroptera.extinct)))){
  if(sum(colnames(intsct.chiroptera.extinct)==unique(colnames(intsct.chiroptera.extinct))[i])==1){
    intsct.chiroptera.extinct.final[,i]<-intsct.chiroptera.extinct[,colnames(intsct.chiroptera.extinct)==unique(colnames(intsct.chiroptera.extinct))[i] ]
  }else{
    intsct.chiroptera.extinct.final[,i]<-apply(intsct.chiroptera.extinct[,colnames(intsct.chiroptera.extinct)==unique(colnames(intsct.chiroptera.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.chiroptera.extinct.final)<-unique(colnames(intsct.chiroptera.extinct))

length(colnames(intsct.chiroptera.extinct.final)) # 4
length(unique(colnames(intsct.chiroptera.extinct.final))) # 4

chiroptera.extinct.book<-cbind(paste0('chiroptera_',1:4) ,unique(colnames(intsct.chiroptera.extinct)))
colnames(intsct.chiroptera.extinct.final)<-paste0('chiroptera_',1:4)


write.csv(intsct.chiroptera.extinct.final,'chiroptera_presences_extinct.csv' )
write.csv(chiroptera.extinct.book,'chiroptera_ID_species_extinct.csv' )


sum(is.na(intsct.chiroptera.final))




##############################################################
# Cats (Carnivora) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Carnivora")

polygon.carnivora <- readOGR(dsn = "data_0.shp")
crs(polygon.carnivora) #4326

#intersect를 계산한다.
library(rgeos)
intsct.carnivora <- gIntersects(polygon.carnivora, terrestrial.grid, byid=TRUE)
intsct.carnivora<-intsct.carnivora+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.carnivora)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.carnivora)<-paste0("carnivora_",polygon.carnivora@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.carnivora))   #296
length(unique(colnames(intsct.carnivora))) #95

dim(intsct.carnivora)
head(intsct.carnivora)

#unique만 계수할 수 있도록 정리.
intsct.carnivora.final<-intsct.carnivora[ ,1:length(unique(colnames(intsct.carnivora)))]
for(i in 1:length(unique(colnames(intsct.carnivora)))){ 
  if(sum(colnames(intsct.carnivora)==unique(colnames(intsct.carnivora))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.carnivora.final[,i]<-intsct.carnivora[,colnames(intsct.carnivora)==unique(colnames(intsct.carnivora))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.carnivora.final[,i]<-apply(intsct.carnivora[,colnames(intsct.carnivora)==unique(colnames(intsct.carnivora))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.carnivora.final)<-unique(colnames(intsct.carnivora))

length(colnames(intsct.carnivora.final)) #95으로 줄어들었음
length(unique(colnames(intsct.carnivora.final))) #동일하게 95

carnivora.book<-cbind(paste0('carnivora_',1:95) ,unique(colnames(intsct.carnivora)))


write.csv(intsct.carnivora.final,'carnivora_presences.csv' )
write.csv(carnivora.book,'carnivora_ID_species.csv' )


sum(is.na(intsct.carnivora.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.carnivora@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.carnivora@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.carnivora@data$PRESENCE, polygon.carnivora@data$SEASONAL)

length(which(polygon.carnivora@data$PRESENCE %in% c(1,2)))   # 220
length(which(polygon.carnivora@data$PRESENCE %in% c(4,5)))   # 37
length(which(polygon.carnivora@data$SEASONAL ==1))           # 295 resident인 경우만

Extant_resident<-which(polygon.carnivora@data$PRESENCE %in% c(1,2) & polygon.carnivora@data$SEASONAL ==1)    # 251 (Extant & resident)
Extinct_resident<-which(polygon.carnivora@data$PRESENCE %in% c(4,5) & polygon.carnivora@data$SEASONAL ==1)   # 13  (Extinct & resident)

#Resident만 골라서, 
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.carnivora.extant <-intsct.carnivora[,Extant_resident] 
intsct.carnivora.extinct<-intsct.carnivora[,Extinct_resident]


length(colnames(intsct.carnivora))         #296
length(unique(colnames(intsct.carnivora))) #95

length(colnames(intsct.carnivora.extant))         #219
length(unique(colnames(intsct.carnivora.extant))) #95

length(colnames(intsct.carnivora.extinct))         #37
length(unique(colnames(intsct.carnivora.extinct))) #7


dim(intsct.carnivora) # rows 635, columns 296
head(intsct.carnivora)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.carnivora.extant.final<-intsct.carnivora.extant[,1:length(unique(colnames(intsct.carnivora.extant)))]
for(i in 1:length(unique(colnames(intsct.carnivora.extant)))){
  if(sum(colnames(intsct.carnivora.extant)==unique(colnames(intsct.carnivora.extant))[i])==1){
    intsct.carnivora.extant.final[,i]<-intsct.carnivora.extant[,colnames(intsct.carnivora.extant)==unique(colnames(intsct.carnivora.extant))[i] ]
  }else{
    intsct.carnivora.extant.final[,i]<-apply(intsct.carnivora.extant[,colnames(intsct.carnivora.extant)==unique(colnames(intsct.carnivora.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.carnivora.extant.final)<-unique(colnames(intsct.carnivora.extant))

length(colnames(intsct.carnivora.extant.final)) #95
length(unique(colnames(intsct.carnivora.extant.final))) #95 둘이 같으니까 잘 나온 것.

carnivora.extant.book<-cbind(paste0('carnivora_',1:95) ,unique(colnames(intsct.carnivora.extant)))
colnames(intsct.carnivora.extant.final)<-paste0('carnivora_',1:95)

write.csv(intsct.carnivora.extant.final,'carnivora_presences_Extant.csv' )
write.csv(carnivora.extant.book,'carnivora_ID_species_Extant.csv' )


intsct.carnivora.extinct.final<-intsct.carnivora.extinct[,1:length(unique(colnames(intsct.carnivora.extinct)))]
for(i in 1:length(unique(colnames(intsct.carnivora.extinct)))){
  if(sum(colnames(intsct.carnivora.extinct)==unique(colnames(intsct.carnivora.extinct))[i])==1){
    intsct.carnivora.extinct.final[,i]<-intsct.carnivora.extinct[,colnames(intsct.carnivora.extinct)==unique(colnames(intsct.carnivora.extinct))[i] ]
  }else{
    intsct.carnivora.extinct.final[,i]<-apply(intsct.carnivora.extinct[,colnames(intsct.carnivora.extinct)==unique(colnames(intsct.carnivora.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.carnivora.extinct.final)<-unique(colnames(intsct.carnivora.extinct))

length(colnames(intsct.carnivora.extinct.final)) # 7
length(unique(colnames(intsct.carnivora.extinct.final))) # 7

carnivora.extinct.book<-cbind(paste0('carnivora_',1:7) ,unique(colnames(intsct.carnivora.extinct)))
colnames(intsct.carnivora.extinct.final)<-paste0('carnivora_',1:7)


write.csv(intsct.carnivora.extinct.final,'carnivora_presences_extinct.csv' )
write.csv(carnivora.extinct.book,'carnivora_ID_species_extinct.csv' )


sum(is.na(intsct.carnivora.final))


##############################################################
# Snakes (Colubridae) Comprehensive ###############################################
##############################################################
setwd("D:\\Environmental data\\Species richness\\Colubridae")

polygon.colubridae <- readOGR(dsn = "data_0.shp")
crs(polygon.colubridae) #4326

#intersect를 계산한다.
library(rgeos)
intsct.colubridae <- gIntersects(polygon.colubridae, terrestrial.grid, byid=TRUE)
intsct.colubridae<-intsct.colubridae+1-1 #원래 매트릭스에는 TRUE, FALSE의 logical 값이 담겨 있는데, 이걸 숫자 0, 1로 변경.

rownames(intsct.colubridae)<- terrestrial.grid@data$id #grid id 부여.
colnames(intsct.colubridae)<-paste0("colubridae_",polygon.colubridae@data$BINOMIAL) #열 이름은 해당 종명으로. 


length(colnames(intsct.colubridae))   #145
length(unique(colnames(intsct.colubridae))) #99

dim(intsct.colubridae)
head(intsct.colubridae)

#unique만 계수할 수 있도록 정리.
intsct.colubridae.final<-intsct.colubridae[ ,1:length(unique(colnames(intsct.colubridae)))]
for(i in 1:length(unique(colnames(intsct.colubridae)))){ 
  if(sum(colnames(intsct.colubridae)==unique(colnames(intsct.colubridae))[i])==1){ #만약에 전체적으로 unique 한게 하나만 나왔으면, 그대로 넣으면 되고,
    intsct.colubridae.final[,i]<-intsct.colubridae[,colnames(intsct.colubridae)==unique(colnames(intsct.colubridae))[i] ]
  }else{ #만약에 같은 종의 column이 여러개 있으면, 같은 이름인 것들의 값을 합쳐서 하나로. 숫자가 합쳐지는게 아니고 unique 한 것 하나만 남는다. 
    intsct.colubridae.final[,i]<-apply(intsct.colubridae[,colnames(intsct.colubridae)==unique(colnames(intsct.colubridae))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}
colnames(intsct.colubridae.final)<-unique(colnames(intsct.colubridae))

length(colnames(intsct.colubridae.final)) #99으로 줄어들었음
length(unique(colnames(intsct.colubridae.final))) #동일하게 99

colubridae.book<-cbind(paste0('colubridae_',1:99) ,unique(colnames(intsct.colubridae)))


write.csv(intsct.colubridae.final,'colubridae_presences.csv' )
write.csv(colubridae.book,'colubridae_ID_species.csv' )


sum(is.na(intsct.colubridae.final))


#종의 위기 상황, 그리고 seasonal 하게 이동하는 습성을 가진 경우를 고려해서 adjust 한다. 
table(polygon.colubridae@data$PRESENCE) #1 Extant, 2 Probably Extant, 3 Possibly Exant, 4 Possibly Extinct, 5 Extinct / 1 에서 5로 커질수록 멸종에 가까워짐
table(polygon.colubridae@data$SEASONAL) #1 Resident, 2 breeding season, 3 non-breeding season, 4 passage, 5 seasonal occurence uncertain
table(polygon.colubridae@data$PRESENCE, polygon.colubridae@data$SEASONAL)

length(which(polygon.colubridae@data$PRESENCE %in% c(1,2)))   # 142
length(which(polygon.colubridae@data$PRESENCE %in% c(4,5)))   # 0
length(which(polygon.colubridae@data$SEASONAL ==1))           # 145 resident인 경우만

Extant_resident<-which(polygon.colubridae@data$PRESENCE %in% c(1,2) & polygon.colubridae@data$SEASONAL ==1)    # 251 (Extant & resident)
Extinct_resident<-which(polygon.colubridae@data$PRESENCE %in% c(4,5) & polygon.colubridae@data$SEASONAL ==1)   # 13  (Extinct & resident)

#Resident만 골라서, 
#바로 위에서 which로 id index로 찾은 위치를 기반으로 나누어서 추출한다. 
intsct.colubridae.extant <-intsct.colubridae[,Extant_resident] 
intsct.colubridae.extinct<-intsct.colubridae[,Extinct_resident]


length(colnames(intsct.colubridae))         #145
length(unique(colnames(intsct.colubridae))) #99

length(colnames(intsct.colubridae.extant))         #142
length(unique(colnames(intsct.colubridae.extant))) #99

length(colnames(intsct.colubridae.extinct))         #0
length(unique(colnames(intsct.colubridae.extinct))) #0


dim(intsct.colubridae) # rows 635, columns 145
head(intsct.colubridae)

#unique한 경우만 고려하는 것. 위에 adjusted 아닐 때 나온 것과 동일.
intsct.colubridae.extant.final<-intsct.colubridae.extant[,1:length(unique(colnames(intsct.colubridae.extant)))]
for(i in 1:length(unique(colnames(intsct.colubridae.extant)))){
  if(sum(colnames(intsct.colubridae.extant)==unique(colnames(intsct.colubridae.extant))[i])==1){
    intsct.colubridae.extant.final[,i]<-intsct.colubridae.extant[,colnames(intsct.colubridae.extant)==unique(colnames(intsct.colubridae.extant))[i] ]
  }else{
    intsct.colubridae.extant.final[,i]<-apply(intsct.colubridae.extant[,colnames(intsct.colubridae.extant)==unique(colnames(intsct.colubridae.extant))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.colubridae.extant.final)<-unique(colnames(intsct.colubridae.extant))

length(colnames(intsct.colubridae.extant.final)) #99
length(unique(colnames(intsct.colubridae.extant.final))) #99 둘이 같으니까 잘 나온 것.

colubridae.extant.book<-cbind(paste0('colubridae_',1:99) ,unique(colnames(intsct.colubridae.extant)))
colnames(intsct.colubridae.extant.final)<-paste0('colubridae_',1:99)

write.csv(intsct.colubridae.extant.final,'colubridae_presences_Extant.csv' )
write.csv(colubridae.extant.book,'colubridae_ID_species_Extant.csv' )


#### 이 경우는 extinct가 0이어서 스킵####
intsct.colubridae.extinct.final<-intsct.colubridae.extinct[,1:length(unique(colnames(intsct.colubridae.extinct)))]
for(i in 1:length(unique(colnames(intsct.colubridae.extinct)))){
  if(sum(colnames(intsct.colubridae.extinct)==unique(colnames(intsct.colubridae.extinct))[i])==1){
    intsct.colubridae.extinct.final[,i]<-intsct.colubridae.extinct[,colnames(intsct.colubridae.extinct)==unique(colnames(intsct.colubridae.extinct))[i] ]
  }else{
    intsct.colubridae.extinct.final[,i]<-apply(intsct.colubridae.extinct[,colnames(intsct.colubridae.extinct)==unique(colnames(intsct.colubridae.extinct))[i] ],1,function(x){as.numeric(sum(x)>0) })
  }}

colnames(intsct.colubridae.extinct.final)<-unique(colnames(intsct.colubridae.extinct))

length(colnames(intsct.colubridae.extinct.final)) # 0
length(unique(colnames(intsct.colubridae.extinct.final))) # 0

colubridae.extinct.book<-cbind(paste0('colubridae_',1:7) ,unique(colnames(intsct.colubridae.extinct)))
colnames(intsct.colubridae.extinct.final)<-paste0('colubridae_',1:7)


write.csv(intsct.colubridae.extinct.final,'colubridae_presences_extinct.csv' )
write.csv(colubridae.extinct.book,'colubridae_ID_species_extinct.csv' )


sum(is.na(intsct.colubridae.final))









##############################################################
# Avian (Accipitriformes) Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Accipitriformes")
accipitriformes_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Accipitriformes\\accipitriforme_int_area.shp")

accipitriformes_int_area_mat <- as.data.frame(accipitriformes_int_area@data)
write.csv(accipitriformes_int_area_mat, "accipitriformes_int_area_mat.csv")

library(readr)
accipitriformes_int_area_mat <- read_csv("accipitriformes_int_area_mat.csv", 
                                         col_types = cols(...1 = col_skip()))

accipitriformes_int_area_mat_1 <- accipitriformes_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- accipitriformes_int_area_mat %>% 
  select(id, area)

area <- unique(area)

accipitriformes_int_area_mat_1 <- accipitriformes_int_area_mat_1 %>% 
  left_join(area, by = "id")

accipitriformes_int_area_mat_1 <- accipitriformes_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


accipitriformes_int_area_mat_2 <- accipitriformes_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision)

accipitriformes_int_area_mat_2[is.na(accipitriformes_int_area_mat_2)] <- 0 
accipitriformes_int_area_mat_2

write.csv(accipitriformes_int_area_mat_2, "accipitriformes_int_decision.csv")



##############################################################
#Extant ################
##############################################################

Extant_resident <- which(accipitriformes_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
accipitriformes_int_area_mat_extant <-accipitriformes_int_area_mat[Extant_resident,] 

accipitriformes_int_area_mat_extant <- as.data.frame(accipitriformes_int_area_mat_extant)
write.csv(accipitriformes_int_area_mat_extant, "accipitriformes_int_area_mat_extant.csv")

library(readr)
accipitriformes_int_area_mat_extant <- read_csv("accipitriformes_int_area_mat_extant.csv", 
                                         col_types = cols(...1 = col_skip()))

accipitriformes_int_area_mat_extant_1 <- accipitriformes_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- accipitriformes_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

accipitriformes_int_area_mat_extant_1 <- accipitriformes_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

accipitriformes_int_area_mat_extant_1 <- accipitriformes_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


accipitriformes_int_area_mat_extant_2 <- accipitriformes_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision)

accipitriformes_int_area_mat_extant_2[is.na(accipitriformes_int_area_mat_extant_2)] <- 0 
accipitriformes_int_area_mat_extant_2

write.csv(accipitriformes_int_area_mat_extant_2, "accipitriformes_int_decision_extant.csv")

accipitriformes.book<-cbind(unique(colnames(accipitriformes_int_area_mat_extant_2)))
write.csv(accipitriformes.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\accipitriformes_exant_list.csv")


accipitriformes_species_over10 <- read.csv("D:\\Environmental data\\Maxent\\Species_list\\accipitriformes_species_over10.csv")
accipitriformes_species_over10 <- as.vector(accipitriformes_species_over10[,2])


accipitriformes_maxent_mat <- accipitriformes_int_area_mat_extant_2[(names(accipitriformes_int_area_mat_extant_2) %in% accipitriformes_species_over10)]
accipitriformes.book<-cbind(unique(colnames(accipitriformes_maxent_mat)))

write.csv(accipitriformes.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\accipitriformes_exant_maxent_list.csv")
write.csv(accipitriformes_maxent_mat, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\accipitriformes_exant_maxent_mat.csv")


##############################################################
# Avian (Falconiformes) Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Falconiformes")
falconiformes_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Falconiformes\\falconiforme_int_area.shp")

falconiformes_int_area_mat <- as.data.frame(falconiformes_int_area@data)
write.csv(falconiformes_int_area_mat, "falconiformes_int_area_mat.csv")

library(readr)
falconiformes_int_area_mat <- read_csv("falconiformes_int_area_mat.csv", 
                                         col_types = cols(...1 = col_skip()))

falconiformes_int_area_mat_1 <- falconiformes_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- falconiformes_int_area_mat %>% 
  select(id, area)

area <- unique(area)

falconiformes_int_area_mat_1 <- falconiformes_int_area_mat_1 %>% 
  left_join(area, by = "id")

falconiformes_int_area_mat_1 <- falconiformes_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


falconiformes_int_area_mat_2 <- falconiformes_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

falconiformes_int_area_mat_2[is.na(falconiformes_int_area_mat_2)] <- 0 
falconiformes_int_area_mat_2

write.csv(falconiformes_int_area_mat_2, "falconiformes_int_decision.csv")

##############################################################
#Extant ################
##############################################################

Extant_resident <- which(falconiformes_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
falconiformes_int_area_mat_extant <-falconiformes_int_area_mat[Extant_resident,] 

falconiformes_int_area_mat_extant <- as.data.frame(falconiformes_int_area_mat_extant)
write.csv(falconiformes_int_area_mat_extant, "falconiformes_int_area_mat_extant.csv")

library(readr)
falconiformes_int_area_mat_extant <- read_csv("falconiformes_int_area_mat_extant.csv", 
                                                col_types = cols(...1 = col_skip()))

falconiformes_int_area_mat_extant_1 <- falconiformes_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- falconiformes_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

falconiformes_int_area_mat_extant_1 <- falconiformes_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

falconiformes_int_area_mat_extant_1 <- falconiformes_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


falconiformes_int_area_mat_extant_2 <- falconiformes_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

falconiformes_int_area_mat_extant_2[is.na(falconiformes_int_area_mat_extant_2)] <- 0 
falconiformes_int_area_mat_extant_2

write.csv(falconiformes_int_area_mat_extant_2, "falconiformes_int_decision_extant.csv")



##############################################################
# Avian (Strigiformes) Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Strigiformes")
strigiformes_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Strigiformes\\strigiforme_int_area.shp")

strigiformes_int_area_mat <- as.data.frame(strigiformes_int_area@data)
write.csv(strigiformes_int_area_mat, "strigiformes_int_area_mat.csv")

library(readr)
strigiformes_int_area_mat <- read_csv("strigiformes_int_area_mat.csv", 
                                       col_types = cols(...1 = col_skip()))

strigiformes_int_area_mat_1 <- strigiformes_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- strigiformes_int_area_mat %>% 
  select(id, area)

area <- unique(area)

strigiformes_int_area_mat_1 <- strigiformes_int_area_mat_1 %>% 
  left_join(area, by = "id")

strigiformes_int_area_mat_1 <- strigiformes_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


strigiformes_int_area_mat_2 <- strigiformes_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

strigiformes_int_area_mat_2[is.na(strigiformes_int_area_mat_2)] <- 0 
strigiformes_int_area_mat_2

write.csv(strigiformes_int_area_mat_2, "strigiformes_int_decision.csv")

##############################################################
#Extant ################
##############################################################

Extant_resident <- which(strigiformes_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
strigiformes_int_area_mat_extant <-strigiformes_int_area_mat[Extant_resident,] 

strigiformes_int_area_mat_extant <- as.data.frame(strigiformes_int_area_mat_extant)
write.csv(strigiformes_int_area_mat_extant, "strigiformes_int_area_mat_extant.csv")

library(readr)
strigiformes_int_area_mat_extant <- read_csv("strigiformes_int_area_mat_extant.csv", 
                                              col_types = cols(...1 = col_skip()))

strigiformes_int_area_mat_extant_1 <- strigiformes_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- strigiformes_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

strigiformes_int_area_mat_extant_1 <- strigiformes_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

strigiformes_int_area_mat_extant_1 <- strigiformes_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


strigiformes_int_area_mat_extant_2 <- strigiformes_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

strigiformes_int_area_mat_extant_2[is.na(strigiformes_int_area_mat_extant_2)] <- 0 
strigiformes_int_area_mat_extant_2

write.csv(strigiformes_int_area_mat_extant_2, "strigiformes_int_decision_extant.csv")



strigiformes_species_over10 <- read.csv("D:\\Environmental data\\Maxent\\Species_list\\strigiformes_species_over10.csv")
strigiformes_species_over10 <- as.vector(strigiformes_species_over10[,2])


strigiformes_maxent_mat <- strigiformes_int_area_mat_extant_2[(names(strigiformes_int_area_mat_extant_2) %in% strigiformes_species_over10)]
strigiformes.book<-cbind(unique(colnames(strigiformes_maxent_mat)))

write.csv(strigiformes.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\strigiformes_exant_maxent_list.csv")
write.csv(strigiformes_maxent_mat, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\strigiformes_exant_maxent_mat.csv")



##############################################################
# Chiroptera Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Chiroptera")
chiroptera_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Chiroptera\\chiroptera_int_area.shp")

chiroptera_int_area_mat <- as.data.frame(chiroptera_int_area@data)
write.csv(chiroptera_int_area_mat, "chiroptera_int_area_mat.csv")

library(readr)
chiroptera_int_area_mat <- read_csv("chiroptera_int_area_mat.csv", 
                                      col_types = cols(...1 = col_skip()))

chiroptera_int_area_mat_1 <- chiroptera_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- chiroptera_int_area_mat %>% 
  select(id, area)

area <- unique(area)

chiroptera_int_area_mat_1 <- chiroptera_int_area_mat_1 %>% 
  left_join(area, by = "id")

chiroptera_int_area_mat_1 <- chiroptera_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


chiroptera_int_area_mat_2 <- chiroptera_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

chiroptera_int_area_mat_2[is.na(chiroptera_int_area_mat_2)] <- 0 
chiroptera_int_area_mat_2

write.csv(chiroptera_int_area_mat_2, "chiroptera_int_decision.csv")

##############################################################
#Extant ################
##############################################################

Extant_resident <- which(chiroptera_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
chiroptera_int_area_mat_extant <-chiroptera_int_area_mat[Extant_resident,] 

chiroptera_int_area_mat_extant <- as.data.frame(chiroptera_int_area_mat_extant)
write.csv(chiroptera_int_area_mat_extant, "chiroptera_int_area_mat_extant.csv")

library(readr)
chiroptera_int_area_mat_extant <- read_csv("chiroptera_int_area_mat_extant.csv", 
                                             col_types = cols(...1 = col_skip()))

chiroptera_int_area_mat_extant_1 <- chiroptera_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- chiroptera_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

chiroptera_int_area_mat_extant_1 <- chiroptera_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

chiroptera_int_area_mat_extant_1 <- chiroptera_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


chiroptera_int_area_mat_extant_2 <- chiroptera_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

chiroptera_int_area_mat_extant_2[is.na(chiroptera_int_area_mat_extant_2)] <- 0 
chiroptera_int_area_mat_extant_2

write.csv(chiroptera_int_area_mat_extant_2, "chiroptera_int_decision_extant.csv")



chiroptera_species_over10 <- read.csv("D:\\Environmental data\\Maxent\\Species_list\\chiroptera_species_over10.csv")
chiroptera_species_over10 <- as.vector(chiroptera_species_over10[,2])


chiroptera_maxent_mat <- chiroptera_int_area_mat_extant_2[(names(chiroptera_int_area_mat_extant_2) %in% chiroptera_species_over10)]
chiroptera.book<-cbind(unique(colnames(chiroptera_maxent_mat)))

write.csv(chiroptera.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\chiroptera_exant_maxent_list.csv")
write.csv(chiroptera_maxent_mat, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\chiroptera_exant_maxent_mat.csv")




##############################################################
# Carnivora Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Carnivora")
carnivora_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Carnivora\\carnivora_int_area.shp")

carnivora_int_area_mat <- as.data.frame(carnivora_int_area@data)
write.csv(carnivora_int_area_mat, "carnivora_int_area_mat.csv")

library(readr)
carnivora_int_area_mat <- read_csv("carnivora_int_area_mat.csv", 
                                    col_types = cols(...1 = col_skip()))

carnivora_int_area_mat_1 <- carnivora_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- carnivora_int_area_mat %>% 
  select(id, area)

area <- unique(area)

carnivora_int_area_mat_1 <- carnivora_int_area_mat_1 %>% 
  left_join(area, by = "id")

carnivora_int_area_mat_1 <- carnivora_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


carnivora_int_area_mat_2 <- carnivora_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

carnivora_int_area_mat_2[is.na(carnivora_int_area_mat_2)] <- 0 
carnivora_int_area_mat_2

write.csv(carnivora_int_area_mat_2, "carnivora_int_decision.csv")

##############################################################
#Extant ################
##############################################################

Extant_resident <- which(carnivora_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
carnivora_int_area_mat_extant <-carnivora_int_area_mat[Extant_resident,] 

carnivora_int_area_mat_extant <- as.data.frame(carnivora_int_area_mat_extant)
write.csv(carnivora_int_area_mat_extant, "carnivora_int_area_mat_extant.csv")

library(readr)
carnivora_int_area_mat_extant <- read_csv("carnivora_int_area_mat_extant.csv", 
                                           col_types = cols(...1 = col_skip()))

carnivora_int_area_mat_extant_1 <- carnivora_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- carnivora_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

carnivora_int_area_mat_extant_1 <- carnivora_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

carnivora_int_area_mat_extant_1 <- carnivora_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


carnivora_int_area_mat_extant_2 <- carnivora_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

carnivora_int_area_mat_extant_2[is.na(carnivora_int_area_mat_extant_2)] <- 0 
carnivora_int_area_mat_extant_2

write.csv(carnivora_int_area_mat_extant_2, "carnivora_int_decision_extant.csv")




carnivora_species_over10 <- read.csv("D:\\Environmental data\\Maxent\\Species_list\\carnivora_species_over10.csv")
carnivora_species_over10 <- as.vector(carnivora_species_over10[,2])


carnivora_maxent_mat <- carnivora_int_area_mat_extant_2[(names(carnivora_int_area_mat_extant_2) %in% carnivora_species_over10)]
carnivora.book<-cbind(unique(colnames(carnivora_maxent_mat)))

write.csv(carnivora.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\carnivora_exant_maxent_list.csv")
write.csv(carnivora_maxent_mat, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\carnivora_exant_maxent_mat.csv")



##############################################################
# Colubridae Comprehensive
# Area 계산해서 0.5 이상만 ###############################################
##############################################################
library(tidyverse)
setwd("D:\\Environmental data\\Species richness\\Intersection_area\\Colubridae")
colubridae_int_area <- readOGR(dsn = "D:\\Environmental data\\Species richness\\Intersection_area\\Colubridae\\colubridae_int_area.shp")

colubridae_int_area_mat <- as.data.frame(colubridae_int_area@data)
write.csv(colubridae_int_area_mat, "colubridae_int_area_mat.csv")

library(readr)
colubridae_int_area_mat <- read_csv("colubridae_int_area_mat.csv", 
                                   col_types = cols(...1 = col_skip()))

colubridae_int_area_mat_1 <- colubridae_int_area_mat %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- colubridae_int_area_mat %>% 
  select(id, area)

area <- unique(area)

colubridae_int_area_mat_1 <- colubridae_int_area_mat_1 %>% 
  left_join(area, by = "id")

colubridae_int_area_mat_1 <- colubridae_int_area_mat_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


colubridae_int_area_mat_2 <- colubridae_int_area_mat_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

colubridae_int_area_mat_2[is.na(colubridae_int_area_mat_2)] <- 0 
colubridae_int_area_mat_2

write.csv(colubridae_int_area_mat_2, "colubridae_int_decision.csv")

##############################################################
#Extant ################
##############################################################

Extant_resident <- which(colubridae_int_area_mat$LEGEND %in% "Extant (resident)")   # 251 (Extant & resident)
colubridae_int_area_mat_extant <-colubridae_int_area_mat[Extant_resident,] 

colubridae_int_area_mat_extant <- as.data.frame(colubridae_int_area_mat_extant)
write.csv(colubridae_int_area_mat_extant, "colubridae_int_area_mat_extant.csv")

library(readr)
colubridae_int_area_mat_extant <- read_csv("colubridae_int_area_mat_extant.csv", 
                                          col_types = cols(...1 = col_skip()))

colubridae_int_area_mat_extant_1 <- colubridae_int_area_mat_extant %>%
  group_by(id, BINOMIAL) %>% 
  summarise(int_area_1 = sum(int_area))

area <- colubridae_int_area_mat_extant %>% 
  select(id, area)

area <- unique(area)

colubridae_int_area_mat_extant_1 <- colubridae_int_area_mat_extant_1 %>% 
  left_join(area, by = "id")

colubridae_int_area_mat_extant_1 <- colubridae_int_area_mat_extant_1 %>% 
  mutate(proportion = int_area_1/area) %>% 
  mutate(decision = case_when(proportion > 0.5 ~ 1,
                              proportion < 0.5 ~ 0)) %>% 
  select(id, BINOMIAL, decision)


colubridae_int_area_mat_extant_2 <- colubridae_int_area_mat_extant_1 %>%
  pivot_wider(names_from = BINOMIAL, values_from = decision, values_fn = function(x) as.numeric(paste(1)))

colubridae_int_area_mat_extant_2[is.na(colubridae_int_area_mat_extant_2)] <- 0 
colubridae_int_area_mat_extant_2

write.csv(colubridae_int_area_mat_extant_2, "colubridae_int_decision_extant.csv")




colubridae_species_over10 <- read.csv("D:\\Environmental data\\Maxent\\Species_list\\colubridae_species_over10.csv")
colubridae_species_over10 <- as.vector(colubridae_species_over10[,2])


colubridae_maxent_mat <- colubridae_int_area_mat_extant_2[(names(colubridae_int_area_mat_extant_2) %in% colubridae_species_over10)]
colubridae.book<-cbind(unique(colnames(colubridae_maxent_mat)))

write.csv(colubridae.book, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\colubridae_exant_maxent_list.csv")
write.csv(colubridae_maxent_mat, "D:\\Environmental data\\Species richness\\Maxent_IUCN\\colubridae_exant_maxent_mat.csv")



######################################################
## biodiversity Total !
######################################################
setwd("D:\\Environmental data\\Species richness")



library(readr)
accipitriformes_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Accipitriformes/accipitriformes_int_decision.csv")
falconiformes_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Falconiformes/falconiformes_int_decision.csv")
strigiformes_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Strigiformes/strigiformes_int_decision.csv")
carnivora_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Carnivora/carnivora_int_decision.csv")
colubridae_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Colubridae/colubridae_int_decision.csv")
chiroptera_presences <- read_csv("D:/Environmental data/Species richness/intersection_area/Chiroptera/chiroptera_int_decision.csv")

accipitriformes_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Accipitriformes/accipitriformes_int_decision_extant.csv")
falconiformes_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Falconiformes/falconiformes_int_decision_extant.csv")
strigiformes_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Strigiformes/strigiformes_int_decision_extant.csv")
carnivora_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Carnivora/carnivora_int_decision_extant.csv")
colubridae_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Colubridae/colubridae_int_decision_extant.csv")
chiroptera_presences_Extant <- read_csv("D:/Environmental data/Species richness/intersection_area/Chiroptera/chiroptera_int_decision_extant.csv")



#각 grid 별로 겹쳐진 species 숫자를 count 한다. 
accipitriformes_presences_count <- accipitriformes_presences[,-1]
accipitriformes_presences_count$count_accipitriformes <- rowSums(accipitriformes_presences_count[,-1])

accipitriformes_count <- as.data.frame(accipitriformes_presences_count$count_accipitriformes)


strigiformes_presences_count <- strigiformes_presences[,-1] 
strigiformes_presences_count$count_strigiformes <- rowSums(strigiformes_presences_count[,-1])

strigiformes_count <- as.data.frame(strigiformes_presences_count$count_strigiformes)


carnivora_presences_count <- carnivora_presences[,-1] 
carnivora_presences_count$count_carnivora <- rowSums(carnivora_presences_count[,-1])

carnivora_count <- as.data.frame(carnivora_presences_count$count_carnivora)


colubridae_presences_count <- colubridae_presences[,-1] 
colubridae_presences_count$count_colubridae <- rowSums(colubridae_presences_count[,-1])

colubridae_count <- as.data.frame(colubridae_presences_count$count_colubridae)


chiroptera_presences_count <- chiroptera_presences[,-1] 
chiroptera_presences_count$count_chiroptera <- rowSums(chiroptera_presences_count[,-1])

chiroptera_count <- as.data.frame(chiroptera_presences_count$count_chiroptera)

##Falcon 빼버리기
presecence_count <- cbind(accipitriformes_count, 
                                  strigiformes_count, carnivora_count, colubridae_count,
                                  chiroptera_count)
colnames(presecence_count) <- c("accipitriformes_prescence", 
                                "strigiformes_prescence", "carnivora_prescence", "colubridae_prescence",
                                "chiroptera_prescence")

presecence_count




#각 grid 별로 겹쳐진 species 숫자를 count 한다. - Extant 
accipitriformes_presences_Extant_count <- accipitriformes_presences_Extant[,-1] 
accipitriformes_presences_Extant_count$count_accipitriformes <- rowSums(accipitriformes_presences_Extant_count[,-1])

accipitriformes_Extant_count <- as.data.frame(accipitriformes_presences_Extant_count$count_accipitriformes)


strigiformes_presences_Extant_count <- strigiformes_presences_Extant[,-1] 
strigiformes_presences_Extant_count$count_strigiformes <- rowSums(strigiformes_presences_Extant_count[,-1])

strigiformes_Extant_count <- as.data.frame(strigiformes_presences_Extant_count$count_strigiformes)


carnivora_presences_Extant_count <- carnivora_presences_Extant[,-1] 
carnivora_presences_Extant_count$count_carnivora <- rowSums(carnivora_presences_Extant_count[,-1])

carnivora_Extant_count <- as.data.frame(carnivora_presences_Extant_count$count_carnivora)


colubridae_presences_Extant_count<- colubridae_presences_Extant[,-1] 
colubridae_presences_Extant_count$count_colubridae <- rowSums(colubridae_presences_Extant_count[,-1])

colubridae_Extant_count <- as.data.frame(colubridae_presences_Extant_count$count_colubridae)


chiroptera_presences_Extant_count <- chiroptera_presences_Extant[,-1] 
chiroptera_presences_Extant_count$count_chiroptera <- rowSums(chiroptera_presences_Extant_count[,-1])

chiroptera_Extant_count <- as.data.frame(chiroptera_presences_Extant_count$count_chiroptera)


Extant_count <- cbind(accipitriformes_Extant_count,
                          strigiformes_Extant_count, carnivora_Extant_count, colubridae_Extant_count,
                          chiroptera_Extant_count)

colnames(Extant_count) <- c("accipitriformes_Extant", 
                                "strigiformes_Extant", "carnivora_Extant", "colubridae_Extant",
                                "chiroptera_Extant")

Extant_count





ID <- as.data.frame(accipitriformes_presences$id) 
colnames(ID) <- "ID"

Species_richness <- cbind(ID, presecence_count, Extant_count)
Species_richness

write.csv(Species_richness, file = "D:\\Environmental data\\Species richness\\Intersection_area\\Species_richness_int_final.csv")






accipitriformes_Extant_Maxent <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN/accipitriformes_exant_maxent_mat.csv")
strigiformes_Extant_Maxent <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN/strigiformes_exant_maxent_mat.csv")
carnivora_Extant_Maxent <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN/carnivora_exant_maxent_mat.csv")
colubridae_Extant_Maxent <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN/colubridae_exant_maxent_mat.csv")
chiroptera_Extant_Maxent <- read_csv("D:\\Environmental data\\Species richness\\Maxent_IUCN/chiroptera_exant_maxent_mat.csv")



#각 grid 별로 겹쳐진 species 숫자를 count 한다. - Extant_maxent 
accipitriformes_Extant_Maxent_count <- accipitriformes_Extant_Maxent[,-1] 
accipitriformes_Extant_Maxent_count$count_accipitriformes <- rowSums(accipitriformes_Extant_Maxent_count[,-1])

accipitriformes_Extant_Maxent_count <- as.data.frame(accipitriformes_Extant_Maxent_count$count_accipitriformes)


strigiformes_Extant_Maxent_count <- strigiformes_Extant_Maxent[,-1] 
strigiformes_Extant_Maxent_count$count_strigiformes <- rowSums(strigiformes_Extant_Maxent_count[,-1])

strigiformes_Extant_Maxent_count <- as.data.frame(strigiformes_Extant_Maxent_count$count_strigiformes)


carnivora_Extant_Maxent_count <- carnivora_Extant_Maxent[,-1] 
carnivora_Extant_Maxent_count$count_carnivora <- rowSums(carnivora_Extant_Maxent_count[,-1])

carnivora_Extant_Maxent_count <- as.data.frame(carnivora_Extant_Maxent_count$count_carnivora)


colubridae_Extant_Maxent_count<- colubridae_Extant_Maxent[,-1] 
colubridae_Extant_Maxent_count$count_colubridae <- rowSums(colubridae_Extant_Maxent_count[,-1])

colubridae_Extant_Maxent_count <- as.data.frame(colubridae_Extant_Maxent_count$count_colubridae)


chiroptera_Extant_Maxent_count <- chiroptera_Extant_Maxent[,-1] 
chiroptera_Extant_Maxent_count$count_chiroptera <- rowSums(chiroptera_Extant_Maxent_count[,-1])

chiroptera_Extant_Maxent_count <- as.data.frame(chiroptera_Extant_Maxent_count$count_chiroptera)


Extant_Maxent_count <- cbind(accipitriformes_Extant_Maxent_count,
                      strigiformes_Extant_Maxent_count, carnivora_Extant_Maxent_count, colubridae_Extant_Maxent_count,
                      chiroptera_Extant_Maxent_count)

colnames(Extant_Maxent_count) <- c("accipitriformes_Extant_Maxent", 
                            "strigiformes_Extant_Maxent", "carnivora_Extant_Maxent", "colubridae_Extant_Maxent",
                            "chiroptera_Extant_Maxent")

Extant_Maxent_count




ID <- as.data.frame(accipitriformes_presences$id) 
colnames(ID) <- "ID"

Species_richness_Maxent <- cbind(ID, Extant_Maxent_count)
Species_richness_Maxent

write.csv(Species_richness_Maxent, file = "D:\\Environmental data\\Species richness\\Maxent_IUCN\\Species_richness_Maxent.csv")





############################################################################################################################
## For raster ###############################################
############################################################################################################################


#############################################################################################################
## Global elevation 
############################################################################################################
# process 1: clipping the elevation tiff by world gird, 
# process 2: averaging by grid cell extent 
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()

library(raster)

setwd("D:\\Environmental data\\SRTM_elevation")

raw.elevation <- raster("Merged.tif")
plot(raw.elevation)

#######
elevation.clipped <- mask(raw.elevation, terrestrial.grid)
plot(elevation.clipped)

writeRaster(elevation.clipped, "clipped_elevation.tif", format="GTiff", overwrite=TRUE)
terrestrial.grid@data
#######



a<-Sys.time()
global.elevation.mat <- 
  foreach(j = 1:nrow(terrestrial.grid@data),.combine = 'rbind',.packages = c("raster"),.errorhandling = 'pass')%dopar%{
  cropped.grid<-crop(raw.elevation, extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
  global.elevation<-mean(getValues(cropped.grid),na.rm=T)
  return(global.elevation)
}
b<-Sys.time()
b-a

global.elevation.mat <- as.data.frame(global.elevation.mat)
global.elevation <- cbind(ID, global.elevation.mat)

write.csv(global.elevation,"global_elevation.csv")

head(global.elevation)
nrow(global.elevation)
sum(is.na(global.elevation))



#############################################################################################################
## Tree cover 2000
############################################################################################################
library(raster)
treecover <- raster("D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\cover\\Merged.tif")
treeloss <- raster("D:\\Environmental data\\Forest_change\\Forest_loss\\loss\\Merged.tif")
treegain <- raster("D:\\Environmental data\\Forest_change\\Forest_gain\\gain\\Merged.tif")



extent.mat<-matrix(NA,ncol=nrow(terrestrial.grid@data),nrow=1)   # it takes  about 4 hours in lab computers
for(i in 1){for(j in 1:nrow(terrestrial.grid@data)){
  extent.mat[i,j] <-
    xmax(extent(treecover.raster.list[[i]]))>=  terrestrial.grid@data$left[j] &
    xmin(extent(treecover.raster.list[[i]]))<=  terrestrial.grid@data$right[j] &
    ymax(extent(treecover.raster.list[[i]]))>=  terrestrial.grid@data$top[j] &
    ymin(extent(treecover.raster.list[[i]]))<=  terrestrial.grid@data$bottom[j]
}}



treecover_data<-NA

a<-Sys.time() #treecover가 75%이상인 경우만.
for(i in 1:ncol(extent.mat)){
  if(apply(extent.mat,2,sum)[i]==1){
    treecover_data[i]<-sum(getValues(crop(treecover_raster_list[[which(extent.mat[,i]==1)]],extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))>75)
  }else{
    treecover_data[i]<-0  
  }}
b<-Sys.time()
b-a # 6 hours

head(treecover_data)
summary(treecover_data)
sum(is.na(treecover_data))

treecover_data <- as.data.frame(treecover_data)
colnames(treecover_data) <- "treecover"

treecover_data <- cbind(ID, treecover_data)

treecover_data<- apply(treecover_data,2,as.character)

write.csv(treecover_data,"D:\\Environmental data\\Forest_change\\Tree_canopy_cover\\treecover.csv")


############################ 참고만
a<-Sys.time()
treecover.mat <- 
  foreach(j = 1:nrow(terrestrial.grid@data),.combine = 'rbind',.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(treecover_list, extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    treecover<-mean(getValues(cropped.grid),na.rm=T)
    return(treecover)
  }
b<-Sys.time()
b-a
##############
a<-Sys.time()
pop.mat<- foreach(i = 1:5,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(pop.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    pop.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(pop.temp)
  }
b<-Sys.time()
b-a
################################


treecover_raster_list[[which(extent.mat[,2]==1)]]

############################## for treeloss
sum(getValues(crop(treecover, extent(terrestrial.grid@data[1,"left"],terrestrial.grid@data[1,"right"],terrestrial.grid@data[1,"bottom"],terrestrial.grid@data[1,"top"]))))
sum(getValues(crop(treecover,extent(terrestrial.grid@data[1,"left"],terrestrial.grid@data[1,"right"],terrestrial.grid@data[1,"bottom"],terrestrial.grid@data[1,"top"])))>75)
getValues(crop(treegain,extent(terrestrial.grid@data[1,"left"],terrestrial.grid@data[1,"right"],terrestrial.grid@data[1,"bottom"],terrestrial.grid@data[1,"top"])))




treeloss.mat<-matrix(NA,ncol=20,nrow=nrow(terrestrial.grid@data))
dim(treeloss.mat)

#이거는 75퍼센트 기준으로 cover를 0, 1로 나누고,
#거기에다가 lossyear의 year 값을 곱하니까. 그래서 0인 경우, 다시 말해서 원래도 75퍼센트 안되던 곳은 따지지 않고
#75퍼센트 이상이어서 coever에서 계수가 되었던 영역 중에 loss 생긴 지역을 따지게 된다. 
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
    treecover.temp <- getValues(crop(treecover, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"]))) > 75
    treeloss.temp <- getValues(crop(treeloss, extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))
    treecover_treeloss.temp<-treecover.temp*treeloss.temp
    
    treeloss.mat[i,1]<-sum(treecover_treeloss.temp==1)
    treeloss.mat[i,2]<-sum(treecover_treeloss.temp==2)
    treeloss.mat[i,3]<-sum(treecover_treeloss.temp==3)
    treeloss.mat[i,4]<-sum(treecover_treeloss.temp==4)
    treeloss.mat[i,5]<-sum(treecover_treeloss.temp==5)
    treeloss.mat[i,6]<-sum(treecover_treeloss.temp==6)
    treeloss.mat[i,7]<-sum(treecover_treeloss.temp==7)
    treeloss.mat[i,8]<-sum(treecover_treeloss.temp==8)
    treeloss.mat[i,9]<-sum(treecover_treeloss.temp==9)
    treeloss.mat[i,10]<-sum(treecover_treeloss.temp==10)
    treeloss.mat[i,11]<-sum(treecover_treeloss.temp==11)
    treeloss.mat[i,12]<-sum(treecover_treeloss.temp==12)
    treeloss.mat[i,13]<-sum(treecover_treeloss.temp==13)
    treeloss.mat[i,14]<-sum(treecover_treeloss.temp==14)
    treeloss.mat[i,15]<-sum(treecover_treeloss.temp==15)
    treeloss.mat[i,16]<-sum(treecover_treeloss.temp==16)
    treeloss.mat[i,17]<-sum(treecover_treeloss.temp==17)
    treeloss.mat[i,18]<-sum(treecover_treeloss.temp==18)
    treeloss.mat[i,19]<-sum(treecover_treeloss.temp==19)
    treeloss.mat[i,20]<-sum(treecover_treeloss.temp==20)

    
  }
b<-Sys.time()
b-a  # 14 hours

dim(treeloss.mat)
apply(treeloss.mat,2,summary)
sum(is.na(treeloss.mat))


loss_data <- as.data.frame(treeloss.mat)
colnames(loss_data)<-paste0("Y",2001:2020)

loss_data <- cbind(ID, loss_data)

loss_data <- apply(loss_data,2,as.character)

write.csv(loss_data,"D:\\Environmental data\\Forest_change\\Forest_loss/treeloss.csv")


## for treegain


gain.raster.list<-list()
for(i in 1:504){
  gain.raster.list[[i]]<-raster(gfc.gain.list[i])
}



gain.mat<-matrix(NA,ncol=1,nrow=nrow(terrestrial.grid@data))

# 75퍼센트이상의 cover 있는지 유무 0, 1 / gain 되었는지 유무 0, 1 그래서 곱하면 0 또는 1임. 
#그러므로 여기서 최종적으로 gain.mat 되는건 75퍼센트 이상의 cover가 있었으면서 동시에 gain 된 경우였다.
### 그러나 좀 코드를 바꿔서, 기존의 treecover에서 기준으로 삼았던 75% 이하의 지역들이 gain이 된 경우를 count하는 것이 맞지 않는가.
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
    treecover.temp<-getValues(crop(treecover,extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"]))) < 75
    gain.temp<-getValues(crop(treegain,extent(terrestrial.grid@data[i,"left"],terrestrial.grid@data[i,"right"],terrestrial.grid@data[i,"bottom"],terrestrial.grid@data[i,"top"])))
    treecover_gain.temp<-treecover.temp*gain.temp
    
    gain.mat[i]<-sum(treecover_gain.temp == 1)
  }
b<-Sys.time()
b-a # 


gain_data <- as.data.frame(gain.mat)
colnames(gain_data) <- "treegain"

gain_data <- cbind(ID, gain_data)

gain_data <- apply(gain_data,2,as.character)

write.csv(gain_data,"D:\\Environmental data\\Forest_change\\Forest_gain\\treegain.csv")




######################################################
## world climate (temperature )
######################################################
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()

library(raster)

temp.list <- list.files(path="D:\\Environmental data\\Temperature", pattern = "tif$", full.names = TRUE)
temp.stack <- raster::stack(temp.list)
slot(temp.stack,"z")<-list(as.Date(c("2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01","2000-06-01",
                                     "2000-07-01","2000-08-01","2000-09-01","2000-10-01","2000-11-01","2000-12-01")))

preci.list <- list.files(path="D:\\Environmental data\\Precipitation", pattern = "tif$", full.names = TRUE)
preci.stack <- raster::stack(preci.list)
slot(preci.stack,"z")<-list(as.Date(c("2000-01-01","2000-02-01","2000-03-01","2000-04-01","2000-05-01","2000-06-01",
                                      "2000-07-01","2000-08-01","2000-09-01","2000-10-01","2000-11-01","2000-12-01")))

dim(temp.stack$wc2.1_30s_tavg_01)
dim(temp.stack$wc2.1_30s_tavg_02)
dim(temp.stack$wc2.1_30s_tavg_03)
dim(temp.stack$wc2.1_30s_tavg_04)
dim(temp.stack$wc2.1_30s_tavg_05)
dim(temp.stack[[1]])

names(terrestrial.grid@data)


a<-Sys.time()
global.temp.mat<- foreach(i = 1:12,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(temp.stack[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(global.temp)
  }
b<-Sys.time()
b-a

global.temp <- as.data.frame(global.temp.mat)
colnames(global.temp) <- c("temp_2000-01-01","temp_2000-02-01","temp_2000-03-01","temp_2000-04-01","temp_2000-05-01","temp_2000-06-01",
                           "temp_2000-07-01","temp_2000-08-01","temp_2000-09-01","temp_2000-10-01","temp_2000-11-01","temp_2000-12-01")
global.temp <- cbind(ID, global.temp)

global.temp <- apply(global.temp,2,as.character)

write.csv(global.temp,"D:\\Environmental data\\Temperature_1\\global_temp.csv")


a<-Sys.time()
global.preci.mat<- foreach(i = 1:12,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(preci.stack[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    global.preci<-mean(getValues(cropped.grid),na.rm=T)
    return(global.preci)
  }
b<-Sys.time()
b-a

global.preci <- as.data.frame(global.preci.mat)
colnames(global.preci) <- c("preci_2000-01-01","preci_2000-02-01","preci_2000-03-01","preci_2000-04-01","preci_2000-05-01","preci_2000-06-01",
                           "preci_2000-07-01","preci_2000-08-01","preci_2000-09-01","preci_2000-10-01","preci_2000-11-01","preci_2000-12-01")
global.preci <- cbind(ID, global.preci)

global.preci <- apply(global.preci,2,as.character)

write.csv(global.preci,"D:\\Environmental data\\Precipitation_1\\global_preci.csv")


#############################################################################################################
## population  
############################################################################################################
library(doParallel)
cl<-makeCluster(2)
registerDoParallel(cl)
detectCores()
registerDoParallel(cores=30)
getDoParWorkers()


library(raster)

pop.list <- list.files(path="D:\\Environmental data\\Worldpop", pattern = "tif$", full.names = TRUE)


pop.list.raster<-list()
for(i in 1:21){
  pop.list.raster[[i]]<-raster(pop.list[i])
}

crs(terrestrial.grid)
crs(pop.list.raster[[1]])
extent(terrestrial.grid)
extent(pop.list.raster[[5]])
plot(pop.list.raster[[5]])

pop.list.raster[[1]]
pop.list.raster[[2]]
pop.list.raster[[3]]
pop.list.raster[[4]]
pop.list.raster[[5]]



b<-Sys.time()
b-a # 1.4 hours

a<-Sys.time()
pop.mat<- foreach(i = 1:21,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(pop.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    pop.temp<-sum(getValues(cropped.grid),na.rm=T)
    return(pop.temp)
  }
b<-Sys.time()
b-a


global_pop_total <- as.data.frame(pop.mat)
colnames(global_pop_total) <- c("pop_2000","pop_2001","pop_2002","pop_2003","pop_2004",
                          "pop_2005","pop_2006","pop_2007","pop_2008","pop_2009","pop_2010",
                          "pop_2011","pop_2012","pop_2013","pop_2014","pop_2015","pop_2016",
                          "pop_2017","pop_2018","pop_2019","pop_2020")
global_pop_total <- cbind(ID, global_pop_total)

round(global_pop_total,2) <- apply(global_pop_total,2,  as.numeric)
global_pop_total<- round(global_pop_total,2)

write.csv(global_pop_total,"D:\\Environmental data\\Worldpop\\global_pop_total.csv")







#################################################################################################################

#############################################################################################################
## GDP / HDI  
############################################################################################################

library(raster)

GDP.list <- list.files(path="D:\\Environmental data\\GDP\\GDP_1", pattern = "tif$", full.names = TRUE)
raster(GDP.list)

GDP.list.raster <-list()
for(i in 1:26){
  GDP.list.raster[[i]]<-raster(GDP.list[1],band=i)
}

crs(terrestrial.grid)
crs(GDP.list.raster[[1]])
extent(terrestrial.grid)
extent(GDP.list.raster[[5]])
plot(GDP.list.raster[[5]])

GDP.list.raster[[1]]
GDP.list.raster[[26]]



b<-Sys.time()
b-a # 1.4 hours

a<-Sys.time()
GDP.mat<- foreach(i = 1:26,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(GDP.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    GDP.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(GDP.temp)
  }
b<-Sys.time()
b-a


global.GDP <- as.data.frame(GDP.mat)
colnames(global.GDP) <- c("GDP_1991","GDP_1992","GDP_1993","GDP_1994","GDP_1995", "GDP_1996", "GDP_1997",
                          "GDP_1998", "GDP_1999", "GDP_2000", "GDP_2001", "GDP_2002", "GDP_2003", "GDP_2004",
                          "GDP_2005", "GDP_2006", "GDP_2007", "GDP_2008", "GDP_2009", "GDP_2010", "GDP_2011",
                          "GDP_2012", "GDP_2013", "GDP_2014", "GDP_2015")
global.GDP <- cbind(ID, global.GDP)

global.GDP <- apply(global.GDP,2,as.character)

write.csv(global.GDP,"D:\\Environmental data\\GDP\\GDP_1\\global_GDP.csv")



### HDI

HDI.list <- list.files(path="D:\\Environmental data\\GDP\\HDI_1", pattern = "tif$", full.names = TRUE)


HDI.list.raster <-list()
for(i in 1:26){
  HDI.list.raster[[i]]<-raster(HDI.list[1],band=i)
}

crs(terrestrial.grid)
crs(HDI.list.raster[[1]])
extent(terrestrial.grid)
extent(HDI.list.raster[[5]])
plot(HDI.list.raster[[5]])

HDI.list.raster[[1]]
HDI.list.raster[[26]]



b<-Sys.time()
b-a # 1.4 hours

a<-Sys.time()
HDI.mat<- foreach(i = 1:26,.combine = 'cbind',.multicombine = T,.packages = c("raster"),.errorhandling = 'pass')%:%
  foreach(j = 1:nrow(terrestrial.grid@data),.packages = c("raster"),.errorhandling = 'pass')%dopar%{
    cropped.grid<-crop(HDI.list.raster[[i]], extent(terrestrial.grid@data$left[j],terrestrial.grid@data$right[j],terrestrial.grid@data$bottom[j],terrestrial.grid@data$top[j]))
    HDI.temp<-mean(getValues(cropped.grid),na.rm=T)
    return(HDI.temp)
  }
b<-Sys.time()
b-a # 약 5분


global.HDI <- as.data.frame(HDI.mat)
colnames(global.HDI) <- c("HDI_1991","HDI_1992","HDI_1993","HDI_1994","HDI_1995", "HDI_1996", "HDI_1997",
                          "HDI_1998", "HDI_1999", "HDI_2000", "HDI_2001", "HDI_2002", "HDI_2003", "HDI_2004",
                          "HDI_2005", "HDI_2006", "HDI_2007", "HDI_2008", "HDI_2009", "HDI_2010", "HDI_2011",
                          "HDI_2012", "HDI_2013", "HDI_2014", "HDI_2015")
global.HDI <- cbind(ID, global.HDI)

global.HDI <- apply(global.HDI,2,as.character)

write.csv(global.HDI,"D:\\Environmental data\\GDP\\HDI_1\\global_HDI.csv")







#################################################################################################################

#############################################################################################################
## human foot print  
############################################################################################################


library(raster)
hfp_2000 <- raster("D:\\Environmental data\\Human_footprint_4326\\hfp2000\\hfp2000.tif")
hfp.list <- list.files(path="D:\\Environmental data\\Human_footprint_4326", pattern = "tif$", full.names = TRUE)
plot(hfp_2000)

hfp.list.raster<-list()
for(i in 1:19){
  hfp.list.raster[[i]]<-raster(hfp.list[i])
}


crs(terrestrial.grid)
crs(hfp.list.raster[[1]]) 
extent(terrestrial.grid)
extent(hfp.list.raster[[1]])

cropped.grid<-crop(hfp.list.raster[[1]],extent(terrestrial.grid@data$left[1],terrestrial.grid@data$right[1],terrestrial.grid@data$bottom[1],terrestrial.grid@data$top[1]))
hfp.mat[i,j]<-mean(getValues(cropped.grid))

hfp.mat<-matrix(NA,ncol=19,nrow=nrow(terrestrial.grid@data))
a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data) ){for(j in 1:19){
  aa<-0
  tryCatch(crop(hfp.list.raster[[j]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i])),error=function(e){aa<<-1})
  
  if(aa ==0 ){
    cropped.grid<-crop(hfp.list.raster[[j]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
    hfp.mat[i,j]<-mean(getValues(cropped.grid),na.rm=T)
  }else{
    hfp.mat[i,j] <-0  
  }
}}
b<-Sys.time()
b-a #

hfp.data  <- as.data.frame(hfp.mat)
colnames(hfp.data ) <- c("hfp_2000", "hfp_2001", "hfp_2002", "hfp_2003", "hfp_2004",
                          "hfp_2005", "hfp_2006", "hfp_2007", "hfp_2008", "hfp_2009", "hfp_2010", "hfp_2011",
                          "hfp_2012", "hfp_2013", "hfp_2014", "hfp_2015", "hfp_2016", "hfp_2017",
                          "hfp_2018")
hfp.data <- cbind(ID, hfp.data)


write.csv(hfp.data,"D:\\Environmental data\\Human_footprint_4326/hfp.csv")



#################################################################################################################

#############################################################################################################
## Global landcover 
############################################################################################################
library(raster)

landcover.list <- list.files(path="D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6\\LC1", pattern = "tif$", full.names = TRUE)


landcover.list.raster<-list()
for(i in 1:20){
  landcover.list.raster[[i]]<-raster(landcover.list[i])
}


crs(terrestrial.grid)
crs(landcover.list.raster[[1]])
extent(terrestrial.grid)
extent(landcover.list.raster[[1]])


landcover_mat_2001 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2002 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2003 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2004 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2005 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2006 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2007 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2008 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2009 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2010 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2011 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2012 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2013 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2014 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2015 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2016 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2017 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2018 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2019 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))
landcover_mat_2020 <- matrix(NA,ncol=18,nrow=nrow(terrestrial.grid@data))




a<-Sys.time()
for(i in 1:nrow(terrestrial.grid@data)){
    cropped.grid<-crop(landcover.list.raster[[1]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
    
    
    landcover_mat_2001[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
    landcover_mat_2001[i,1]<-sum(getValues(cropped.grid)==1)
    landcover_mat_2001[i,2]<-sum(getValues(cropped.grid)==2)
    landcover_mat_2001[i,3]<-sum(getValues(cropped.grid)==3)
    landcover_mat_2001[i,4]<-sum(getValues(cropped.grid)==4)
    landcover_mat_2001[i,5]<-sum(getValues(cropped.grid)==5)
    landcover_mat_2001[i,6]<-sum(getValues(cropped.grid)==6)
    landcover_mat_2001[i,7]<-sum(getValues(cropped.grid)==7)
    landcover_mat_2001[i,8]<-sum(getValues(cropped.grid)==8)
    landcover_mat_2001[i,9]<-sum(getValues(cropped.grid)==9)
    landcover_mat_2001[i,10]<-sum(getValues(cropped.grid)==10)
    landcover_mat_2001[i,11]<-sum(getValues(cropped.grid)==11)
    landcover_mat_2001[i,12]<-sum(getValues(cropped.grid)==12)
    landcover_mat_2001[i,13]<-sum(getValues(cropped.grid)==13)
    landcover_mat_2001[i,14]<-sum(getValues(cropped.grid)==14)
    landcover_mat_2001[i,15]<-sum(getValues(cropped.grid)==15)
    landcover_mat_2001[i,16]<-sum(getValues(cropped.grid)==16)
    landcover_mat_2001[i,17]<-sum(getValues(cropped.grid)==17)

}
  
for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[2]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2002[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2002[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2002[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2002[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2002[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2002[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2002[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2002[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2002[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2002[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2002[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2002[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2002[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2002[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2002[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2002[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2002[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2002[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[3]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2003[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2003[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2003[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2003[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2003[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2003[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2003[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2003[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2003[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2003[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2003[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2003[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2003[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2003[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2003[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2003[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2003[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2003[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[5]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2005[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2005[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2005[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2005[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2005[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2005[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2005[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2005[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2005[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2005[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2005[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2005[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2005[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2005[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2005[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2005[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2005[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2005[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[4]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2004[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2004[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2004[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2004[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2004[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2004[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2004[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2004[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2004[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2004[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2004[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2004[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2004[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2004[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2004[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2004[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2004[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2004[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[6]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2006[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2006[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2006[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2006[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2006[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2006[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2006[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2006[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2006[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2006[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2006[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2006[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2006[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2006[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2006[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2006[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2006[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2006[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[7]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2007[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2007[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2007[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2007[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2007[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2007[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2007[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2007[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2007[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2007[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2007[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2007[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2007[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2007[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2007[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2007[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2007[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2007[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[8]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2008[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2008[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2008[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2008[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2008[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2008[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2008[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2008[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2008[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2008[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2008[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2008[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2008[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2008[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2008[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2008[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2008[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2008[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[9]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2009[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2009[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2009[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2009[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2009[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2009[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2009[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2009[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2009[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2009[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2009[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2009[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2009[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2009[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2009[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2009[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2009[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2009[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[10]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2010[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2010[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2010[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2010[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2010[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2010[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2010[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2010[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2010[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2010[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2010[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2010[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2010[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2010[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2010[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2010[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2010[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2010[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[11]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2011[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2011[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2011[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2011[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2011[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2011[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2011[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2011[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2011[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2011[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2011[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2011[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2011[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2011[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2011[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2011[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2011[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2011[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[12]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2012[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2012[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2012[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2012[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2012[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2012[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2012[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2012[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2012[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2012[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2012[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2012[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2012[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2012[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2012[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2012[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2012[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2012[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[13]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2013[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2013[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2013[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2013[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2013[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2013[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2013[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2013[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2013[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2013[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2013[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2013[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2013[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2013[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2013[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2013[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2013[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2013[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[14]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2014[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2014[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2014[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2014[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2014[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2014[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2014[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2014[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2014[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2014[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2014[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2014[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2014[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2014[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2014[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2014[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2014[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2014[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[15]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2015[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2015[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2015[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2015[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2015[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2015[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2015[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2015[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2015[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2015[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2015[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2015[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2015[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2015[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2015[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2015[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2015[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2015[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[16]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2016[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2016[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2016[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2016[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2016[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2016[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2016[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2016[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2016[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2016[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2016[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2016[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2016[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2016[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2016[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2016[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2016[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2016[i,17]<-sum(getValues(cropped.grid)==17)
  
}


for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[17]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2017[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2017[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2017[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2017[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2017[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2017[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2017[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2017[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2017[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2017[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2017[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2017[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2017[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2017[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2017[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2017[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2017[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2017[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[18]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2018[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2018[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2018[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2018[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2018[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2018[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2018[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2018[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2018[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2018[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2018[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2018[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2018[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2018[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2018[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2018[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2018[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2018[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[19]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2019[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2019[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2019[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2019[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2019[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2019[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2019[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2019[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2019[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2019[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2019[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2019[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2019[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2019[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2019[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2019[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2019[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2019[i,17]<-sum(getValues(cropped.grid)==17)
  
}

for(i in 1:nrow(terrestrial.grid@data)){
  cropped.grid<-crop(landcover.list.raster[[20]],extent(terrestrial.grid@data$left[i],terrestrial.grid@data$right[i],terrestrial.grid@data$bottom[i],terrestrial.grid@data$top[i]))
  
  
  landcover_mat_2020[i,18]<-sum(getValues(cropped.grid)>=1,na.rm=T)
  landcover_mat_2020[i,1]<-sum(getValues(cropped.grid)==1)
  landcover_mat_2020[i,2]<-sum(getValues(cropped.grid)==2)
  landcover_mat_2020[i,3]<-sum(getValues(cropped.grid)==3)
  landcover_mat_2020[i,4]<-sum(getValues(cropped.grid)==4)
  landcover_mat_2020[i,5]<-sum(getValues(cropped.grid)==5)
  landcover_mat_2020[i,6]<-sum(getValues(cropped.grid)==6)
  landcover_mat_2020[i,7]<-sum(getValues(cropped.grid)==7)
  landcover_mat_2020[i,8]<-sum(getValues(cropped.grid)==8)
  landcover_mat_2020[i,9]<-sum(getValues(cropped.grid)==9)
  landcover_mat_2020[i,10]<-sum(getValues(cropped.grid)==10)
  landcover_mat_2020[i,11]<-sum(getValues(cropped.grid)==11)
  landcover_mat_2020[i,12]<-sum(getValues(cropped.grid)==12)
  landcover_mat_2020[i,13]<-sum(getValues(cropped.grid)==13)
  landcover_mat_2020[i,14]<-sum(getValues(cropped.grid)==14)
  landcover_mat_2020[i,15]<-sum(getValues(cropped.grid)==15)
  landcover_mat_2020[i,16]<-sum(getValues(cropped.grid)==16)
  landcover_mat_2020[i,17]<-sum(getValues(cropped.grid)==17)
  
}

b<-Sys.time()
b-a #

library(readr)
accipitriformes_presences <- read_csv("D:/Environmental data/Species richness/Accipitriformes/accipitriformes_presences.csv")
ID <- as.data.frame(accipitriformes_presences$ID) 
colnames(ID) <- "ID"

landcover_mat_2001<-cbind(ID, landcover_mat_2001)
landcover_mat_2002<-cbind(ID, landcover_mat_2002)
landcover_mat_2003<-cbind(ID, landcover_mat_2003)
landcover_mat_2004<-cbind(ID, landcover_mat_2004)
landcover_mat_2005<-cbind(ID, landcover_mat_2005)
landcover_mat_2006<-cbind(ID, landcover_mat_2006)
landcover_mat_2007<-cbind(ID, landcover_mat_2007)
landcover_mat_2008<-cbind(ID, landcover_mat_2008)
landcover_mat_2009<-cbind(ID, landcover_mat_2009)
landcover_mat_2010<-cbind(ID, landcover_mat_2010)
landcover_mat_2011<-cbind(ID, landcover_mat_2011)
landcover_mat_2012<-cbind(ID, landcover_mat_2012)
landcover_mat_2013<-cbind(ID, landcover_mat_2013)
landcover_mat_2014<-cbind(ID, landcover_mat_2014)
landcover_mat_2015<-cbind(ID, landcover_mat_2015)
landcover_mat_2016<-cbind(ID, landcover_mat_2016)
landcover_mat_2017<-cbind(ID, landcover_mat_2017)
landcover_mat_2018<-cbind(ID, landcover_mat_2018)
landcover_mat_2019<-cbind(ID, landcover_mat_2019)
landcover_mat_2020<-cbind(ID, landcover_mat_2020)

colnames(landcover_mat_2001)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2002)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2003)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2004)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2005)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2006)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2007)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2008)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2009)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2010)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2011)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2012)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2013)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2014)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2015)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2016)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2017)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2018)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2019)<-c("ID",paste0("landcover_",1:17), "total")
colnames(landcover_mat_2020)<-c("ID",paste0("landcover_",1:17), "total")


setwd("D:\\Environmental data\\Agricultural land_use_MODIS\\LandCover_Type_Yearly_005dg_v6")
write.csv(landcover_mat_2001,"landcover_mat_2001.csv")
write.csv(landcover_mat_2002,"landcover_mat_2002.csv")
write.csv(landcover_mat_2003,"landcover_mat_2003.csv")
write.csv(landcover_mat_2004,"landcover_mat_2004.csv")
write.csv(landcover_mat_2005,"landcover_mat_2005.csv")
write.csv(landcover_mat_2006,"landcover_mat_2006.csv")
write.csv(landcover_mat_2007,"landcover_mat_2007.csv")
write.csv(landcover_mat_2008,"landcover_mat_2008.csv")
write.csv(landcover_mat_2009,"landcover_mat_2009.csv")
write.csv(landcover_mat_2010,"landcover_mat_2010.csv")
write.csv(landcover_mat_2011,"landcover_mat_2011.csv")
write.csv(landcover_mat_2012,"landcover_mat_2012.csv")
write.csv(landcover_mat_2013,"landcover_mat_2013.csv")
write.csv(landcover_mat_2014,"landcover_mat_2014.csv")
write.csv(landcover_mat_2015,"landcover_mat_2015.csv")
write.csv(landcover_mat_2016,"landcover_mat_2016.csv")
write.csv(landcover_mat_2017,"landcover_mat_2017.csv")
write.csv(landcover_mat_2018,"landcover_mat_2018.csv")
write.csv(landcover_mat_2019,"landcover_mat_2019.csv")
write.csv(landcover_mat_2020,"landcover_mat_2020.csv")





#################################################################################################################

