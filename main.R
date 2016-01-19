## Author: Wilmar van Ommeren
## Date: November 2015
# to do: remove weed from analysis

## Set working directory
setwd('E:/Mijn documenten/Wageningen/Thesis/Scripts')

## Load & install packages if required
if (!require(sp)){install.packages("sp")}
if (!require(raster)){install.packages("raster")}
if (!require(rgdal)){install.packages("rgdal")}
if (!require(gdalUtils)){install.packages("gdalUtils")}
if (!require(dismo)){install.packages("dismo")}
if (!require(class)){install.packages("class")}
if (!require(caret)){install.packages("caret")}

if (!require(rpart)){install.packages("rpart")}
if (!require(rpart.plot)){install.packages("rpart.plot")}
if (!require(SDMTools)){install.packages("SDMTools")}
if (!require(dtw)){install.packages("dtw")}
if (!require(Morpho)){install.packages("Morpho")}

## Load source scripts
source('./R/align.rasters.R')
source('./R/overlay.function.R')
source('./R/training.statistics.field.R')
source('./R/training.statistics.crop.R')
source('./R/vegetation.index.type1.R')
source('./R/vegetation.index.type2.R')
source('./R/bare.soil.mask.creation.R')
source('./R/mean.index.profiles.R')
source('./R/dynamic.time.warping.R')
source('./R/rescale.rasters.R')


## Set flight height (my flights are at 35,)20151015_180m_mask_align
fheight <-'Sougoumba2014'

## Align or load rasters
names <- substr(list.files('./Input/Sougoumba/2014mask',pattern='*Sougoumba2014.tif'),1,8)
inputrasterpaths <- list.files('./Input/Sougoumba/2014mask',pattern='*Sougoumba2014.tif',full.names=T)
outputrasterspaths <- paste('./Input/Sougoumba/radio_',fheight,'_',names,'.tif',sep='')
referencerasterpath <- inputrasterpaths[1]

alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)

## Rescale rasters
outputrasterspaths <- paste('./Output/rescaled_',names,'_',fheight,'.tif',sep='')
rescalelist<-rescale.rasters(alignlist,outputrasterspaths)

## Calculate vegetation indexes
NIR<-4 #x
RED<-3 #y
NDVI <- function(x,y){(x-y)/(x+y)}
NDVIbrick <-vegetation.index.type1(rescalelist,NDVI,'ndvi',fheight,NIR,RED)

SAVI <- function(x,y){((x-y)/(x+y+0.5))*(1+0.5)} #L<- 0.5
SAVIbrick<-vegetation.index.type1(rescalelist,SAVI,'savi',fheight,NIR,RED)

interval <- 0.005
Xfactor <- 0.08
TSAVI <- function(x,y){(s*(x-s*y-a))/(a*x+y-a*s+Xfactor*(1+s^2))}
TSAVIbrick<-vegetation.index.type2(rescalelist,TSAVI,'tsavi',fheight,NIR,RED,interval)

PVI <- function (x,y){(1/sqrt(1+s^2))*(x-s*y-a)}#http://naldc.nal.usda.gov/download/9394/PDF
PVIbrick<-vegetation.index.type2(rescalelist,PVI,'pvi',fheight,NIR,RED,interval)

WDVI <- function (x,y){x-s*y} #C = NIR soil ref/red soil ref == slope
WDVIbrick<-vegetation.index.type2(rescalelist,WDVI,'wdvi',fheight,NIR,RED,interval)

## Remove bare soil
NDVIbare_value <- 0.2
bare.soil.mask <-bare.soil.mask.creation(fheight,NDVIbrick,NDVIbare_value)
NDVIbrick <-overlay(NDVIbrick,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename=paste('./Output/ndvi',fheight,'bare.tif',sep=''))
WDVIbrick<-overlay(WDVIbrick,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename=paste('./Output/wdvi',fheight,'bare.tif',sep=''))
PVIbrick<-overlay(PVIbrick,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename=paste('./Output/pvi',fheight,'bare.tif',sep=''))
SAVIbrick<-overlay(SAVIbrick,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename=paste('./Output/savi',fheight,'bare.tif',sep=''))
TSAVIbrick<-overlay(TSAVIbrick,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename=paste('./Output/tsavi',fheight,'bare.tif',sep=''))

## Calculate mean index profiels per training area and per crop type
trainingareas <- shapefile('./Input/Sougoumba/Samanko2014_fields90perc.shp') 
colnames<-trainingareas$Crop_type
NDVIprofilelist <- mean.index.profiles(NDVIbrick,'ndvi',fheight,trainingareas,colnames,names)
NDVIprofile_field <- NDVIprofilelist[[1]]
NDVIprofile_crop <- NDVIprofilelist[[2]]

SAVIprofilelist <- mean.index.profiles(SAVIbrick,'savi',fheight,trainingareas,colnames,names)
SAVIprofile_field <- SAVIprofilelist[[1]]
SAVIprofile_crop <- SAVIprofilelist[[2]]

TSAVIprofilelist <- mean.index.profiles(TSAVIbrick,'tsavi',fheight,trainingareas,colnames,names)
TSAVIprofile_field <- TSAVIprofilelist[[1]]
TSAVIprofile_crop <- TSAVIprofilelist[[2]]

PVIprofilelist <- mean.index.profiles(PVIbrick,'pvi',fheight,trainingareas,colnames,names)
PVIprofile_field <- PVIprofilelist[[1]]
PVIprofile_crop <- PVIprofilelist[[2]]

WDVIprofilelist <- mean.index.profiles(WDVIbrick,'wdvi',fheight,trainingareas,colnames,names)
WDVIprofile_field <- WDVIprofilelist[[1]]
WDVIprofile_crop <- WDVIprofilelist[[2]]

## Create 100 random points per crop type
# Select raster cells that do not have NA values in their time series
bare.soil.withoutNA<-raster::as.matrix(bare.soil.mask)
row.has.na <- apply(bare.soil.withoutNA, 1, function(x){any(is.na(x))})
row.has.na<-ifelse(row.has.na,NA,1)
bare.soil.withoutNA<-setValues(bare.soil.mask[[1]],row.has.na)
trainingareas<-spTransform(trainingareas,CRS(projection(bare.soil.mask)))

TRAININGcotton <-SpatialPointsDataFrame(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(trainingareas,Crop_type=='Cotton')),100),CRS(projection(trainingareas))),data=data.frame(rep('cotton',100),rep(1,100)))
names(TRAININGcotton)<-c('Crop','CropNO')
TRAININGmillet <-SpatialPointsDataFrame(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(trainingareas,Crop_type=='Millet')),100),CRS(projection(trainingareas))),data=data.frame(rep('millet',100),rep(2,100)))
names(TRAININGmillet)<-c('Crop','CropNO')
TRAININGsorghum <-SpatialPointsDataFrame(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(trainingareas,Crop_type=='Sorghum')),100),CRS(projection(trainingareas))),data=data.frame(rep('sorghum',100),rep(3,100)))
names(TRAININGsorghum)<-c('Crop','CropNO')
TRAININGmaize <- SpatialPointsDataFrame(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(trainingareas,Crop_type=='Maize')),100),CRS(projection(trainingareas))),data=data.frame(rep('maize',100),rep(5,100)))
names(TRAININGmaize)<-c('Crop','CropNO')
TRAININGpeanut <- SpatialPointsDataFrame(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(trainingareas,Crop_type=='Peanut')),100),CRS(projection(trainingareas))),data=data.frame(rep('peanut',100),rep(6,100)))
names(TRAININGpeanut)<-c('Crop','CropNO')

## Create random points
set.seed(123123)
trainset<-sample(seq(1,100,1),0.8*100)
shapefile(TRAININGpeanut[trainset,],paste('./Input/PeanutsPointsTrain_',fheight,'.shp',sep=''))
shapefile(TRAININGpeanut[-trainset,],paste('./Input/PeanutsPointsTest_',fheight,'.shp',sep=''))
shapefile(TRAININGmaize[trainset,],paste('./Input/MaizePointsTrain_',fheight,'.shp',sep=''))
shapefile(TRAININGmaize[-trainset,],paste('./Input/MaizePointsTest_',fheight,'.shp',sep=''))
shapefile(TRAININGcotton[trainset,],paste('./Input/CottonPointsTrain_',fheight,'.shp',sep=''))
shapefile(TRAININGcotton[-trainset,],paste('./Input/CottonPointsTest_',fheight,'.shp',sep=''))
shapefile(TRAININGsorghum[trainset,],paste('./Input/SorghumPointsTrain_',fheight,'.shp',sep=''))
shapefile(TRAININGsorghum[-trainset,],paste('./Input/SorghumPointsTest_',fheight,'.shp',sep=''))
shapefile(TRAININGmillet[trainset,],paste('./Input/MilletPointsTrain_',fheight,'.shp',sep=''))
shapefile(TRAININGmillet[-trainset,],paste('./Input/MilletPointsTest_',fheight,'.shp',sep=''))

## KNN classification
TRAININGpoints <-rbind(shapefile(paste('./Input/MaizePointsTrain_',fheight,'.shp',sep='')),
                       shapefile(paste('./Input/PeanutsPointsTrain_',fheight,'.shp',sep='')),
                       shapefile(paste('./Input/CottonPointsTrain_',fheight,'.shp',sep='')),
                       shapefile(paste('./Input/SorghumPointsTrain_',fheight,'.shp',sep='')),
                       shapefile(paste('./Input/MilletPointsTrain_',fheight,'.shp',sep='')))
TESTpoints <-rbind(shapefile(paste('./Input/MaizePointsTest_',fheight,'.shp',sep='')),
                   shapefile(paste('./Input/PeanutsPointsTest_',fheight,'.shp',sep='')),
                   shapefile(paste('./Input/CottonPointsTest_',fheight,'.shp',sep='')),
                   shapefile(paste('./Input/SorghumPointsTest_',fheight,'.shp',sep='')),
                   shapefile(paste('./Input/MilletPointsTest_',fheight,'.shp',sep='')))

index <- PVIbrick
TRAIN <- data.frame(extract(index,TRAININGpoints),TRAININGpoints$Crop)
TEST <- data.frame(extract(index,TESTpoints),TESTpoints$Crop)
TRAINtarget<-TRAIN[,length(TRAIN[1,])]
TESTtarget<-TEST[,length(TRAIN[1,])]

prd_test_pred<-knn(train=TRAIN[,1:12],test=TEST[,1:12],cl=TRAINtarget,k=21)

## Accuracy check
confusionMatrix(prd_test_pred,TESTtarget)

## Stratification sougoumba
# Soil
soilmap <- shapefile('./Input/Sougoumba_soil.shp')
soilmap<-spTransform(soilmap,CRS(projection(bare.soil.mask)))

types <- unique(soilmap@data$Matlpartl)
traininglist<-list()
for (i in 1:length(types)){
  print(i)
  soiltype <- subset(soilmap,Matlpartl == types[i])
  traininglist[i]<-intersect(trainingareas,soiltype)
}

names(traininglist)<-types


peanutlist<-list()
maizelist<-list()
cottonlist<-list()
sorghumlist<-list()
milletlist<-list()

for (i in 1:length(traininglist)){
  print(i)
  peanut <- try(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(traininglist[[i]],Crop_type=='Peanut')),100),CRS(projection(trainingareas))))
  peanutlist[i]<-try(SpatialPointsDataFrame(peanut,data=data.frame(rep('peanut',length(peanut)),rep(6,length(peanut)))))
  
  maize <- try(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(traininglist[[i]],Crop_type=='Maize')),100),CRS(projection(trainingareas))))
  maizelist[i]<-try(SpatialPointsDataFrame(maize,data=data.frame(rep('maize',length(maize)),rep(5,length(maize)))))
  
  cotton <- try(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(traininglist[[i]],Crop_type=='Cotton')),100),CRS(projection(trainingareas))))
  cottonlist[i]<-try(SpatialPointsDataFrame(cotton,data=data.frame(rep('cotton',length(cotton)),rep(1,length(cotton)))))
  
  sorghum <- try(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(traininglist[[i]],Crop_type=='Sorghum')),100),CRS(projection(trainingareas))))
  sorghumlist[i]<-try(SpatialPointsDataFrame(sorghum,data=data.frame(rep('sorghum',length(sorghum)),rep(3,length(sorghum)))))
  
  millet <- try(SpatialPoints(randomPoints(mask(bare.soil.withoutNA[[1]],subset(traininglist[[i]],Crop_type=='Millet')),100),CRS(projection(trainingareas))))
  milletlist[i]<-try(SpatialPointsDataFrame(millet,data=data.frame(rep('millet',length(millet)),rep(2,length(millet)))))
    }

for (i in 1:length(traininglist)){
  if(class(peanutlist[[i]])=='character'){peanutlist[[i]]<-data.frame(NA,NA)}
  if(class(maizelist[[i]])=='character'){maizelist[[i]]<-data.frame(NA,NA)}
  if(class(cottonlist[[i]])=='character'){cottonlist[[i]]<-data.frame(NA,NA)}
  if(class(sorghumlist[[i]])=='character'){sorghumlist[[i]]<-data.frame(NA,NA)}
  if(class(milletlist[[i]])=='character'){milletlist[[i]]<-data.frame(NA,NA)}
  names(peanutlist[[i]])<-c('Crop','CropNO')
  names(maizelist[[i]])<-c('Crop','CropNO')
  names(cottonlist[[i]])<-c('Crop','CropNO')
  names(sorghumlist[[i]])<-c('Crop','CropNO')
  names(milletlist[[i]])<-c('Crop','CropNO')
}

peanuttrain <- list()
peanuttest <- list()
maizetrain <- list()
maizetest <- list()
cottontrain <- list()
cottontest <- list()
sorghumtrain <- list()
sorghumtest <- list()
millettrain <- list()
millettest <- list()
set.seed(123123)

for (i in 1:length(traininglist)){
  print(i)
  if (!is.na(peanutlist[[i]])){
  trainset<-sample(seq(1,length(peanutlist[[i]]),1),0.8*length(peanutlist[[i]]))
  peanuttrain[i] <- peanutlist[[i]][trainset,]
  peanuttest[i] <- peanutlist[[i]][-trainset,]
  } else {
    peanuttrain[[i]]<-peanutlist[[i]]
    peanuttest[[i]]<-peanutlist[[i]]
  }
  
  if (!is.na(maizelist[[i]])){
  trainset<-sample(seq(1,length(maizelist[[i]]),1),0.8*length(maizelist[[i]]))
  maizetrain[i] <- maizelist[[i]][trainset,]
  maizetest[i] <- maizelist[[i]][-trainset,]
  }else {
    maizetrain[[i]]<-maizelist[[i]]
    maizetest[[i]]<-maizelist[[i]]
  }
  
  if (!is.na(cottonlist[[i]])){
  trainset<-sample(seq(1,length(cottonlist[[i]]),1),0.8*length(cottonlist[[i]]))
  cottontrain[i] <- cottonlist[[i]][trainset,]
  cottontest[i] <- cottonlist[[i]][-trainset,]
  }else {
    cottontrain[[i]]<-cottonlist[[i]]
    cottontest[[i]]<-cottonlist[[i]]
  }
  
  if (!is.na(sorghumlist[[i]])){
  trainset<-sample(seq(1,length(sorghumlist[[i]]),1),0.8*length(sorghumlist[[i]]))
  sorghumtrain[i] <- sorghumlist[[i]][trainset,]
  sorghumtest[i] <- sorghumlist[[i]][-trainset,]
  }else {
    sorghumtrain[[i]]<-sorghumlist[[i]]
    sorghumtest[[i]]<-sorghumlist[[i]]
  }
  
  if (!is.na(milletlist[[i]])){
  trainset<-sample(seq(1,length(milletlist[[i]]),1),0.8*length(milletlist[[i]]))
  millettrain[i] <- milletlist[[i]][trainset,]
  millettest[i] <- milletlist[[i]][-trainset,]
  }else {
    millettrain[[i]]<-milletlist[[i]]
    millettest[[i]]<-milletlist[[i]]
  }
}


klist<-list()
for (i in 1:length(traininglist)){
  square<-sqrt(sum(ifelse(class(try(length(millettrain[[i]])))=='try-error',0,length(millettrain[[i]])),
                   ifelse(class(try(length(sorghumtrain[[i]])))=='try-error',0,length(sorghumtrain[[i]])),
                   ifelse(class(try(length(cottontrain[[i]])))=='try-error',0,length(cottontrain[[i]])),
                   ifelse(class(try(length(maizetrain[[i]])))=='try-error',0,length(maizetrain[[i]])),
                   ifelse(class(try(length(peanuttrain[[i]])))=='try-error',0,length(peanuttrain[[i]])),
                   na.rm=T))
  roundedsquare<-round(square)
  if (roundedsquare%%2==0){
    direction<-roundedsquare-square
    if (direction < 0){
      roundedsquare <- roundedsquare+1
    } else{
      roundedsquare <- roundedsquare-1
    }
  }
  klist[i]<-roundedsquare
}

## KNN classification
TRAININGpointslist<-list()
TESTpointslist<-list()
for (i in 1:length(traininglist)){
  trainingwithoutna  <-list()
  testwithoutna  <-list()
  
  if(!is.na(millettrain[[i]][,1])){
    trainingwithoutna[length(trainingwithoutna)+1]<-millettrain[[i]]
    testwithoutna[length(testwithoutna)+1]<-millettest[[i]]
  }
  if(!is.na(sorghumtrain[[i]][,1])){
    trainingwithoutna[length(trainingwithoutna)+1]<-sorghumtrain[[i]]
    testwithoutna[length(testwithoutna)+1]<-sorghumtest[[i]]
  }
  if(!is.na(cottontrain[[i]][,1])){
    trainingwithoutna[length(trainingwithoutna)+1]<-cottontrain[[i]]
    testwithoutna[length(testwithoutna)+1]<-cottontest[[i]]
  }
  if(!is.na(maizetrain[[i]][,1])){
    trainingwithoutna[length(trainingwithoutna)+1]<-maizetrain[[i]]
    testwithoutna[length(testwithoutna)+1]<-maizetest[[i]]
  }
  if(!is.na(peanuttrain[[i]][,1])){
    trainingwithoutna[length(trainingwithoutna)+1]<-peanuttrain[[i]]
    testwithoutna[length(testwithoutna)+1]<-peanuttest[[i]]
  }
  
  
  TRAININGpointslist[i] <-do.call("rbind", trainingwithoutna)
  TESTpointslist[i] <-do.call("rbind", testwithoutna)
  
}

index <- PVIbrick
for (i in 1:length(traininglist)){
  print(i)
  TRAIN <- data.frame(extract(index,TRAININGpointslist[[i]]),TRAININGpointslist[[i]]$Crop)
  TEST <- data.frame(extract(index,TESTpointslist[[i]]),TESTpointslist[[i]]$Crop)
  TRAINtarget<-TRAIN[,length(TRAIN[1,])]
  TESTtarget<-TEST[,length(TRAIN[1,])]
  
  prd_test_pred<-knn(train=TRAIN[,1:12],test=TEST[,1:12],cl=TRAINtarget,k=klist[[i]])
  print(confusionMatrix(prd_test_pred,TESTtarget))
}

plot(TRAININGpointslist[[1]])
TRAININGpointslist[[1]]$Crop
TRAININGpointslist
types
## Accuracy check
confusionMatrix(prd_test_pred,TESTtarget)









## Accuracy check
indexname<-'savi'
foldername<-'./Output/Classification'
filenames<-list.files(foldername,paste('*',indexname,fheight,'*',sep=''),full.names=T)

#remove values bigger than 4 or smaller than 1
accuracylist<-list()
for (i in 1:length(filenames)){
  print(filenames[i])
  CLASSrast<-calc(raster(filenames[i]),function(x) { x[x<1] <- NA; return(x) })
  resultpoints<-extract(CLASSrast,TESTpoints)
  print((confusionMatrix(resultpoints,TESTpoints@data$CropNO)))
}


folders<-list.files('./Input/Sougoumba/2014mask/',full.names=T)
rasterlist<-list()
for(i in 1:length(folders)){
  rasterlist[i]<-brick(folders[i])
}


for (i in 1:length(rasterlist)){
  print(i)
  writeRaster(crop(rasterlist[[i]],shapefile('./Input/Sougoumba/studyarea.shp')),paste(names[i],'SOU2014.tif',sep=''))
}



names <- substr(list.files('./Input/Sougoumba/2014',full.names=T),24,31)
inputrasterpaths <- paste('./Input/radio_',fheight,'_',names,'.tif',sep='')
outputrasterspaths <- paste('./Input/radio_',fheight,'_',names,'.tif',sep='')
referencerasterpath <- inputrasterpaths[6]




## Aggregate
index<-NDVIbrick
indexbare<-overlay(index,bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA),filename='./Output/ndvi180bare.tif')
fact2<-aggregate(indexbare, fact=2,fun=mean,filename='./Output/NDVI180fact2.tif')
fact3<-aggregate(indexbare, fact=3,fun=mean,filename='./Output/tsavi180fact3.tif')
fact5<-aggregate(indexbare, fact=5,fun=mean,filename='./Output/tsavi180fact5.tif')

library(rasclass)
library(class)
library(caret)


?knn

summary(a)
library(kknn)
m2<-kknn(layer~.,TRAIN,TEST,k=1)
summary(m2)
fit<-fitted(m2)
?fitted
table(TEST[,11],fit)
a<-raster::predict(index,m2)
names(index)
names(TRAIN)

table(TEST[,11],m1)
length(m1)
length(TEST[,1])
## Accuracy check
indexname<-'spect180'
foldername<-'./AccuracyPoints'
filenames<-list.files(foldername,paste('*',indexname,'*',sep=''),full.names=T)
testpoints<-shapefile('./Input/TestPointsCombined.shp')
filenames
accuracylist<-list()
for (i in 1:length(filenames)){
  print(filenames[i])
  resultpoints<-extract(raster(filenames[i]),testpoints)
  print((confusionMatrix(resultpoints,testpoints@data$CropNo)))

}


testpoints<-shapefile('./Input/TestPointsCombined.shp')
trainpoints<-shapefile('./Input/TrainPointsCombined.shp')
index<-alignlist[[1]]

train<-data.frame(extract(index,trainpoints))
test<-data.frame(extract(index,testpoints))
train[7]<-trainpoints@data$CropNo
test[7]<-testpoints@data$CropNo
prd_test_pred<-knn(train=train[,1:6],test=test[,1:6],cl=train[,7],k=8)
confusionMatrix(prd_test_pred,test[,7])
sqrt(80)

emptylist<-list()
for (i in 1:100){
  print(i)
  prd_test_pred<-knn(train=train[,1:10],test=test[,1:10],cl=train[,11],k=i)
  emptylist[i]<-confusionMatrix(prd_test_pred,test[,11])$overall[[1]]
}
names(emptylist)<-seq(from=1,to=100,by=1)
a<-data.frame(emptylist)
names(a)<-seq(from=1,to=100,by=1)
write.csv(x=a,file='k.csv',sep=',')
?write.csv

maxl1<-calc(mask(raster('./Indexfactor/maxl_ndvi180_fact1_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
maxl2<-calc(mask(raster('./Indexfactor/maxl_ndvi180_fact2_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
maxl3<-calc(mask(raster('./Indexfactor/maxl_ndvi180_fact3_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
maxl5<-calc(mask(raster('./Indexfactor/maxl_ndvi180_fact5_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
maxlRad<-calc(mask(raster('./Indexfactor/maxl_radio180_1015_fact1_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})

validclasses1<-rasterize(x=trainingareas,y=maxl1,'ClassNO',fun='last' ,na.rm=T)
validclasses2<-rasterize(x=trainingareas,y=maxl2,'ClassNO',fun='last' ,na.rm=T)
validclasses3<-rasterize(x=trainingareas,y=maxl3,'ClassNO',fun='last' ,na.rm=T)
validclasses5<-rasterize(x=trainingareas,y=maxl5,'ClassNO',fun='last' ,na.rm=T)

maxl1tab<-raster::as.matrix(maxl1)
maxl2tab<-raster::as.matrix(maxl2)
maxl3tab<-raster::as.matrix(maxl3)
maxl5tab<-raster::as.matrix(maxl5)
maxlRadtab<-raster::as.matrix(maxlRad)


valid1tab<-raster::as.matrix(validclasses1)
valid2tab<-raster::as.matrix(validclasses2)
valid3tab<-raster::as.matrix(validclasses3)
valid5tab<-raster::as.matrix(validclasses5)

maxl1con<-confusionMatrix(maxl1tab,valid1tab)
maxl2con<-confusionMatrix(maxl2tab,valid2tab)
maxl3con<-confusionMatrix(maxl3tab,valid3tab)
maxl5con<-confusionMatrix(maxl5tab,valid5tab)
maxlRadcon<-confusionMatrix(maxlRadtab,valid1tab)

maxl1con$table
maxl2con$table
maxl3con$table
maxl5con$table
maxlRadcon$table

## load mindist
mindist1<-calc(mask(raster('./Indexfactor/mindist_ndvi180_fact1_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
mindist2<-calc(mask(raster('./Indexfactor/mindist_ndvi180_fact2_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
mindist3<-calc(mask(raster('./Indexfactor/mindist_ndvi180_fact3_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
mindist5<-calc(mask(raster('./Indexfactor/mindist_ndvi180_fact5_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})
mindistRad<-calc(mask(raster('./Indexfactor/mindist_radio180_1015_fact1_5cl.tif'),trainingareas),function(x){x[x<1]<-NA; return(x)})

mindist1tab<-raster::as.matrix(mindist1)
mindist2tab<-raster::as.matrix(mindist2)
mindist3tab<-raster::as.matrix(mindist3)
mindist5tab<-raster::as.matrix(mindist5)
mindistRadtab<-raster::as.matrix(mindistRad)

mindist1con<-confusionMatrix(mindist1tab,valid1tab)
mindist2con<-confusionMatrix(mindist2tab,valid2tab)
mindist3con<-confusionMatrix(mindist3tab,valid3tab)
mindist5con<-confusionMatrix(mindist5tab,valid5tab)
mindistRadcon<-confusionMatrix(mindistRadtab,valid1tab)

mindist1con$table
mindist2con$table
mindist3con$table
mindist5con$table
mindistRadcon$table

trainingareas@data













#create some data
obs = c(sample(c(0,1),20,replace=TRUE),NA); obs = obs[order(obs)]
pred = runif(length(obs),0,1); pred = pred[order(pred)]

#calculate the confusion matrix
kappa(confusion.matrix(obs,pred,threshold=0.5))


## Accuracy check
library(caret)
?confusionMatrix()
validclasses<-rasterize(x=trainingareas,y=NDVIbrick[[1]],'ClassNO',fun='last' ,na.rm=T)
validclasses<-calc(validclasses,fun=function(x)ifelse(is.na(x),0,x))
VALIDmat<-raster::as.array(validclasses)
indexclasses<-raster('./Indexfactor/maxl_ndvi180_1.tif')
plot(validclasses)
plot(indexclasses)
INDEXmat<-raster::as.array(indexclasses)
comb<-data.frame(cbind(INDEXmat,VALIDmat))
true<-as.factor(comb$VALIDmat)
pred<-as.factor(comb$INDEXmat)
a<-confusionMatrix(pred,true)


projection(bare.soil.mask)
names(indexclasses)
INDEXagg<-aggregate(WDVIbrick, fact=2,fun=mean)
set.seed(123123)
classes<-rasterize(x=trainingareas,y=INDEXagg[[1]],'ClassNO',fun='last' ,na.rm=T)
INDEXmat<-raster::as.matrix(INDEXagg)
CLASSmat<-raster::as.array(classes)
DATA<-data.frame(cbind(CLASSmat,INDEXmat))
names(DATA)<-c('Class', 't1','t2','t3', 't4','t5','t6', 't7','t8','t9','t10')
trainrow<-sample(seq(1,length(DATA[,1]),1),0.8*length(DATA[,1]))
test<- DATA[trainrow,]
train<- DATA[-trainrow,]

prediction<-rpart(formula=Class~t1+t2+t3+t4+t5+t6+t7+t8+t9+t10,data=train,method='class')
summary(prediction)
rsq.rpart(prediction)

wdviangle<-overlay(raster('./Input/spectangle_wdvi.tif'),bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA))
wdvinear<-overlay(raster('./Input/mindist_wdvi.tif'),bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA))
wdvimaxl<-overlay(raster('./Input/maxlik_wdvi.tif'),bare.soil.mask,fun=function(x,y)ifelse(y==1,x,NA))

#ndvi= 0.77369 xerror =0.78554
#wdvi= 0.75998 xerror=0.78429
#pvi = 0.75998 xerror=0.78429
#savi = 0.76185 xerror=0.79115
#tsavi = 0.77494 xerror=0.79177 0.015489


# Smooth bricks with focal statistics
focalmatrix<-matrix(c(1,1,1,1,1,1,1,1,1),nrow=3)
NDVIfocal<-focal.statistics(NDVIbrick,focalmatrix,mean)
SAVIfocal<-focal.statistics(SAVIbrick,focalmatrix,mean)
TSAVIfocal<-focal.statistics(TSAVIbrick,focalmatrix,mean)
PVIfocal<-focal.statistics(PVIbrick,focalmatrix,mean)
WDVIfocal<-focal.statistics(NDVIbrick,focalmatrix,mean)

# Calculate slope per pixel with linearmodel
time<-1:nlayers(NDVIbrick)
slopefun <- function(x) {if (is.na(x[1])) { NA } else {lm(x ~ time)$coefficients[2] }}
NDVIslope <- calc(NDVIbrick, fun)

# calculate dynamic time warping distance per crop type and per cell
NDVIdtw.brick<-dynamic.time.warping(NDVIprofile_crop, NDVIbrick, 'ndvi', fheight)

classification <- dtw.brick[[1]]
class.data <- raster::as.matrix(dtw.brick)
class.func<-function(x)ifelse(min(x)>1, 'Unknown', names(which.min(x)))
test.class<-apply(class.data,1,class.func)
class.no.funct<-function(x)ifelse(x=='Sorghum1', 1,ifelse(x=='Millet', 2, ifelse(x=='Cotton', 3,ifelse(x=='Weed', 4,ifelse(x=='Unknown', 5)))))
test.class<-cbind(test.class,apply(test.class,1,class.no.funct))

class.no<-matrix(test.class)
for (i in 1:length(test.class)){
  x<-test.class[i]
  if(is.na(x)){
    class.no[i]<-NA
  }
  else if(x=='Sorghum1'){
    class.no[i]<-1}
  else if(x=='Millet'){
    class.no[i]<-2}
  else if(x=='Cotton'){
    class.no[i]<-3}
  else if(x=='Weed'){
    class.no[i]<-4}
  else if(x=='Unknown'){
    class.no[i]<-5}
}

class.raster<-setValues(classification,as.numeric(class.no))
class.raster<-mask(class.raster, NDVI_VEGbrick[[1]])

plot(NDVI_VEGbrick[[1]])
plot(class.raster,legend=F,col=rainbow(5))
legend("bottomright", legend = c("Sorghum", "Millet", "Cotton", "Weed",'Unknown'), fill = rainbow(5))

test.class[788616]
classification<-
  summary(test.class)
extent(mask)
extent(class.raster)

a<-crop(class.raster,mask)
extent(a)
spplot(a)
#################
dates<-c()
for (i in 1:NDVI_VEGbrick@file@nbands){
  year_monthday <- sub( '(?<=.{4})', '-', names[i], perl=TRUE)
  dates[i] <- sub( '(?<=.{7})', '-', year_monthday, perl=TRUE)
}
NDVIseries<- rts(NDVI_VEGbrick,dates)
plot(NDVIslope)

#### DTW practice
a <- c(1,1,1,1,1,1,1,1,1)
b <- c(2,2,2,2,2,2,2,2,2)
c <- c(1,1,1,2,2,2,3,3,3)
d <- c(1,2,NA,6,7,4,3,1,NA)
e <- c(1,2,2,2,3,3,3,4,10)
library(Morpho)
ifelse(is.na(a),NA,ifelse(is.na(d),NA,angle.calc(a,d)))
test(a,d)

angle.calc(a,e)

#'normalized' dtw binnen standaarddeviatie training areas


time<-1:9
system.time(lm(c~time)$coefficients[2])

# time<-1:nlayers(NDVIbrick)
# fun <- function(x) {if (is.na(x[1])) { NA } else {lm(x ~ time)$coefficients[2] }}
# NDVIslope <- calc(NDVIbrick, fun)

theta <- function(x,y) acos(sum(x*y) / ( sqrt(sum(x^2)) * sqrt(sum(y^2))))




?apply
rbind(c,a)
theta(c,a)
apply(rbind(c),1,theta,y=d)

a+d))

theta <- acos( sum(a*c) / ( sqrt(sum(a * a)) * sqrt(sum(c * c)) ) )

lm(a)

z<-dtw(a,b,keep.internals=T,step.pattern=typeIds)
z$distance
z$costMatrix

y<-dtw(a,c,keep.internals=T,step.pattern=typeIds)
y$distance
y$costMatrix

