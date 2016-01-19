## Author: Wilmar van Ommeren
## Date: Januari 2016

######################################################################################################################
# STEP 1: PREPARE SCRIPT #############################################################################################
######################################################################################################################

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

## Load source scripts
source('./R/align.rasters.R')
source('./R/rescale.rasters.R')
source('./R/overlay.function.list.R')
source('./R/overlay.function.checkfile.R')
source('./R/vegetation.index.type1.R')
source('./R/vegetation.index.type2.R')
source('./R/bare.soil.mask.creation.R')
source('./R/training.statistics.field.R')
source('./R/training.statistics.crop.R')
source('./R/mean.index.profiles.R')
source('./R/singlemask.R')
source('./R/create.train.test.R')
source('./R/create.random.points.R')
source('./R/round.to.first.uneven.R')

######################################################################################################################
# STEP 2: LOAD OR PREPARE DATA #######################################################################################
######################################################################################################################

## Set ID with which rasters will be loaded and saved
id <-'Sougoumba2014'

## Align or load rasters
dates <- substr(list.files('./Input/Sougoumba/SYNCED',pattern='*_output.tif$'),1,8)
inputrasterpaths <- list.files('./Input/Sougoumba/SYNCED',pattern='*_output.tif$',full.names=T)
outputrasterspaths <- paste('./Input/Sougoumba/radio_',id,'_',dates,'.tif',sep='')
referencerasterpath <- inputrasterpaths[1]
alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)

## Rescale rasters between 0 and 1
rescalefactor <- 10000
outputrasterspaths <- paste('./Input/rescaled_',id,'_',dates,'.tif',sep='')
rescalelist<-rescale.rasters(alignlist,outputrasterspaths,rescalefactor)

## Calculate vegetation indexes
NIR<-4 #x
RED<-3 #y
outputfolder <- './Output/'
NDVI <- function(x,y){(x-y)/(x+y)}
NDVIbrick <-vegetation.index.type1(rescalelist,NDVI,'ndvi',id,NIR,RED,outputfolder)

SAVI <- function(x,y){((x-y)/(x+y+0.5))*(1+0.5)} #L<- 0.5
SAVIbrick<-vegetation.index.type1(rescalelist,SAVI,'savi',id,NIR,RED,outputfolder)

interval <- 0.005
Xfactor <- 0.08
TSAVI <- function(x,y){(s*(x-s*y-a))/(a*x+y-a*s+Xfactor*(1+s^2))}
TSAVIbrick<-vegetation.index.type2(rescalelist,TSAVI,'tsavi',id,NIR,RED,interval,outputfolder)

PVI <- function (x,y){(1/sqrt(1+s^2))*(x-s*y-a)}#http://naldc.nal.usda.gov/download/9394/PDF
PVIbrick<-vegetation.index.type2(rescalelist,PVI,'pvi',id,NIR,RED,interval,outputfolder)

WDVI <- function (x,y){x-s*y} #C = NIR soil ref/red soil ref == slope
WDVIbrick<-vegetation.index.type2(rescalelist,WDVI,'wdvi',id,NIR,RED,interval,outputfolder)

######################################################################################################################
# STEP 3: PREPARE FOR CLASSIFICATION #################################################################################
######################################################################################################################

## Select the index to be used in the analysis
index<-PVIbrick
indexname<-'PVI'

## Remove bare soil
NDVIbare_value <- 0.2
bare.soil.mask <-bare.soil.mask.creation(id,NDVIbrick,NDVIbare_value,outputfolder)

# Remove bare from index
INDEXbare<-overlay.function.checkfile(index, bare.soil.mask,function(x,y)ifelse(y==1,x,NA),outputfolder,paste(indexname,id,'bare.tif',sep=''))

# Calculate mean index profiels per training area and per crop type
trainingareas <- shapefile('./Input/Sougoumba/Sougoumba2014fields90perc.shp') 
crop_types<-trainingareas$Crop_type

INDEXprofilelist <- mean.index.profiles(INDEXbare,indexname,id,trainingareas,crop_types,dates,outputfolder)
INDEXprofile_field <- INDEXprofilelist[[1]]
INDEXprofile_crop <- INDEXprofilelist[[2]]

## Remove trees from raster
treemask <- raster('./Input/Sougoumba/sougoumba2014treemask.tif')
INDEXtree<-overlay.function.checkfile(index, treemask,function(x,y)ifelse(y==1,NA,x),outputfolder,filename=paste('PVI',id,'_tree.tif',sep=''))

## Aggregate raster
INDEXtreeagg<-aggregate(INDEXtree, fact=4,fun=max)
PVIbrick4<-aggregate(PVIbrick, fact=4,fun=mean)

######################################################################################################################
# STEP 4: CLASSIFICATION WITHOUT STRATA ##############################################################################
######################################################################################################################

## Create 100 random points per crop type
# peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2
set.seed(123123)
crop_types<-unique(trainingareas$Crop_type)
crop_column_no <- 3
crop_numbers<-c(6,3,2,5,1)
samplesize<-100
trainingportion<-0.8
randompointsraster<-INDEXtree
classificationraster<-PVIbrick4

randompoints<-create.train.test(trainingareas,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion)
TRAINpoints<-randompoints[[1]]
TESTpoints<-randompoints[[2]]

## KNN classification without strata
kvalue<- round.to.first.uneven(length(TRAINpoints[[1]]))
TRAIN <- data.frame(extract(classificationraster,TRAINpoints),TRAINpoints$Crop)
TEST <- data.frame(extract(classificationraster,TESTpoints),TESTpoints$Crop)
TRAINtarget<-TRAIN[,length(TRAIN[1,])]
TESTtarget<-TEST[,length(TRAIN[1,])]

KNNclassification<-knn(train=TRAIN[,1:length(TRAIN)-1],test=TEST[,1:length(TEST)-1],cl=TRAINtarget,k=kvalue)
confusionMatrix(KNNclassification,TESTtarget)

######################################################################################################################
# STEP 5: CLASSIFICATION WITH STRATA #################################################################################
######################################################################################################################

# Random points per strata per crop type
# peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2
set.seed(123123)
crop_types<-unique(trainingareas$Crop_type)[2:5]
crop_column_no <- 3
crop_numbers<-c(3,2,5,1)
samplesize<-100
trainingportion<-0.8
strata<-spTransform(rasterToPolygons(raster('./Input/Sougoumba/Sougoumba_height_iso.tif'),dissolve=T),CRS(projection(trainingareas)))
randompointsraster<-INDEXtree
classificationraster<-PVIbrick4
return.raster=T

knn.with.strata<-function(trainingareas,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion){
  kvaluelist<<-list()
  trainlist<<-list()
  testlist<<-list()
  traintargetlist<<-list()
  for (i in 1:length(strata)){
    print (paste('Strata:',i))
    
    # Create points per strata
    trainingstrata<-intersect(trainingareas,subset(strata,get(names(strata))==i))
    randompoints<-create.train.test(trainingstrata,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion)
    TRAINpoints<-randompoints[[1]]
    TESTpoints<-randompoints[[2]]
    
    # Calculate optimal k-value
    kvalue<- round.to.first.uneven(length(randompoints[[1]]))
    print(paste('K-value: ',kvalue))
    kvaluelist[i]<-kvalue
    
    # Extract values per random point
    TRAIN <- data.frame(extract(classificationraster,TRAINpoints),TRAINpoints$Crop)
    TEST <- data.frame(extract(classificationraster,TESTpoints),TESTpoints$Crop)
    TRAINtarget<-TRAIN[,length(TRAIN[1,])]
    TESTtarget<-TEST[,length(TRAIN[1,])]
    
    if (i==1){
      traintargetlist <<- lapply('TRAINtarget', get)
      testlist <<- lapply('TEST', get)
      trainlist <<- lapply('TRAIN', get)
    } else {
      traintargetlist[[i]]<<-TRAINtarget
      testlist[[i]]<<-TEST
      trainlist[[i]]<<-TRAIN
    }
    
    # Classification and result
    KNNclassification<-knn(train=TRAIN[,1:length(TRAIN)-1],test=TEST[,1:length(TEST)-1],cl=TRAINtarget,k=kvalue)
    print(confusionMatrix(KNNclassification,TESTtarget))
  }
  
  # Create raster is return.raster = TRUE
  if (return.raster){
    print('Creating classified raster...')
    masklist <<- list()
    mask.dflist<<-list()
    mask.dfwithoutNA<<-list()
    combined.dflist<<-list()
    rasterlist<-list()
    for (j in 1:length(strata)){
      # Create raster mask per strata
      print(paste('Step 1: Splitting index raster into different strata [',j,'/',length(strata),']',sep=''))
      masklist[j]<<-mask(index,subset(strata,get(names(strata))==j))
      
      # Convert raster to dataframe
      print(paste('Step 2: Transforming strata to dataframe for calculations [',j,'/',length(strata),']',sep=''))
      mask.df<<-as.data.frame(raster::as.matrix(masklist[[j]]))
      
      if (j==1){
        mask.dflist <<- lapply('mask.df', get)
      } else {
        mask.dflist[[j]]<<-mask.df
      }
      
      # Create per dataframe a new column with the row numbers
      print(paste('Step 3: Creating a new column with row numbers [',j,'/',length(strata),']',sep=''))
      mask.dflist[[j]]['rowno']<<-matrix(seq(from=1,to=nrow(mask.dflist[[j]]),by=1),ncol=1)
      
      # Remove NAs from dataframe
      print(paste('Step 4: Removing NAs from dataframe [',j,'/',length(strata),']',sep=''))
      mask.na.df<<-na.omit(mask.dflist[[j]])
      if (j==1){
        mask.dfwithoutNA <<- lapply('mask.na.df', get)
      } else {
        mask.dfwithoutNA[[j]]<<-mask.na.df
      }
      
      # Add column with KNN klassification
      print(paste('Step 5: KNN classification on dataframe without NAs [',j,'/',length(strata),']',sep=''))
      mask.dfwithoutNA[[j]]['KNN']<<-knn(train=trainlist[[j]][,1:length(trainlist[[j]])-1],test=mask.dfwithoutNA[[j]][,1:length(mask.dfwithoutNA[[j]])-1],cl=traintargetlist[[j]],k=kvaluelist[[j]])
      
      # Merge classified dataframe without NAs with the dataframe containing all row numbers
      print(paste('Step 6: Merging dataframes with and without NAs on row numbers [',j,'/',length(strata),']',sep=''))
      combined.df<<-merge(mask.dflist[[j]],mask.dfwithoutNA[[j]],by='rowno',all=T)
      
      if (j==1){
        combined.dflist <<- lapply('combined.df', get)
      } else {
        combined.dflist[[j]]<<-combined.df
      }
    }
  }
}


combined.dflist[[1]]

knn.with.strata(trainingareas,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion)



names(trainlist[[1]][1:11])
names(mask.dfwithoutNA[[1]])
knn(train=trainlist[[j]][,1:length(trainlist[[j]])-1],test=mask.dfwithoutNA[[j]][,1:length(trainlist[[j]])-2],cl=traintargetlist[j],k=kvaluelist[j])

knn(train=trainlist[[1]][,1:length(trainlist[[1]])-1],test=mask.dfwithoutNA[[1]][,1:length(mask.dfwithoutNA[[1]])],cl=traintargetlist[[1]],k=21)

a<-trainlist[[1]][,1:length(trainlist[[1]])-1]
b<-traintargetlist[[1]]

length(b)

na.omit(mask.dflist[[2]])
kvaluelist
head(mask.dfwithoutNA[[1]])
head(trainlist[[1]][,1:length(trainlist[[1]])-1])
traintargetlist
mask.dfwithoutNA

ncol(trainlist[1])
mask.df<-as.data.frame(raster::as.matrix(masklist[[j]]))

a<-data.frame(c(1,2,3,4,5),c(45,NA,NA,43,21))
names(a)<-c('a','b')
c<-na.omit(a)
c[3]<-c('d','d','d')
nrow(a)
a['rowno']<-2
a
c
trainlist[[1]]['rowno']<-1
merge(a,c,by=1,all=T)

?merge
a

trainlist[[2]]<-TRAIN

mask1MAT[,13]<-NA
for (i in 1:length(mask1NA[,1])){
  print(paste(i,'/',length(mask1NA[,1])))
  rowno<-mask1NA[,12][i]
  mask1MAT[,13][rowno]<-toString(mask1NA[,13][i])
}
# peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2

combinedMAT<-mask1MAT[,c(12,13)]

for (i in 1:length(combinedMAT[,1])){
  print(paste(i,'/',length(combinedMAT[,1])))
  if (!is.na(mask1MAT[,13][i])){
    combinedMAT[,1][i]<-mask1MAT[,13][i]
    if (mask1MAT[,13][i]=="Sorghum"){
      combinedMAT[,2][i]<-3
    } else if (mask1MAT[,13][i]=="Millet"){
      combinedMAT[,2][i]<-2
    } else if (mask1MAT[,13][i]=="Maize"){
      combinedMAT[,2][i]<-5
    }else if (mask1MAT[,13][i]=="Cotton" ){
      combinedMAT[,2][i]<-1
    }
  } else if (!is.na(mask2MAT[,13][i])){
    combinedMAT[,1][i]<-mask2MAT[,13][i]
    if (mask2MAT[,13][i]=="Sorghum"){
      combinedMAT[,2][i]<-3
    } else if (mask2MAT[,13][i]=="Millet"){
      combinedMAT[,2][i]<-2
    } else if (mask2MAT[,13][i]=="Maize"){
      combinedMAT[,2][i]<-5
    }else if (mask2MAT[,13][i]=="Cotton" ){
      combinedMAT[,2][i]<-1
    }
  } else {
    combinedMAT[,1][i]<-NA
  }
}
unique(mask1MAT[,13])


combinedMAT[,1]<-emptylist

setValues(training2)
mask2[[12]]<-setValues(mask2[[11]],as.numeric(combinedMAT[,2]))
ncell(mask2)
length(mask2NA[,13])
plot(mask2[[12]])
plot(trainingareas,add=T)

writeRaster(mask2[[12]],'PVI_factor4_class.tif')





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
filedates<-list.files(foldername,paste('*',indexname,id,'*',sep=''),full.names=T)

#remove values bigger than 4 or smaller than 1
accuracylist<-list()
for (i in 1:length(filedates)){
  print(filedates[i])
  CLASSrast<-calc(raster(filedates[i]),function(x) { x[x<1] <- NA; return(x) })
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
  writeRaster(crop(rasterlist[[i]],shapefile('./Input/Sougoumba/studyarea.shp')),paste(dates[i],'SOU2014.tif',sep=''))
}



dates <- substr(list.files('./Input/Sougoumba/2014',full.names=T),24,31)
inputrasterpaths <- paste('./Input/radio_',id,'_',dates,'.tif',sep='')
outputrasterspaths <- paste('./Input/radio_',id,'_',dates,'.tif',sep='')
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
filedates<-list.files(foldername,paste('*',indexname,'*',sep=''),full.names=T)
testpoints<-shapefile('./Input/TestPointsCombined.shp')
filedates
accuracylist<-list()
for (i in 1:length(filedates)){
  print(filedates[i])
  resultpoints<-extract(raster(filedates[i]),testpoints)
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
NDVIdtw.brick<-dynamic.time.warping(NDVIprofile_crop, NDVIbrick, 'ndvi', id)

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
  year_monthday <- sub( '(?<=.{4})', '-', dates[i], perl=TRUE)
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

z<-dtw(a,b,keep.internals=T,step.pattern=typeIds)TRAINpointslist
z$distance
z$costMatrix

y<-dtw(a,c,keep.internals=T,step.pattern=typeIds)
y$distance
y$costMatrix

