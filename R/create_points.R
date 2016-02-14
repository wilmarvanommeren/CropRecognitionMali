## Author:    Wilmar van Ommeren
## Date:      February 2016
## E-mail:    wilmarvanommeren@gmail.com
## Linkedin:  https://nl.linkedin.com/in/wilmarvanommeren

#######################################################################################################################
# STEP 1: PREPARE SCRIPT ##############################################################################################
#######################################################################################################################

## Set working directory
setwd('E:/Mijn documenten/Wageningen/Thesis/Scripts')

## Load & install packages if required
if (!require(sp)){install.packages("sp")}
if (!require(raster)){install.packages("raster")}
if (!require(rgeos)){install.packages("rgeos")}
if (!require(rgdal)){install.packages("rgdal")}
if (!require(gdalUtils)){install.packages("gdalUtils")}
if (!require(dismo)){install.packages("dismo")}
if (!require(class)){install.packages("class")}
if (!require(caret)){install.packages("caret")}
if (!require(e1071)){install.packages("e1071")}
if (!require(rpart)){install.packages("rpart")}

## Load source scripts
source('./R/align.rasters.R')
source('./R/rescale.rasters.R')
source('./R/trim.rasters.R')
source('./R/overlay.function.list.R')
source('./R/overlay.function.checkfile.R')
source('./R/vegetation.index.type1.R')
source('./R/vegetation.index.type2.R')
source('./R/bare.soil.mask.creation.R')
source('./R/training.statistics.field.R')
source('./R/training.statistics.crop.R')
source('./R/mean.index.profiles.R')
source('./R/calculate.mean.quantiles.R')
source('./R/create.train.test.R')
source('./R/create.random.points.R')
source('./R/round.to.first.uneven.R')
source('./R/knn.with.strata.R')
source('./R/regressiontree.with.strata.R')

#######################################################################################################################
# STEP 2: LOAD OR PREPARE DATA ########################################################################################
#######################################################################################################################

## Set ID with which rasters will be loaded and saved
id <-'Sougoumba2014'

## Align or load rasters
dates <- substr(list.files('./Input/Sougoumba/2014/Imagery/5. Trimmed/',pattern='.tif$'),23,30)
inputrasterpaths <- list.files('./Input/Sougoumba/2014/Imagery/5. Trimmed/',pattern='.tif$',full.names=T)
outputrasterspaths <- paste('./Input/Sougoumba/2014/Imagery/3. Aligned/align_',id,'_',dates,'.tif',sep='')
referencerasterpath <- inputrasterpaths[1]
alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)

## Rescale rasters between 0 and 1
rescalefactor <- 10000
outputrasterspaths<-paste('./Input/Sougoumba/2014/Imagery/4. Rescaled/rescaled_',id,'_',dates,'.tif',sep='')
rescalelist<-rescale.rasters(alignlist,outputrasterspaths,rescalefactor)

## Remove values outside wanted range
min <- 0
max <- 1
outputrasterspaths <- paste('./Input/Sougoumba/2014/Imagery/5. Trimmed/trimmed_',id,'_',dates,'.tif',sep='')
trimmedlist<-trim.rasters(rescalelist,min,max,outputrasterspaths)

## Calculate vegetation indexes
NIR<-4 #x
RED<-3 #y
outputfolder <- './Output/'
NDVI <- function(x,y){(x-y)/(x+y)}
NDVIbrick <-vegetation.index.type1(trimmedlist,NDVI,'ndvi',id,NIR,RED,outputfolder)

#######################################################################################################################
# STEP 3: PREPARE FOR CLASSIFICATION ##################################################################################
#######################################################################################################################

## Remove bare soil
NDVIbare_value <- 0.2
bare.soil.mask <-bare.soil.mask.creation(id,NDVIbrick,NDVIbare_value,outputfolder)

# Remove bare from index (2014=8,2015=5:6)
outputfolder<-'./Output/'
full.season.rasters<-bare.soil.mask[[8]]
INDEXbare<-raster::stackApply(x=full.season.rasters,indices=c(1,1),fun=mean,na.rm=F)

treemask <- shapefile('./Input/Sougoumba/2014/Ancillary Data/sougoumba2014treemask5m.shp')
INDEXtree<-mask(INDEXbare,treemask,inverse=T)

trainingareas <- shapefile("./Input/Sougoumba/2014/Ancillary Data/Sougoumba2014fields10m.shp")

# crop numbers are: peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2

crop_types<-unique(trainingareas$Crop)
crop_types
crop_numbers<-c(6,3,2,5,1)
samplesize<-100
trainingportion<-0.8


for (i in 1:5){
  traintest<-create.train.test(trainingareas,INDEXtree,crop_types,2,crop_numbers,samplesize,trainingportion)
  shapefile(traintest[[1]],paste('TRAINsou2014_',i,'.shp',sep=''))
  shapefile(traintest[[2]],paste('TESTsou2014_',i,'.shp',sep=''))
  
  }

  
  