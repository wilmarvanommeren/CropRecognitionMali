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
source('./R/create.train.test.R')
source('./R/create.random.points.R')
source('./R/round.to.first.uneven.R')
source('./R/knn.with.strata.R')
source('./R/regressiontree.with.strata.R')

#######################################################################################################################
# STEP 2: LOAD OR PREPARE DATA ########################################################################################
#######################################################################################################################

## Set ID with which rasters will be loaded and saved
id <-'Sougoumba2015'

## Align or load rasters
dates <- substr(list.files('./Input/Sougoumba/2015/Imagery/2. Erdas Autosync, clouds removed, unneeded bands removed, cropped & masked',pattern='*_output.tif$'),1,8)
inputrasterpaths <- list.files('./Input/Sougoumba/2015/Imagery/2. Erdas Autosync, clouds removed, unneeded bands removed, cropped & masked',pattern='*_output.tif$',full.names=T)
outputrasterspaths <- paste('./Input/Sougoumba/2015/Imagery/3. Aligned/align_',id,'_',dates,'.tif',sep='')
referencerasterpath <- inputrasterpaths[1]
alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)

## Rescale rasters between 0 and 1
rescalefactor <- 10000
outputrasterspaths <- paste('./Input/Sougoumba/2015/Imagery/4. Rescaled/rescaled_',id,'_',dates,'.tif',sep='')
rescalelist<-rescale.rasters(alignlist,outputrasterspaths,rescalefactor)

## Remove values outside wanted range
min <- 0
max <- 1
outputrasterspaths <- paste('./Input/Sougoumba/2015/Imagery/5. Trimmed/trimmed_',id,'_',dates,'.tif',sep='')
trimmedlist<-trim.rasters(rescalelist,min,max,outputrasterspaths)

## Calculate vegetation indexes
NIR<-4 #x
RED<-3 #y
outputfolder <- './Output/'
NDVI <- function(x,y){(x-y)/(x+y)}
NDVIbrick <-vegetation.index.type1(trimmedlist,NDVI,'ndvi',id,NIR,RED,outputfolder)

SAVI <- function(x,y){((x-y)/(x+y+0.5))*(1+0.5)} #L<- 0.5
SAVIbrick<-vegetation.index.type1(trimmedlist,SAVI,'savi',id,NIR,RED,outputfolder)

interval <- 0.005
Xfactor <- 0.08
TSAVI <- function(x,y){(s*(x-s*y-a))/(a*x+y-a*s+Xfactor*(1+s^2))}
TSAVIbrick<-vegetation.index.type2(trimmedlist,TSAVI,'tsavi',id,NIR,RED,interval,outputfolder)

PVI <- function (x,y){(1/sqrt(1+s^2))*(x-s*y-a)}#http://naldc.nal.usda.gov/download/9394/PDF
PVIbrick<-vegetation.index.type2(trimmedlist,PVI,'pvi',id,NIR,RED,interval,outputfolder)

WDVI <- function (x,y){x-s*y} #C = NIR soil ref/red soil ref == slope
WDVIbrick<-vegetation.index.type2(trimmedlist,WDVI,'wdvi',id,NIR,RED,interval,outputfolder)

#######################################################################################################################
# STEP 3: PREPARE FOR CLASSIFICATION ##################################################################################
#######################################################################################################################

## Select the index to be used in the analysis
index<-PVIbrick
indexname<-'PVI'

## Remove bare soil
NDVIbare_value <- 0.2
bare.soil.mask <-bare.soil.mask.creation(id,NDVIbrick,NDVIbare_value,outputfolder)

# Remove bare from index
outputfolder<-'./Output'
full.season.rasters<-bare.soil.mask[[5:6]]
INDEXbare<-raster::stackApply(x=full.season.rasters,indices=c(1,1),fun=mean,na.rm=F)

# Calculate mean index profiels per training area and per crop type
trainingareas <- shapefile("./Input/Sougoumba/2015/Ancillary Data/Sougoumba2015ICRISATfields5m3.shp") 
crop_types<-trainingareas$Crop

INDEXprofilelist <- mean.index.profiles(INDEXbare,indexname,id,trainingareas,crop_types,dates,outputfolder)
INDEXprofile_field <- INDEXprofilelist[[1]]
INDEXprofile_crop <- INDEXprofilelist[[2]]

## Remove trees from raster
treemask <- shapefile('./Input/Sougoumba/2015/Ancillary Data/Sougoumba2015treemask5m2.shp')
INDEXtree<-mask(INDEXbare,treemask,inverse=T)

## Aggregate raster
PVIbrick2<-aggregate(PVIbrick, fact=2,fun=mean)
INDEXtreeagg<-aggregate(INDEXtree, fact=3,fun=mean)

######################################################################################################################
# STEP 4: KNN CLASSIFICATION  ########################################################################################
######################################################################################################################
### Data
# crop numbers are: peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2
# soil strata is: spTransform(shapefile('./Input/Sougoumba/Ancillary Data/Soil_Strata.shp'),CRS(projection(trainingareas)))
# elevation strata is: spTransform(rasterToPolygons(raster('./Input/Sougoumba/Ancillary Data/Sougoumba_height_iso.tif'),dissolve=T),CRS(projection(trainingareas)))

### Extra instructions
# If no strata, fill in 1 for strata
# It is also possible to build a loop with the knn.with.strata alghorithm in it. The accuracy results of each run
# are in the table called: accuracy.output. For each strata there ar 5 columns: K-value, overall accuracy, users accuracy,
# producers accuracy & kappa. The first strata results are in the first 5 columns, second in the next 5, etc.
source('./R/knn.with.strata.R')
classificationrasterlist<-list(PVIbrick,PVIbrick2,PVIbrick3,PVIbrick4,NDVIbrick,WDVIbrick,SAVIbrick,TSAVIbrick)
names(classificationrasterlist)<-c('PVIbrick','PVIbrick2','PVIbrick3','PVIbrick4','NDVIbrick','WDVIbrick','SAVIbrick','TSAVIbrick')
stratalist<-list(spTransform(rasterToPolygons(raster('./Input/Sougoumba/Ancillary Data/Sougoumba_height_iso.tif'),dissolve=T),CRS(projection(trainingareas))),spTransform(shapefile('./Input/Sougoumba/Ancillary Data/Soil_Strata.shp'),CRS(projection(trainingareas))))
names(stratalist)<-c('height','Soil')
for (n in 1:length(stratalist)){
  strata<- stratalist[[n]]
  for (l in 1:length(classificationrasterlist)){
    set.seed(123123)
    remove(accuracy.output)
    crop_types<-unique(trainingareas$Crop)[c(1:3,5)]
    crop_column_no <- 4
    crop_numbers<-c(5,2,1,3)
    samplesize<-100
    trainingportion<-0.8
    randompointsraster<-INDEXtree
    classificationraster<-classificationrasterlist[[l]]
    return.raster=F
    
    for (m in 1:20){
      print(paste('Loop:',m))
      print('')
      print('')
      knn.with.strata(trainingareas,strata,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion,return.raster)
    }
    write.csv(accuracy.output,paste('knn',names(classificationrasterlist[l]),names(strata)[n],'.csv'),row.names = F,col.names = F)
  }
}

######################################################################################################################
# STEP 5: REGR.TREE CLASSIFICATION  ##################################################################################
######################################################################################################################
### Extra instructions
# Similar function as KNN function. See step 4.

source('./R/regressiontree.with.strata.R')

for (n in 1:length(stratalist)){
  strata<- stratalist[[n]]
  for (l in 1:length(classificationrasterlist)){
    set.seed(123123)
    remove(accuracy.output)
    crop_types<-unique(trainingareas$Crop)[c(1:3,5)]
    crop_column_no <- 4
    crop_numbers<-c(5,2,1,3)
    samplesize<-100
    trainingportion<-0.8
    randompointsraster<-INDEXtree
    classificationraster<-classificationrasterlist[[l]]
    return.raster=F
    
    for (m in 1:20){
      print(paste('Loop:',m))
      print('')
      print('')
      regressiontree.with.strata(trainingareas,strata,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion,return.raster)
    }
    write.csv(accuracy.output,paste('regtr',names(classificationrasterlist[l]),names(strata)[n],'.csv'),row.names = F,col.names = F)
  }
}

######################################################################################################################
# STEP 6: MAJORITY FILTER  ###########################################################################################
######################################################################################################################

maj.raster<-focal(classificationraster,w=matrix(c(1,1,1,1,1,1,1,1,1),nrow=3),fun=modal)

crop_types<-unique(trainingareas$Crop)[c(1:3,5)]
crop_column_no <- 4
crop_numbers<-c(5,2,1,3)
accuracypoints<-create.random.points (trainingareas,INDEXtree,crop_types,crop_column_no,crop_numbers,1000)
class.result<-extract(maj.raster,accuracypoints)
class.target<-overlay(accuracypoints,trainingareas)

######################################################################################################################
# END ################################################################################################################
######################################################################################################################