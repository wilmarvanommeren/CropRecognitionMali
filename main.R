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
source('./R/per.field.class.R')

#######################################################################################################################
# STEP 2: LOAD OR PREPARE DATA ########################################################################################
#######################################################################################################################

## Set ID with which rasters will be loaded and saved
id <-'Sougoumba2015'

## Align or load rasters
dates <- substr(list.files('./Input/Sougoumba/2015/Imagery/5. Trimmed/',pattern='.tif$'),23,30)
inputrasterpaths <- list.files('./Input/Sougoumba/2015/Imagery/5. Trimmed/',pattern='.tif$',full.names=T)
outputrasterspaths <- paste('./Input/Sougoumba/2015/Imagery/3. Aligned/align_',id,'_',dates,'.tif',sep='')
referencerasterpath <- inputrasterpaths[1]
alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)

## Rescale rasters between 0 and 1
rescalefactor <- 10000
outputrasterspaths<-paste('./Input/Sougoumba/2015/Imagery/4. Rescaled/rescaled_',id,'_',dates,'.tif',sep='')
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

PVI <- function (x,y){(1/sqrt(1+s^2))*(x-s*y-a)}
PVIbrick<-vegetation.index.type2(trimmedlist,PVI,'pvi',id,NIR,RED,interval,outputfolder)

WDVI <- function (x,y){x-s*y} #C = NIR soil ref/red soil ref == slope
WDVIbrick<-vegetation.index.type2(trimmedlist,WDVI,'wdvi',id,NIR,RED,interval,outputfolder)


#######################################################################################################################
# STEP 3: PREPARE FOR CLASSIFICATION ##################################################################################
#######################################################################################################################

## Remove bare soil
NDVIbare_value <- 0.2
bare.soil.mask <-bare.soil.mask.creation(id,NDVIbrick,NDVIbare_value,outputfolder)

## Remove bare from index (2015=8,2015=5:6)
outputfolder<-'./Output/'
full.season.rasters<-bare.soil.mask[[5:6]]
INDEXbare<-raster::stackApply(x=full.season.rasters,indices=c(1,1),fun=mean,na.rm=F)

## Remove trees
treemask <- shapefile('./Input/Sougoumba/2015/Ancillary Data/Sougoumba2015treemask5m.shp')
INDEXtree<-mask(INDEXbare,treemask,inverse=T)

## Load trainingareas
trainingareas <- shapefile("./Input/Sougoumba/2015/Ancillary Data/Sougoumba2015ICRISATfields10m.shp")

## Calculate mean and quartile spectral or temporal index profiles per training area and per crop type
samankobare<-overlay(PVIbrick,INDEXtree,fun=function(x,y){x*y})
rownames<-dates
crop_types<-trainingareas$Crop
crop_column_no<-2
INDEXprofilelist <- mean.index.profiles(samankobare,'Temporal',id,trainingareas,crop_types,rownames,outputfolder)
quantiles<-calculate.mean.quantiles(samankobare, trainingareas,unique(trainingareas$Crop),crop_column_no)
write.csv(quantiles,'quantilesspectralSougoumba2015.csv')

## Aggregate raster
PVIbrick2<-aggregate(PVIbrick, fact=2,fun=mean)
PVIbrick3<-aggregate(PVIbrick, fact=3,fun=mean)
PVIbrick4<-aggregate(PVIbrick, fact=4,fun=mean)

######################################################################################################################
# STEP 4: KNN CLASSIFICATION  ########################################################################################
######################################################################################################################
### Data
# crop numbers are: peanut = 6; maize = 5; cotton = 1; sorghum = 3; millet = 2
# soil strata is: spTransform(shapefile('./Input/Sougoumba/Ancillary Data/Soil_Strata.shp'),CRS(projection(trainingareas)))
# elevation strata is: spTransform(rasterToPolygons(raster('./Input/Sougoumba/Ancillary Data/Sougoumba_height_iso.tif'),dissolve=T),CRS(projection(trainingareas)))

### Extra instructions
# If no strata, fill in 1 for strata
# It is also possible to run the knn.with.strata alghorithm without a loop. The accuracy results of each run
# are in the table called: accuracy.output. For each strata there ar 5 columns: K-value, overall accuracy, users accuracy,
# producers accuracy & kappa. The first strata results are in the first 5 columns, second in the next 5, etc.

classificationrasterlist<-list(PVIbrick,PVIbrick2,PVIbrick3,PVIbrick4)
names(classificationrasterlist)<-c('PVIbrick','PVIbrick2','PVIbrick3','PVIbrick4')
stratalist<-list(spTransform(shapefile('./Input/Sougoumba/2015/Ancillary Data/Buildup_Strata2015.shp'),CRS(projection(trainingareas))),spTransform(rasterToPolygons(raster('./Input/Sougoumba/2015/Ancillary Data/Sougoumba2015_height_iso.tif'),dissolve=T),CRS(projection(trainingareas))),spTransform(shapefile('./Input/Sougoumba/2015/Ancillary Data/Soil_Strata2015.shp'),CRS(projection(PVIbrick_cl5))))
names(stratalist)<-c('Buildup','Elevation','Soil')

classrasterlist<-list()
for (n in 1:1){
  strata<- stratalist[[3]]
  for (l in 1:length(classificationrasterlist)){
    set.seed(123123)
    remove(accuracy.output)
    crop_types<-unique(trainingareas$Crop)
    crop_column_no <- 2
    crop_numbers<-c(4,3,2,5,1)
    samplesize<-100
    trainingportion<-0.8
    randompointsraster<-INDEXtree
    classificationraster<-classificationrasterlist[[l]]
    return.raster=T
    
    for (m in 1:5){
      print(paste('Loop:',m))
      print('')
      print('')
      classrasterlist[m]<-knn.with.strata(trainingareas,strata,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion,return.raster)
    }
    write.csv(accuracy.output,paste('knn',names(classificationrasterlist[l]),names(stratalist)[n],length(crop_types),'crops',id,'.csv',sep=''),row.names = F,col.names = F)
  }
}


######################################################################################################################
# STEP 5: REGR.TREE CLASSIFICATION  ##################################################################################
######################################################################################################################
### Extra instructions
# Similar function as KNN function. See step 4.

for (n in 1:length(stratalist)){
  strata<- stratalist[[n]]
  for (l in 1:length(classificationrasterlist)){
    set.seed(123123)
    remove(accuracy.output)
    crop_types<-unique(trainingareas$Crop)[c(1:4,7)]
    crop_column_no <- 2
    crop_numbers<-c(3,2,1,4,5)
    samplesize<-100
    trainingportion<-0.8
    randompointsraster<-INDEXtree
    classificationraster<-classificationrasterlist[[l]]
    return.raster=F
    
    for (m in 1:5){
      print(paste('Loop:',m))
      print('')
      print('')
      regressiontree.with.strata(trainingareas,strata,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion,return.raster)
    }
    write.csv(accuracy.output,paste('regtr',names(classificationrasterlist[l]),names(stratalist)[n],length(crop_types),'crops',id,'.csv',sep=''),row.names = F,col.names = F)
  }
}


######################################################################################################################
# STEP 6: per-field classification  ##################################################################################
######################################################################################################################

per.field.class(classrasterlist,rasterToPolygons(INDEXtree),trainingareas,crop_types,crop_numbers)
  
######################################################################################################################
# EXTRA: plot feature space plots ####################################################################################
######################################################################################################################

# plot feature space
library(ggplot2)
library(devtools)
library(digest)
library(extrafont)


trainingareas <- shapefile("./Input/Sougoumba/2014/Ancillary Data/Sougoumba2014fields10m.shp")


# Extract values per crop type
crop_column_no<-2
crop_types<-unique(trainingareas$Crop)[c(1:4,7)]
sorg <-extract(PVIbrick,subset(trainingareas,get(names(trainingareas)[crop_column_no])=='Sorghum'))
sorgdf <- do.call(rbind,sorg)
mil <-extract(PVIbrick,subset(trainingareas,get(names(trainingareas)[crop_column_no])=='Millet'))
mildf <- do.call(rbind,mil)
cot <-extract(PVIbrick,subset(trainingareas,get(names(trainingareas)[crop_column_no])=='Cotton'))
cotdf <- do.call(rbind,cot)
maz <-extract(PVIbrick,subset(trainingareas,get(names(trainingareas)[crop_column_no])=='Maize'))
mazdf <- do.call(rbind,maz)
pen <-extract(PVIbrick,subset(trainingareas,get(names(trainingareas)[crop_column_no])=='Peanut'))
pendf <- do.call(rbind,pen)

# Bind the created dataframes per crop
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}

crop_bands<-data.frame(cbind.fill(sorgdf[,c(1,5)],cotdf[,c(1,5)],mildf[,c(1,5)],mazdf[,c(1,5)],pendf[,c(1,5)]))

# Plot results in feature space plot with 95% confidence ellipse
legenditems<-c("Sorghum"="#A80000",'Millet'="#3E57FC", "Cotton"="#FB6648","Maize"='#3B3A35',"Peanut"='#9ED7C2')
font_import(pattern="[T/t]imes")
loadfonts(device="win")

# uncomment 'jpeg' and 'dev.off' to save plot
# jpeg('featurespaceplotSougoumba1_5_2015.jpg')
ggplot(crop_bands, aes(x=value, y = value, color = variable)) + 
  geom_point(aes(x=layer.1,y = layer.5),size=1, colour = "#740000") +
  geom_point(aes(x=layer.1.1,y = layer.5.1),size=1, colour = "#FDAD9D") +
  geom_point(aes(x=layer.1.2,y = layer.5.2),size=1, colour = "#7F91FD") +
  geom_point(aes(x=layer.1.3,y = layer.5.3),size=1, colour = "#9C9A90") +
  geom_point(aes(x=layer.1.4,y = layer.5.4),size=1, colour = "#D0ECE2") +
  stat_ellipse(aes(x=layer.1,y = layer.5, colour = "Sorghum"),size=1.2,linetype=2) +
  stat_ellipse(aes(x=layer.1.1,y = layer.5.1, colour = "Cotton"),size=1.2,linetype=2) +
  stat_ellipse(aes(x=layer.1.2,y = layer.5.2, colour = "Millet"),size=1.2,linetype=2) +
  stat_ellipse(aes(x=layer.1.3,y = layer.5.3, colour = "Maize"),size=1.2,linetype=2) +
  stat_ellipse(aes(x=layer.1.4,y = layer.5.4, colour = "Peanut"),size=1.2,linetype=2) +
  xlab('03-09-15')+ylab('05-11-15')+scale_colour_manual(name="Legend",values=legenditems)+
  theme(text=element_text(size=25,family="Times New Roman"))+
  theme(axis.title=element_text(size=25,family="Times New Roman",face='bold'))+
  theme(legend.title=element_text(size=25,family="Times New Roman",face='bold'))
# dev.off()