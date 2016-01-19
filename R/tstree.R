# TStree

## Load library
library(raster)
library(gtools)


## Set working directory
setwd('D:/GEO-X8000/')

## load source scripts
source('./Scripts/R/align.rasters.R')
source('./Scripts/R/overlay.function.R')

## Create test data
names <- substr(list.files(pattern='*0M_SAMANKO'),start=0,stop=8)
inputrasterpaths <- paste('./Erdas/',names,'_90m_mask_output.tif',sep='')
outputrasterspaths <- paste('./Erdas/',names,'_90m_mask_align.tif',sep='')
referencerasterpath <- inputrasterpaths[5]
alignlist <- align.rasters (referencerasterpath, inputrasterpaths, outputrasterspaths)
funct <- function(x,y){(x-y)/(x+y)}
x<-4 #NIR band
y<-2 #RED band
NDVIrasters<-overlay.function(alignlist, funct,x,y)
NDVIbrick<-brick(NDVIrasters)
aggNDVI<- aggregate(NDVIbrick,10,mean)
matNDVI<-as.matrix(aggNDVI)
transmatNDVI <- t(matNDVI)

# create categories
sequence<-seq(-1,1,0.1)
categories <- letters[1:length(sequence)-1]
categoriestable<- matrix(nrow=length(categories),ncol=2)
rownames(categoriestable)<-categories
colnames(categoriestable)<-c('min','max')
for (i in 1:length(categories)){
  categoriestable[i,1]<-sequence[i]
  categoriestable[i,2]<-sequence[i+1]
}

# assign categories
catmatNDVI<-matNDVI
length(matNDVI)
for (i in 1:length(matNDVI)){
  x<-matNDVI[i]
  for (j in 1:length(categoriestable[,1])){
    min <- categoriestable[j,1]
    if (is.na(x)){
      catmatNDVI[i]<-NA
    }else{
      if (x >= min){
        catmatNDVI[i]<-row.names(categoriestable)[j]
      }
    }
  }
}


