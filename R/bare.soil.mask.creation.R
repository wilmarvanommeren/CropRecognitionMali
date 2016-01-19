## Creating a vegetation mask from a vegetation brick (vegetation=1,bare=NA)
# id:           Unique id for dataset
# rasterbrick:  Vegetation index raster
# bare_value:   Vegetation index value at which the pixel is classified as bare soil
# Outputfolder: Folder in which the calculated raster should be saved

bare.soil.mask.creation<-function(id,rasterbrick,bare_value,outputfolder){
  # If bare soil mask exists
  if (length(list.files(path=outputfolder,pattern=paste('bare',id,'.tif',sep='')))==1){
    print(paste('Load bare soil mask from file',sep=''))
    bare.soil.mask<-brick(list.files(path=outputfolder,pattern=paste('bare',id,'.tif',sep=''),full.names=T))
  } else {
    # Else calculate vegetation mask
    print(paste('Creating vegetation mask. Bare soil = ',deparse(substitute(rasterbrick)),' <',bare_value,sep=''))
    barefunct <- function(x){ifelse(is.na(x),NA,ifelse(x<bare_value,NA,1))}
    bare.soil.mask<-calc(rasterbrick,barefunct)
    print(paste('Saving mask to: ',outputfolder,'bare',id,'.tif',sep=''))
    writeRaster(bare.soil.mask, paste(outputfolder,'bare',id,'.tif',sep=''))
  }
}