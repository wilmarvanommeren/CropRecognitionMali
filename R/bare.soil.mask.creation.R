## Creating a vegetation mask from the NDVI brick (vegetation=1,bare=NA)
# funct:          function to be used in the overlay loop
# NDVIbrick:      brick of NDVI rasters
# NDVIbare_value: ndvi value at which the pixel is classified as bare soil

bare.soil.mask.creation<-function(fheight,NDVIbrick,NDVIbare_value){
  if (length(list.files(path='./Output/',pattern=paste('bare',fheight,'.tif',sep='')))==1){
    bare.soil.mask<-brick(list.files(path='./Output/',pattern=paste('bare',fheight,'.tif',sep=''),full.names=T))
  } else {
    print(paste('Creating vegetation mask. Bare soil = ndvi <',NDVIbare_value,sep=''))
    barefunct <- function(x){ifelse(is.na(x),NA,ifelse(x<NDVIbare_value,NA,1))}
    bare.soil.mask<-calc(NDVIbrick,barefunct)
    print(paste('Saving mask to: ./Output/bare',fheight,'.tif',sep=''))
    writeRaster(bare.soil.mask, paste('./Output/bare',fheight,'.tif',sep=''))
  }
}