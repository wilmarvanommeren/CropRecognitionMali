## Mask raster or load mask
# folders:        list of the folders were the (un)masked rasters are located
# inputpattern:   unique pattern which can be used to locate the raster file (e.g. '*.tif')
# outputpattern:  unique pattern which can be used to locate the masked raster file (e.g. '*mask.tif')
# mask:           Raster or Spatial object that represents the mask
# names:          list of names to assign to the masked raster (_mask.tif will be concetinated to the end of this name)



mask.rasters <- function(inputrasterpaths,outputrasterpaths,mask,names){
  emptylist <- list()
  for (i in 1:length(inputrasterpaths)){
    # if mask exists add it to the list
    if (length(list.files(outputrasterpaths[i]))==1){
      emptylist[i]<-brick(outputrasterpaths[i])
    } 
    # else create a mask and add it to the list
    else {
      
        unmaskedraster<-brick(inputrasterpaths[i])
        print(paste('Step 1: Masking raster. Loop: ',i,'/',length(inputrasterpaths),sep=''))
        maskedraster <- mask (unmaskedraster,mask)
        print(paste('Step 2: Cropping raster. Loop: ',i,'/',length(inputrasterpaths),sep=''))
        emptylist[i] <- crop (maskedraster,mask,outputrasterpaths[i])
        names(emptylist)[i]<-names[i]
      
    }
  }
  return (emptylist)
}