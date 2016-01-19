## Function that sets values below minimum or above maximum to NA
# rasterlist:         list of rasters to be rescaled
# min:                wanted minimum raster value
# max:                wanted maximum raster value
# outputrasterspaths: unique pattern which can be used to save the output raster file (e.g. '*align.tif'

trim.rasters <- function(rasterlist,min,max,outputrasterspaths){
  emptylist<-list()
  for (i in 1:length(rasterlist)){
    # If raster is already rescaled load raster
    if(file.exists(outputrasterspaths[i])){
      print(paste('Load trimmed raster from file ',i,'/',length(rasterlist),sep=''))
      emptylist[i]<-brick(outputrasterspaths[i])
      # Else rescale raster
    }else{
      print (paste('Trimming rasters between ',min,' & ',max,' Loop: ',i,'/',length(rasterlist),sep=''))
      trimmedmin<-calc(rasterlist[[i]], function(x) { x[x<min] <- NA; return(x) })
      emptylist[i]<-calc(trimmedmin, function(x) { x[x>max] <- NA; return(x) },filename=outputrasterspaths[i])
    }
  }
  return(emptylist)
}