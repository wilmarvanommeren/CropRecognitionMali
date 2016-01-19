## Load or rescale rasters
# rasterlist:           list of aligned rasters
# outputrasterspaths:   unique pattern which can be used to save the output raster file (e.g. '*align.tif'
# rescalefactor:        value needed to rescale the raster values between 0 and 1

rescale.rasters <- function(rasterlist,outputrasterspaths, rescalefactor){
  emptylist<-list()
  for (i in 1:length(rasterlist)){
    # If raster is already rescaled load raster
    if(file.exists(outputrasterspaths[i])){
      print(paste('Load rescaled raster from file ',i,'/',length(rasterlist),sep=''))
      emptylist[i]<-brick(outputrasterspaths[i])
    # Else rescale raster
    }else{
      print(paste('Rescale and save raster ',i,'/',length(rasterlist),sep=''))
      emptylist[i]<-calc(rasterlist[[i]], function(x){x/rescalefactor},outputrasterspaths[i])
    }
  }
  return (emptylist)
}

