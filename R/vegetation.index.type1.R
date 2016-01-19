## Calculating vegetation indices that only use the NIR and RED band
# rasterlist: list that includes all raster files
# funct:      function to be used in the overlay loop
# indexname:  string with the name of the vegetation index
# fheight:    flight altitude
# NIR:        NIR rasterband used in the function
# RED:        RED rasterband used in the function

vegetation.index.type1<-function(rasterlist,funct,indexname,fheight,NIR,RED){
  if (length(list.files(path='./Output/',pattern=paste('^',indexname,fheight,'.tif',sep='')))==1){
    INDEXbrick<-brick(list.files(path='./Output/',pattern=paste('^',indexname,fheight,'.tif',sep=''),full.names=T))
  } else {
    INDEXbrick<-overlay.function(rasterlist, funct,NIR,RED)
    print(paste('Saving rasterbrick to: ./Output/',indexname,fheight,'.tif',sep=''))
    writeRaster(INDEXbrick, paste('./Output/',indexname,fheight,'.tif',sep=''))
  }
  return(INDEXbrick)
}



  