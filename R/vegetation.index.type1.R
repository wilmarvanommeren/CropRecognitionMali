## Calculating vegetation indices that only use the NIR and RED band
# rasterlist:   list that includes all raster files
# funct:        function to be used in the overlay loop
# indexname:    string with the name of the vegetation index
# id:           Unique id for dataset
# NIR:          NIR rasterband used in the function
# RED:          RED rasterband used in the function
# Outputfolder: Folder in which the calculated rasters should be saved

vegetation.index.type1<-function(rasterlist,funct,indexname,id,NIR,RED,outputfolder){
  #if file exists load vegetation index
  if (length(list.files(path=outputfolder,pattern=paste('^',indexname,id,'.tif',sep='')))==1){
    print(paste('Load vegetation index (',indexname,') from file',sep=''))
    INDEXbrick<-brick(list.files(path=outputfolder,pattern=paste('^',indexname,id,'.tif',sep=''),full.names=T))
  } else {
    # Else calculate and save raster
    print(paste('Calculating vegetation index (',indexname,')',sep=''))
    INDEXbrick<-overlay.function.list(rasterlist, funct,NIR,RED)
    print(paste('Saving rasterbrick to: ',outputfolder,indexname,id,'.tif',sep=''))
    writeRaster(INDEXbrick, paste(outputfolder,indexname,id,'.tif',sep=''))
  }
  return(INDEXbrick)
}



  