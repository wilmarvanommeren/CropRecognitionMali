## Load or align rasters
# referencerasterpath:  list of the folderpaths were the (un)aligned rasters are located
# inputrasterpaths:     unique pattern which can be used to locate the input raster file (e.g. '*.tif')
# outputrasterspaths:   unique pattern which can be used to save the output raster file (e.g. '*align.tif')

align.rasters <- function(referencerasterpath, inputrasterpaths, outputrasterspaths){
  emptylist<-list()
  for (i in 1:length(inputrasterpaths)){
    # Check if input raster exists
    if (file.exists(inputrasterpaths[i])){
      # Check if output raster exists and if this raster is properly aligned
      if(file.exists(outputrasterspaths[i])){
        if(class(try(compareRaster(brick(outputrasterspaths[i]),brick(referencerasterpath)),silent=T))!="try-error"){
          print(paste('Load aligned raster from file ',i,'/',length(inputrasterpaths),sep=''))
          emptylist[i]<-brick(outputrasterspaths[i])
        } 
      }
      # Check if input raster already is aligned and if so add this to the list
      else if (class(try(compareRaster(brick(inputrasterpaths[i]),brick(referencerasterpath)),silent=T))!="try-error"){
        print(paste('Input raster is already aligned ',i,'/',length(inputrasterpaths),sep=''))
        emptylist[i]<-brick(x=inputrasterpaths[i])
      } 
      # Else align and save raster
      else{
        print(paste('Align and save raster ',i,'/',length(inputrasterpaths),sep=''))
        alignedraster <- align_rasters(inputrasterpaths[i],referencerasterpath,outputrasterspaths[i],T,'ALL_CPUS')
        emptylist[i]<-alignedraster
      }
    } 
    # Return message is input raster doen't exist
    else {
      print (paste('The input raster doesn\'t exist ','(',inputrasterpaths[i],').',sep=''))
    } 
  }
  return(emptylist)
}