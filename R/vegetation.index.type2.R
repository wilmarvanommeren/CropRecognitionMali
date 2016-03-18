## Calculating vegetation indices that use the soil line
# rasterlist:   list that includes all raster files
# funct:        function to be used in the overlay loop
# indexname:    string with the name of the vegetation index
# id:           Unique id for dataset
# NIR:          NIR rasterband used in the function
# RED:          RED rasterband used in the function
# interval:     interval ratio of the values in the RED raster at which the minimum NIR value is calculated
# Outputfolder: Folder in which the calculated rasters should be saved

vegetation.index.type2<-function(rasterlist,funct,indexname,id,NIR,RED,interval,outputfolder){
  # If vegetation index exists load it from file
  if (length(list.files(path=outputfolder,pattern=paste(indexname,id,'.tif',sep='')))==1){
    print(paste('Load vegetation index (',indexname,') from file',sep=''))
    INDEXbrick<-brick(list.files(path=outputfolder,pattern=paste(indexname,id,'.tif',sep=''),full.names=T))
  } else {
    # Calculate soil line slope and intercept
    soil_intercept<-list()
    soil_slope<-list()
    for (i in 1:length(rasterlist)){
      print(paste('Selecting NIR and RED raster; creating interval between minimal and maximal RED values. Loop ',i,'/',length(rasterlist),sep=''))
      REDvect <- raster::as.vector(rasterlist[[i]][[RED]])
      NIRvect <- raster::as.vector(rasterlist[[i]][[NIR]])
      minval <- min(REDvect,na.rm=T)
      maxval <- max(REDvect,na.rm=T)
      intervalseq<-seq(from = minval, to=maxval,by=interval)
      minNIR<-intervalseq
      #get minimal NIR value for each RED interval
      for (j in 1:length(intervalseq)){
        print(paste('Extracting minimal NIR values per RED interval. Loop ',i,'/',length(rasterlist), '; Innerloop ', j,'/',length(intervalseq),sep=''))
        minNIR[j]<-try(min(NIRvect[c(which((REDvect>=intervalseq[j])&(REDvect<intervalseq[j+1])))],na.rm=T))
      }
      minNIRlm<-lm(ifelse(is.infinite(minNIR),NA,minNIR)~intervalseq)
      soil_intercept[i]<-minNIRlm$coefficients[1]
      soil_slope[i]<-minNIRlm$coefficients[2]
    }
    # Calculate the vegetation index for each raster with their unique soil variables
    INDEXlist<-list()
    print(paste('Calculating the ',indexname, ' for each of the ',length(rasterlist),' rasters, with their unique soil variables.'))
    for(i in 1:length(rasterlist)){
      INDEXraster <- rasterlist[i]
      s<<-as.numeric(soil_slope[i])
      a<<-as.numeric(soil_intercept[i])
      INDEXlist[i]<-overlay.function.list(INDEXraster, funct,NIR,RED)
    }
    print('Creating a rasterbrick from the calculated vegetation indexes')
    INDEXbrick<-brick(INDEXlist)
    print(paste('Saving rasterbrick to: ',outputfolder,indexname,id,'.tif',sep=''))
    writeRaster(INDEXbrick, paste(outputfolder,indexname,id,'.tif',sep=''))
  }
  return(INDEXbrick)
}