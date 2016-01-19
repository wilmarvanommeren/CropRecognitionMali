## Calculating vegetation indices that use the soil line
# rasterlist: list that includes all raster files
# funct:      function to be used in the overlay loop
# indexname:  string with the name of the vegetation index
# fheight:    flight altitude
# NIR:        NIR rasterband used in the function
# RED:        RED rasterband used in the function
# interval:   interval ratio of the values in the RED raster at which the minimum NIR value is calculated

vegetation.index.type2<-function(rasterlist,funct,indexname,fheight,NIR,RED,interval){
  if (length(list.files(path='./Output/',pattern=paste(indexname,fheight,'.tif',sep='')))==1){
    INDEXbrick<-brick(list.files(path='./Output/',pattern=paste(indexname,fheight,'.tif',sep=''),full.names=T))
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
    #C = NIR soil ref/red soil ref
    
    # INDEX
    INDEXlist<-list()
    for(i in 1:length(rasterlist)){
      INDEXraster <- rasterlist[i]
      s<<-as.numeric(soil_slope[i])
      a<<-as.numeric(soil_intercept[i])
      INDEXlist[i]<-overlay.function(INDEXraster, funct,NIR,RED)
    }
    print('Creating rasterbrick')
    INDEXbrick<-brick(INDEXlist)
    print(paste('Saving rasterbrick to: ./Output/',indexname,fheight,'.tif',sep=''))
    writeRaster(INDEXbrick, paste('./Output/',indexname,fheight,'.tif',sep=''))
  }
  return(INDEXbrick)
}