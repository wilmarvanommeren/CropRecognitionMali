## Calculate the dynamic time warping distance between optimal time series of crop classes and time series per cell
# INDEXclasses: A dataframe containing classes (columns) and optimal values per moment in time (rows)
# INDEXbrick:   A raster brick or stack containing a time series
# indexname:    String with the name of the vegetation index
# fheight:      Flight altitude

dynamic.time.warping<-function(INDEXclasses, INDEXbrick, indexname, fheight){
  if (length(list.files(path='./Output/',pattern=paste('dtw_',indexname,fheight,'.tif',sep='')))==1){
    DTWbrick<-brick(list.files(path='./Output/',pattern=paste('dtw_',indexname,fheight,'.tif',sep=''),full.names=T))
  } else{
    transposedclass <- t(INDEXclasses)
    tsmatrix <- raster::as.matrix(INDEXbrick)
    distraster <- INDEXbrick[[1]]
    distmatrix <- tsmatrix[,1]
    diststack<-stack()
    names<-row.names(transposedclass)
    for (i in 1:length(INDEXclasses)){
      reference<-transposedclass[i,]
      print(paste('Calculating dtw for: ',names[i], '. Loop ', i,'/',length(INDEXclasses),sep=''))
      for (j in 1:length(distmatrix)){
        if (any(is.na(tsmatrix[j,]))){
          distmatrix[j]<-NA
        }else{
          distmatrix[j]<-dtw(x=tsmatrix[j,],y=reference,'Manhattan',distance.only=T)$distance
        }
      }
      distraster <- setValues(distraster,distmatrix)
      names(distraster)<-names[i]
      diststack <- stack(diststack,distraster)
    }
    writeRaster(diststack,paste('./Output/dtw_',indexname,fheight,'.tif',sep=''),overwrite=T)
  }
  return(diststack)
}