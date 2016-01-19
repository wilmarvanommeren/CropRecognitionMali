## Extracting the mean index profiles per field and per crop
# indexbrick:     brick of indexrasters
# indexname:      string with the name of the vegetation index
# fheight:        flight altitude
# bare.soil.mask: A vegetation mask (vegetation=1,bare=NA)
# trainingareas:  points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
# colnames:       column names of the resulting data frame
# rownames:       row names of the resulting data frame

mean.index.profiles<-function(INDEXbrick,indexname,fheight,trainingareas,colnames,rownames){
  if (length(list.files(path='./Output/',pattern=paste('*Mean_',indexname,fheight,'.csv',sep='')))==2){
    INDEXprofile_field<-read.csv(list.files('./Output/',pattern=paste('FieldMean_',indexname,fheight,'.csv',sep=''),full.names=T),header=T,sep=',')
    INDEXprofile_crop<-read.csv(list.files('./Output/',pattern=paste('CropMean_',indexname,fheight,'.csv',sep=''),full.names=T),header=T,sep=',')
  } else {
    print(paste('Mask the index brick with the ndvi mask'))
    INDEXprofile_field <- training.statistics.field(INDEXbrick, trainingareas, mean,colnames,rownames)
    INDEXprofile_crop <- training.statistics.crop(INDEXprofile_field)
    print(paste('Save results to table: ./Output/FieldMean_',indexname,fheight,'.csv',sep=''))
    write.table(INDEXprofile_field,paste('./Output/FieldMean_',indexname,fheight,'.csv',sep=''),sep=',')
    write.table(INDEXprofile_crop,paste('./Output/CropMean_',indexname,fheight,'.csv',sep=''),sep=',')
  }
  return(list(INDEXprofile_field,INDEXprofile_crop))
}