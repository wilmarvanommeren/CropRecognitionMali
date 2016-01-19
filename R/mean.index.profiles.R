## Extracting the mean index profiles per field and per crop
# indexbrick:     brick of indexrasters
# indexname:      string with the name of the vegetation index
# fheight:        flight altitude
# bare.soil.mask: A vegetation mask (vegetation=1,bare=NA)
# trainingareas:  points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
# crop_types:     the unique crop types represented in the training areas
# rownames:       row names of the resulting data frame
# Outputfolder: Folder in which the calculated rasters should be saved

mean.index.profiles<-function(INDEXbrick,indexname,fheight,trainingareas,colnames,rownames,outputfolder){
  # If file exists load file
  if (length(list.files(path=outputfolder,pattern=paste('*Mean_',indexname,fheight,'.csv',sep='')))==2){
    print ('Load mean index profiles per field from file')
    INDEXprofile_field<-read.csv(list.files(outputfolder,pattern=paste('FieldMean_',indexname,fheight,'.csv',sep=''),full.names=T),header=T,sep=',')
    print ('Load mean index profiles per crop from file')
    INDEXprofile_crop<-read.csv(list.files(outputfolder,pattern=paste('CropMean_',indexname,fheight,'.csv',sep=''),full.names=T),header=T,sep=',')
  } else {
    print('Mask the index brick with the ndvi mask, and calculate the mean index profiles per field')
    INDEXprofile_field <- training.statistics.field(INDEXbrick, trainingareas, mean,colnames,rownames)
    print('Calculate the mean index profiles per crop')
    INDEXprofile_crop <- training.statistics.crop(INDEXprofile_field)
    print(paste('Save field results to table: ',outputfolder, 'FieldMean_',indexname,fheight,'.csv',sep=''))
    write.table(INDEXprofile_field,paste(outputfolder, 'FieldMean_',indexname,fheight,'.csv',sep=''),sep=',')
    print(paste('Save crop results to table: ',outputfolder, 'CropMean_',indexname,fheight,'.csv',sep=''))
    write.table(INDEXprofile_crop,paste(outputfolder, 'CropMean_',indexname,fheight,'.csv',sep=''),sep=',')
  }
  return(list(INDEXprofile_field,INDEXprofile_crop))
}