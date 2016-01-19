## Calculate or load overlay a
# funct:        function to be used in the overlay loop
# x:            rasterband 1 used in the function
# y:            rasterband 2 used in the function
# Outputfolder: Folder in which the calculated rasters should be saved
# filename:     Output file name

overlay.function.checkfile<-function(x, y,funct,outputfolder,filename){
  # Check if file exists
  if (file.exists(paste(outputfolder,filename,sep=''))){
    print ('Load brick from file')
    indexbrick<-brick(paste(outputfolder,filename,sep=''))
  } else {
    # Else calculate overlay function
    print ('Calculate and save brick to file')
    indexbrick<-overlay(x,y,fun=funct,filename=paste(outputfolder,filename,sep=''))
  }
  return (indexbrick)
}

