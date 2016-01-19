## Random point creator
# trainingareas:      SpatialPolygonsDataframe of the training areas in which the random points have to be chosen.
#                     One of the columns of the dataframe should repressent the different crop types.
# randompointsraster: raster on which the points should be created
# crop_types:         List with each the name of each crop type
# crop_column_no:     Column number where the crop type variables are stored in the trainingareas dataframe
# crop_numbers:       Requested identification number for each crop type (needed for classification)
# samplesize:         Requested number of random points

create.random.points <- function(trainingareas,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize){
  emptylist<-list()
  for (i in 1:length(crop_types)){
    crop<-crop_types[i]
    print(paste('Creating ',samplesize,' random points for crop type: ',crop,' [',i,'/',length(crop_types),']',sep=''))
    cropNO<-crop_numbers[i]
    # try to create random points for the requested sample size
    cropPoints <-try(SpatialPoints(randomPoints(mask(randompointsraster,subset(trainingareas,get(names(trainingareas)[crop_column_no])==crop)),samplesize),CRS(projection(trainingareas))),silent=T)
    cropPoints <- try(SpatialPointsDataFrame(cropPoints,data=data.frame(rep(crop,length(cropPoints)),rep(cropNO,length(cropPoints)))),silent=T)
    # if there are no points created for a certain crop type: return error & break
    if (class(cropPoints)=="try-error"){
      print(paste('No random points created for ',crop,'. There were no trainingareas found for this crop.',sep=''))
      next
    # If the number of random points is lower than the requested sample size: return warning
    } else if (length(cropPoints)<samplesize){
      print(paste('Warning: Number of created random points is lower than the requested sample size (',length(cropPoints),'/',samplesize,').',crop,i,sep=''))
    }
    names(cropPoints)<-c('Crop','CropNO')
    if (length(emptylist)==0){
      emptylist[1]<-cropPoints
    } else{
      emptylist[length(emptylist)+1]<-cropPoints
    }
  }
  return(emptylist)
}