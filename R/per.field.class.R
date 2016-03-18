## Aggregate the classifications to field level
# classifiedrasters:    List that contains multiple rasters of the same extent with categorical classes
# mask:                 Polygon or raster mask that contains the cell locations which are not trees or bare soil.
#                       The raster mask should have the same extent and cell size. Therefore, a polygon mask is recommended.
# trainingareas:        SpatialPolygonsDataframe of the training areas in which the random points have to be chosen.
#                       One of the columns of the dataframe should repressent the different crop types.
# crop_types:           List with each the name of each crop type
# crop_numbers:         Requested identification number for each crop type (needed for classification)

per.field.class <- function(classifiedrasters,mask,trainingareas,crop_types,crop_numbers){
  # a. Determine the modal value from the 5 classified rasters
  mode.raster<-calc(brick(classrasterlist),fun=modal)
  # b. Mask trees and bare soil from the modal raster
  mode.raster<-mask(mode.raster,rasterToPolygons(INDEXtree))
  # c. Select training fields with crops that are occuring in the classified raster
  trainingareas<-trainingareas[trainingareas$Crop %in% c(crop_types),]
  # d. Select most frequent class in these training fields
  trainingareas$classid<-c(extract(mode.raster,trainingareas,modal,na.rm=T))
  # e. create validation data with crops and crop numbers
  factors<-data.frame(crop_types,crop_numbers)
  names(factors)<-c('Crop','ClassResult')
  # f. merge validation data with the per-field classificiation results
  data<-merge(trainingareas@data[c(2,length(trainingareas@data[1,]))],factors,by='Crop')
  # g. print confusionMatrix
  print(confusionMatrix(data[[2]],data[[3]]))
}