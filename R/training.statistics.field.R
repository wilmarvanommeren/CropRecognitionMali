## Calculate the training statistics per field and save them in a dataframe
# rasterlist:     List of rasters to calculate the statistics on
# trainingareas:  points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
# funct:          function to summarize the values (e.g. mean, min, max)
# colnames:       column names of the resulting data frame
# rownames:       row names of the resulting data frame

training.statistics.field <- function(rasterbrick, trainingareas, funct,colnames,rownames){
  # Create matrix to save the values of the calculation
  FieldValueMat <- matrix(ncol=length(trainingareas[[1]]),nrow=length(names(rasterbrick)))
    for (i in 1:length(names(rasterbrick))){
    raster <- rasterbrick[[i]]
    print(paste('Extracting mean raster values per field. Loop: ',i,'/',length(names(rasterbrick)),sep=''))
    # Extract mean values per field, per raster
    trainingValues <- extract(raster, trainingareas, fun=funct,na.rm=T)
    FieldValueMat[i,]<-trainingValues
    }
  # Transform matrix to dataframe and add column and row names
  dataframe <- data.frame(FieldValueMat)
  colnames(dataframe)<-colnames
  rownames(dataframe)<-rownames
  return (dataframe)
}
