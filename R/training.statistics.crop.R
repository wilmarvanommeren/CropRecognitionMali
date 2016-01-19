## Calculate the training statistics per crop and save them in a dataframe
# rasterlist:     List of rasters to calculate the statistics on
# trainingareas:  points represented by a two-column matrix or data.frame, or SpatialPoints*; SpatialPolygons*; SpatialLines; Extent; or a numeric vector representing cell numbers
# funct:          function to summarize the values (e.g. mean, min, max)
# colnames:       column names of the resulting data frame
# rownames:       row names of the resulting data frame

training.statistics.crop <- function(fieldstatistics){
  # Create a list of all unique crops in the field
  uniquecrop <- unique(colnames(fieldstatistics))
  # Create matrix to save the values of the calculation
  matrix <- matrix(ncol=length(uniquecrop),nrow=length(rownames(fieldstatistics)))
  for (i in 1:length(uniquecrop)){
    print(paste('Calculate temporal profiles per crop'))
    # Extract mean values per crop, per raster
    crop <- uniquecrop[i]
    if (length(which(colnames(fieldstatistics)==crop )) >=2){
      cropmean <- rowMeans(fieldstatistics[,names(fieldstatistics) == crop],na.rm = T)
      matrix[,i]<-cropmean
    } else {
      matrix[,i] <- fieldstatistics[[i]]
    }
    
  }
  # Transform matrix to dataframe and add column and row names
  dataframe<-data.frame(matrix)
  colnames(dataframe)<-uniquecrop
  rownames(dataframe)<-rownames(fieldstatistics)
  return (dataframe)
}

