## Calculate overlay for a list of rasters (and extract mean values for training areas)
# rasterlist: list that includes all raster files
# funct:      function to be used in the overlay loop
# x:          rasterband 1 used in the function
# y:          rasterband 2 used in the function

overlay.function.list<-function(rasterlist, funct,x,y){
  emptylist<-list()
  for (i in 1:length(rasterlist)){
    input.raster <- rasterlist[[i]]
    print(paste('Calculating overlay function. Loop: ',i,'/',length(rasterlist),sep=''))
    overlay.output <- overlay(input.raster[[x]],input.raster[[y]],fun=funct)
    emptylist[i]<-overlay.output
  }
  return (brick(emptylist))
}