focal.statistics <- function(rasterbrick,focalmatrix,funct){
  focalraster<-list()
  for (i in 1:length(names(rasterbrick))){
    focalraster[i]<-focal(rasterbrick[[i]],focalmatrix,funct)
  }
  return(brick(focalraster))
}
