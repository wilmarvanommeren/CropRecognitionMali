
rescale.rasters <- function(rasterlist,outputrasterspaths){
  emptylist<-list()
  for (i in 1:length(rasterlist)){
    if(file.exists(outputrasterspaths[i])){
      emptylist[i]<-brick(outputrasterspaths[i])
    }else if (compareRaster(rasterlist[[1]],rasterlist[[i]])){
      emptylist[i]<-rasterlist[[i]]
    }else{
      print(paste('Loop ',i,'/',length(rasterlist),sep=''))
      emptylist[i]<-calc(rasterlist[[i]], function(x){x/256},outputrasterspaths[i])
    }
  }
  return (emptylist)
}

