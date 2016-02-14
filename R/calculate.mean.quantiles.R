calculate.mean.quantiles<-function(INDEXbrick, trainingareas,crop_types,crop_column_no){
  crop_types<-unique(crop_types)
  quantiledata<-data.frame(matrix(ncol=5))
  for (i in 1:length(crop_types)){
    print(paste('Calculating quantiles for:',crop_types[i]))
    for(j in 1:nlayers(INDEXbrick)){
      allvalues<- extract(INDEXbrick[[j]],subset(trainingareas,get(names(trainingareas)[crop_column_no])==crop_types[i]),na.rm=T)
      print(paste('Layer',j))
      quantiles<-quantile(unlist(allvalues),na.rm=T)
      quantiledata<-rbind(quantiledata,quantiles)
    }
  }
  quantiledata<-quantiledata[-1,]
  colnames(quantiledata)<-c('0%','25%','50%','75%','100%')
  return(quantiledata)
}