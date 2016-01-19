# 90m rescale
rasterx<-rescalelist[[1]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.73519746})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.813885131})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.991004596})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.967313104})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*1.104490307})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151015.tif',overwrite=T)

rasterx<-rescalelist[[2]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.387755102})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.397959184})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.484536082})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.515789474})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.604166667})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151021.tif',overwrite=T)

rasterx<-rescalelist[[3]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.387755102})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.397959184})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.494736842})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.521276596})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.617021277})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151023.tif',overwrite=T)

rasterx<-rescalelist[[4]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.38})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.393939394})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.47})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.5})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.637362637})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151026.tif',overwrite=T)

rasterx<-rescalelist[[5]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.38})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.393939394})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.479591837})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.505154639})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.597938144})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151029.tif',overwrite=T)

rasterx<-rescalelist[[6]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.38})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.39})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.47})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.49})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.585858586})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151102.tif',overwrite=T)

rasterx<-rescalelist[[7]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.38})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.39})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.47})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.49})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.58})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151105.tif',overwrite=T)

rasterx<-rescalelist[[8]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.383838384})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.393939394})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.479591837})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.505154639})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.610526316})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151109.tif',overwrite=T)

rasterx<-rescalelist[[9]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.382303})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.386251})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.473959})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.495453})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.58473})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151113.tif')

rasterx<-rescalelist[[10]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.382303})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.39291})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.473959})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.495453})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.568024})
writeRaster(rasterx, './DN orthophotos aligned/radio_20151118.tif',overwrite=T)

# 180m rescale
rasterx<-rescalelist[[1]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.764605359})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.682683356})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.831092781})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.796463915})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*1.006059682})
writeRaster(rasterx, './DN/radio_180m_20151015.tif',overwrite=T)

rasterx<-rescalelist[[2]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.393939394})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.405})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.425})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.5})
writeRaster(rasterx, './DN/radio_180m_20151021.tif',overwrite=T)

rasterx<-rescalelist[[3]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.393939394})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.413265306})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.425})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.532967033})
writeRaster(rasterx, './DN/radio_180m_20151023.tif',overwrite=T)

rasterx<-rescalelist[[4]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.39})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.409090909})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.425})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.527173913})
writeRaster(rasterx, './DN/radio_180m_20151026.tif',overwrite=T)

rasterx<-rescalelist[[5]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.393939394})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.413265306})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.425})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.485})
writeRaster(rasterx, './DN/radio_180m_20151029.tif',overwrite=T)

rasterx<-rescalelist[[6]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.423913043})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.338541667})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.435483871})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.456989247})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.584337349})
writeRaster(rasterx, './DN/radio_180m_20151102.tif',overwrite=T)

rasterx<-rescalelist[[7]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.39})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.405})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.425})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.48989899})
writeRaster(rasterx, './DN/radio_180m_20151105.tif',overwrite=T)

rasterx<-rescalelist[[8]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.39})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.325})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.409090909})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.429292929})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.48989899})
writeRaster(rasterx, './DN/radio_180m_20151109.tif',overwrite=T)

rasterx<-rescalelist[[9]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.415546391})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.329571275})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.415546391})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.434434863})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.503029841})
writeRaster(rasterx, './DN/radio_180m_20151113.tif')

rasterx<-rescalelist[[10]]
rasterx[[1]]<-calc(rasterx[[1]],function(x){x*0.406704978})
rasterx[[2]]<-calc(rasterx[[2]],function(x){x*0.329571275})
rasterx[[3]]<-calc(rasterx[[3]],function(x){x*0.415546391})
rasterx[[4]]<-calc(rasterx[[4]],function(x){x*0.434434863})
rasterx[[5]]<-calc(rasterx[[5]],function(x){x*0.516625242})
writeRaster(rasterx, './DN/radio_180m_20151118t.tif',overwrite=T)