## Set all cells that are NA during time to NA in a single raster
# maskbrick:  brick/stack which contains the bare soil masks over time

singlemask <- function(maskbrick){
  maskmatrix<-raster::as.matrix(maskbrick)
  row.has.na <- apply(maskmatrix, 1, function(x){any(is.na(x))})
  row.has.na<-ifelse(row.has.na,NA,1)
  singlemask<-setValues(maskbrick[[1]],row.has.na)
  return(singlemask)
}