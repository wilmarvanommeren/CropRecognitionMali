## Round float to first uneven integer

round.to.first.uneven<- function(trainingsize){
  square<-sqrt(trainingsize)
  roundedsquare<-round(square)
  if (roundedsquare%%2==0){
    direction<-roundedsquare-square
    if (direction < 0){
      roundedsquare <- roundedsquare+1
    } else{
      roundedsquare <- roundedsquare-1
    }
  }
  return(roundedsquare)
}