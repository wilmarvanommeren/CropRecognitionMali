## Create random training and test points
# trainingareas:      SpatialPolygonsDataframe of the training areas in which the random points have to be chosen.
#                     One of the columns of the dataframe should repressent the different crop types.
# randompointsraster: raster on which the points should be created
# crop_types:         List with each the name of each crop type
# crop_column_no:     Column number where the crop type variables are stored in the trainingareas dataframe
# crop_numbers:       Requested identification number for each crop type (needed for classification)
# samplesize:         Requested number of random points
# trainingportion:    Requestion training size 

create.train.test <- function (trainingareas,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion){
  randompointlist<<-create.random.points(trainingareas,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize)
  trainlist<-list()
  testlist<-list()
  for (i in 1:length(randompointlist)){
    print(paste('Splitting the random points in a test and training dataset [',i,'/',length(randompointlist),']',sep=''))
    trainset<-sample(seq(1,length(randompointlist[[i]]),1),trainingportion*length(randompointlist[[i]]))
    testlist[i]<-randompointlist[[i]][-trainset,]
    trainlist[i]<-randompointlist[[i]][trainset,]
  }
  TRAINpoints<-do.call(rbind,trainlist)
  TESTpoints<-do.call(rbind,testlist)
  return(c(TRAINpoints,TESTpoints))
}

