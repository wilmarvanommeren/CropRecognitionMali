## Apply the knn classification technique on different strata
# trainingareas:        SpatialPolygonsDataframe of the training areas in which the random points have to be chosen.
#                       One of the columns of the dataframe should repressent the different crop types.
# strata:               SpatialPolygonsDataframe of the strata
# randompointsraster:   Raster on which the points should be created
# classificationraster: Raster that will be used for the training of the classification
# crop_types:           List with each the name of each crop type
# crop_column_no:       Column number where the crop type variables are stored in the trainingareas dataframe
# crop_numbers:         Requested identification number for each crop type (needed for classification)
# samplesize:           Requested number of random points
# trainingportion:      Requestion training size
# return.raster:        If True return a classified raster, else only return the classification accuracy

### Extra instructions
# If no strata, fill in 1 for strata
# It is also possible to build a loop with the knn.with.strata alghorithm in it. The accuracy results of each run
# are in the table called: accuracy.output. For each strata there ar 5 columns: K-value, overall accuracy, users accuracy,
# producers accuracy & kappa. The first strata results are in the first 5 columns, second in the next 5, etc.

regressiontree.with.strata<-function(trainingareas,strata,randompointsraster,classificationraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion,return.raster){
  kvaluelist<<-list()
  trainlist<<-list()
  testlist<<-list()
  traintargetlist<<-list()
  user.acc<<-list()
  prod.acc<<-list()
  if (exists('accuracy.output')==F){
    accuracy.output<<-data.frame(matrix(ncol=length(strata)*5))}
  for (i in 1:length(strata)){
    print (paste('Step1: Calculating accuracy for strata: ',i,'/',length(strata),sep=''))
    # Create points per strata per crop type
    if (length(strata)==1){
      trainingstrata<-intersect(trainingareas,strata)
    } else {
      trainingstrata<-intersect(trainingareas,subset(strata,get(names(strata))==i))
    }
    randompoints<<-create.train.test(trainingstrata,randompointsraster,crop_types,crop_column_no,crop_numbers,samplesize,trainingportion)
    TRAINpoints<<-randompoints[[1]]
    TESTpoints<<-randompoints[[2]]
    
    # Calculate optimal k-value
    kvalue<- round.to.first.uneven(length(randompoints[[1]]))
    print(paste('K-value: ',kvalue))
    kvaluelist[i]<<-kvalue
    
    # Extract values per random point
    TRAIN <<- na.omit(data.frame(extract(classificationraster,TRAINpoints),TRAINpoints$Crop))
    TEST <<- na.omit(data.frame(extract(classificationraster,TESTpoints),TESTpoints$Crop))
    TRAINtarget<<-TRAIN[,length(TRAIN[1,])]
    TESTtarget<<-TEST[,length(TRAIN[1,])]
    
    # Create list of dataframes (only way to do this!)
    if (i==1){
      traintargetlist <<- lapply('TRAINtarget', get)
      testlist <<- lapply('TEST', get)
      trainlist <<- lapply('TRAIN', get)
    } else {
      traintargetlist[[i]]<<-TRAINtarget
      testlist[[i]]<<-TEST
      trainlist[[i]]<<-TRAIN
    }
    # Classification 
    REGRTclassification<-rpart(formula=as.formula(paste(paste(names(TRAIN)[length(TRAIN)],'~'),paste(names(TRAIN)[1:length(TRAIN)-1],collapse='+'))), data=TRAIN,method='class')
    
    # Improve tree
    bestcp <- REGRTclassification$cptable[which.min(REGRTclassification$cptable[,"xerror"]),"CP"]
    REGRTclassification.pruned <- prune(REGRTclassification, cp = bestcp)
    
    # Accuracy assessment
    accuracy.assess<-confusionMatrix(predict(REGRTclassification.pruned,newdata=TEST,type='class'),TEST[,length(TEST)])
    overall<-accuracy.assess$overall[1]
    kappa<-accuracy.assess$overall[2]
    accuracy.assess.table<<-as.data.frame.matrix(accuracy.assess$table)
    accuracy.assess.table[,length(accuracy.assess.table[,1])+1]<<-apply(accuracy.assess.table,1,FUN=sum)
    accuracy.assess.table[length(accuracy.assess.table[1,]),]<<-apply(accuracy.assess.table,2,FUN=sum)
    
    for(k in 1:length(accuracy.assess.table[,1])-1){
      user.acc[k]<<-accuracy.assess.table[k,k]/accuracy.assess.table[k,length(accuracy.assess.table[,1])]
      prod.acc[k]<<-accuracy.assess.table[k,k]/accuracy.assess.table[length(accuracy.assess.table[,1]),1]
    }
    user<-mean(unlist(user.acc))
    prod<-mean(unlist(prod.acc))
    
    print(paste('Overal Accuracy:',round(overall,2)))
    print(paste('Users Accuracy:',round(user,2)))
    print(paste('Producers Accuracy:',round(prod,2)))
    print(paste('Kappa:',round(kappa,2)))
    print('')
    
    rowno<-ifelse(any(is.na(accuracy.output[length(accuracy.output[,1]),])),length(accuracy.output[,1]),length(accuracy.output[,1])+1)
    accuracy.output[rowno,(i*5-5)+1]<<-kvalue
    accuracy.output[rowno,(i*5-5)+2]<<-overall
    accuracy.output[rowno,(i*5-5)+3]<<-user
    accuracy.output[rowno,(i*5-5)+4]<<-kappa
    accuracy.output[rowno,(i*5-5)+5]<<-prod
  }

  # Create raster is return.raster = TRUE
  if (return.raster){
    for (j in 1:length(strata)){
      print (paste('Step 2: Classifying strata: ',i,'/',length(strata),sep=''))
      # Create raster mask per strata
      print(paste('Splitting index raster into different strata. Loop: ',j,'/',length(strata),sep=''))
      if(length(strata)==1){
        masklist=classificationraster
      }else{
        masklist<-mask(classificationraster,subset(strata,get(names(strata))==j))}
      
      
      # Convert raster to dataframe
      print(paste('Transforming strata to dataframe for calculations. Loop: ',j,'/',length(strata),sep=''))
      mask.df<-as.data.frame(raster::as.matrix(masklist))
      
      # Create per dataframe a new column with the row numbers
      print(paste('Creating a new column with row numbers. Loop: ',j,'/',length(strata),sep=''))
      mask.df['rowno']<-matrix(seq(from=1,to=nrow(mask.df),by=1),ncol=1)
      
      # Remove NAs from dataframe
      print(paste('Removing NAs from dataframe. Loop: ',j,'/',length(strata),sep=''))
      mask.na.df<<-na.omit(mask.df)
      
      # Regression tree classification
      print(paste('Regression tree classification on dataframe without NAs. Loop: ',j,'/',length(strata),sep=''))
      REGRTclassification<-rpart(formula=as.formula(paste(paste(names(trainlist[[j]])[length(trainlist[[j]])],'~'),paste(names(trainlist[[j]])[1:length(trainlist[[j]])-1],collapse='+'))), data=trainlist[[j]],method='class')
      
      # Improve tree
      bestcp <- REGRTclassification$cptable[which.min(REGRTclassification$cptable[,"xerror"]),"CP"]
      REGRTclassification.pruned <- prune(REGRTclassification, cp = bestcp)
      
      # Add column with Regression tree classification
      mask.na.df['Crop']<<-predict(REGRTclassification.pruned,newdata=mask.na.df[,1:length(mask.na.df)-1],type='class')
      
      # Create dataframe with classification result per strata
      print(paste('Creating dataframe with classification results per strata. Loop: ',j,'/',length(strata),sep=''))
      if(j==1){
        combined.df<<-merge(mask.df,mask.na.df,by='rowno',all=T)[c('rowno','Crop')]
      }else{
        combined.df[j+1]<<-merge(mask.df,mask.na.df,by='rowno',all=T)['Crop']
      }
    }
    
    print('Step 3: Creating classified raster by merging both strata')
    # Merge classification results to a single column
    print('Merging classification results into a single column')
    combined.df[ncol(combined.df)+1]<-apply(combined.df[2:ncol(combined.df)],1,max,na.rm=T)
    combined.df<-combined.df[c(1,ncol(combined.df))]
    names(combined.df)<-c('rowno','Crop')
    
    # Add crop number to the dataframe and order by rownumber
    print('Add crop number to dataframe with the classification result')
    classes<<-data.frame(crop_types,crop_numbers)
    names(classes)<-c('Crop','CropNO')
    combined.df.cropno<-merge(combined.df,classes,by='Crop',all=T)
    
    # Create classified raster with the crop numbers
    print('Creating raster with the crop numbers')
    combined.df.cropno<-combined.df.cropno[order(combined.df.cropno$rowno),]
    classifiedraster<-setValues(classificationraster[[1]],as.numeric(combined.df.cropno[[3]]))
    
    return(classifiedraster)
  }
}
