library("mlr3")
library("mlr3measures")
library("mlr3learners")
library("Rfast")
library("data.table")
library("random")
library("kappalab")
library("pbapply")
library("sads")
library("caret")
library("matrixStats")

randomGuessingLogLoss <- function(labels){
  classDistribution = unlist(list(as.numeric(table(labels))))
  
  classRatio1 = classDistribution[1]/ classDistribution[2]
  classRatio0 = 1 - classRatio1
  
  probabilities = data.frame(rep(classRatio0,length(labels)), rep(classRatio1,length(labels)))
  names(probabilities)<-c("0","1")
  ll = logloss(labels, as.matrix(probabilities))
  return(ll)
}

#returns 1/log loss of model ensamble
paralelMeanLogloss<-function(full_df, comb, labels){
  logloss <- meanlogloss(full_df[,comb], labels)
  return (1/logloss)
}

#function to calculate men of all the columns in data frame provided and return log loss value for this ensamble
meanlogloss <- function(comb_df, labels) {
  
  
  meansDF = data.frame(Means=rowmeans(comb_df))
  
  meansDF$'1' <- meansDF$Means 
  meansDF$'0' <- with(meansDF, 1-Means)
  
  meansDF = meansDF[c("1", "0")]
  
  ll = logloss(labels, as.matrix(meansDF, rownames.force = NA))
  return(ll)
}

wmeanll <- function(df, classes, w){
  
  meansDF = data.frame(Means=rowWeightedMeans(as.matrix(df), w))
  
  meansDF$'1' <- round(meansDF$Means,digits=14) 
  meansDF$'0' <- with(meansDF, 1-round(Means,digits=14))
  
  meansDF = meansDF[c("1", "0")]
  
  ll = logloss(classes, as.matrix(meansDF, rownames.force = NA))
  return(ll)
}

f_logloss <-function(prediction){
  measure = msr("classif.logloss")
  loglossMono = prediction$score(measure)
  return(unname(loglossMono))
}


train_part <- function(dftrain, dftest, type) {
  dftrain$ID <- seq.int(nrow(dftrain))
  dftrain$V1 <-factor(dftrain$V1, levels=c(1, 0))
  btrain = DataBackendDataTable$new(dftrain, primary_key = "ID")
  
  dftest$ID <- seq.int(nrow(dftest))
  dftest$V1 <-factor(dftest$V1, levels=c(1, 0))
  btest = DataBackendDataTable$new(dftest, primary_key = "ID")
  
  train <- TaskClassif$new(id = "train", backend = btrain, target = "V1")
  test <- TaskClassif$new(id = "test", backend =  btest, target = "V1")
  
  learner = lrn(type, predict_type = "prob")
  # we set learner parameters to match those in PySpark
  #if(type == "classif.rpart"){
  #learner$param_set$values <- list(minsplit=3, minbucket=1, maxdepth=10, cp=0.0001)
  #}
  learner$train(train)
  prediction = learner$predict(test)
  
  return(prediction)
}

#number of ensambles to create
numberOfEnsambles <- c(2,3,5,8,13)

for(ensNr in numberOfEnsambles){
  print(ensNr)
  
  old <- Sys.time() # get start time
  
  type = "classif.rpart"
  #type = "classif.log_reg"
  if(type == "classif.log_reg")
  {
    fileType = "LR"
    modeltype = "LR"
  }else{
    fileType = "DT"
    modeltype = "DT"
  }
  
  dataFile = "Bank"
  
 

  #filename = paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/resPYBank/',fileType,ensNr,'.csv', sep="", collapse=NULL)
  #fileEns = file(filename, 'w')
  #close(fileEns)
  #fileEns = file(filename, 'a')
  #writeLines('classifier_name,dataset_name,accuracy', fileEns)
  
  set.seed(307)
  
  for(val in 1:100){
    aval = val-1
    
    filename = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/Validation/',ensNr,fileType,val,'.csv', sep="", collapse=NULL)
    
    fileShap = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/ShapleyWeights/',ensNr,fileType,val,'.csv', sep="", collapse=NULL)

    predFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/',ensNr,modeltype,aval,'.csv', sep="", collapse=NULL)
    predictions = fread(predFileName)
    # print(head(predictions))
    
    
    resPredFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/',ensNr,"reserve",modeltype,aval,'.csv', sep="", collapse=NULL)
    reserved_predictions = fread(resPredFileName)
    
    testPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBank/',ensNr,"test",val,'.csv', sep="", collapse=NULL)
    test = fread(testPath)
  
    testReservedPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBank/',ensNr,"test_reserved",val,'.csv', sep="", collapse=NULL)
    test_reserved = fread(testReservedPath)
  
    labels = factor(test$V1, levels=c(1, 0))
    validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    
    zipfPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBank/',ensNr,"zipf",'.csv', sep="", collapse=NULL)
    zipf = fread(zipfPath)
    list = zipf
    
    #calculate random weights
    temprweigths <- 1
    for(num in list$x){
      temprweigths <- c(temprweigths, runif(1, min = 0, max = 1))
    }
    rweights <- temprweigths[-1] 
    
    N = lengths(list)
    #print(N)
    #calculate all the possible combinations
    combinations = unlist(lapply(1:N, function(m) combn(1:N, m, simplify=F)), recursive=F)

    # we remove all the single digit combinations, as those are calculated differently
    combinations = combinations[-c(1:N)]
    
    rgll = randomGuessingLogLoss(labels)
    
    ensll <- 1
    
    combloglosses <-list(1/rgll)
    
    start_amount = 0
    end_amount = 0
    i = 1
    
   singleLLFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/',ensNr,modeltype,'part',aval,'.csv', sep="", collapse=NULL)
   singleLL = fread(singleLLFileName)

   for(amount in unlist(list)){
     ll = singleLL$V1[i]
     combloglosses <- append(combloglosses, list(1/unname(ll)))
     pname=paste("pred",i, sep="", collapse=NULL)

     if( i == 1){
       full_df = get(pname, predictions)
     }else{
       full_df = cbind(full_df,get(pname, predictions))
     }
     i = i+1
   }

   loglossesPath = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBank/',ensNr,modeltype,'part',aval,'.csv', sep="", collapse=NULL)
   wll = fread(loglossesPath)
   wll = 1/wll$V1


   pbapply::pboptions(type = "txt", style = 3)
   # res = paralelMeanLogloss(predictions, combinations[3], labels)

   #calculating Shapley values
   paralelLogLosses <- pblapply(combinations,function(m) paralelMeanLogloss(full_df, m, labels))


    combloglosses <- append(combloglosses, paralelLogLosses)
    # print(as.numeric(combloglosses))

    mu <- set.func(as.numeric(combloglosses))
    ShapeyKl = kappalab::Shapley.value(mu)

    ShapSignRemoved = c()
    ShapElementRemoved = c()
    ElementsToInvert = c()

    for(wght in ShapeyKl){
      if(wght < 0){
        ElementsToInvert= append(ElementsToInvert,FALSE)
        ShapSignRemoved= append(ShapSignRemoved, -1 * wght)
        ShapElementRemoved = append(ShapElementRemoved,0)
      }else{
        ElementsToInvert= append(ElementsToInvert,TRUE)
        ShapSignRemoved= append(ShapSignRemoved,wght)
        ShapElementRemoved = append(ShapElementRemoved,wght)
      }
    }
# print(ShapeyKl)
# print(ShapSignRemoved)
# print(ShapElementRemoved)
    #creating log loss weights
    
    #creating predictions for validation data set
    i = 1
    for(amount in unlist(list)){
      pname=paste("pred",i, sep="", collapse=NULL)
      if( i == 1){
        validation_df = get(pname, reserved_predictions)
      }else{
        validation_df = cbind(validation_df,get(pname, reserved_predictions))
      }
      i = i+1
    }
    
    write.csv(validation_df, filename, row.names=TRUE)
    
    i = 1
    for(amount in unlist(list)){
      pname=paste("pred",i, sep="", collapse=NULL)
      if( i == 1 ){
        validation_df = get(pname, reserved_predictions)
        if(ElementsToInvert[i]){
          inversed_V_DF = get(pname, reserved_predictions)
        }else{
          inversed_V_DF = 1 - get(pname, reserved_predictions)
        }
      }else{
        validation_df = cbind(validation_df,get(pname, reserved_predictions))
        if(ElementsToInvert[i]){
          inversed_V_DF = cbind(inversed_V_DF,get(pname, reserved_predictions))
        }else{
          inversed_V_DF = cbind(inversed_V_DF,1-get(pname, reserved_predictions))
        }
      }
      i =  i+1
    }
    
    # Roz calculations for comparison with Rozembecki voting-based Shapley solution
    # rozFileName = paste("/home/vdledger/Downloads/PyHomoResults/",dataFile,modeltype,ensNr,'Shap',val,'.csv', sep="", collapse=NULL)
    # RozWeights=read.csv(rozFileName,check.names=FALSE, header=FALSE)
    # RzW = as.vector(as.numeric(RozWeights))
    # print(RzW)
    # validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    # RzShap = wmeanll(validation_df, validationLabels, RzW)
    # print(RzShap)
    # EQll = meanlogloss(validation_df, validationLabels)
    # print(EQll)
    # RozName = paste("Roz",ensNr, sep="", collapse=NULL)
    # RozLine = paste(RozName,val,RzShap, sep=",", collapse=NULL)
    # writeLines(c(RozLine), fileEns)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Roz",RzShap, sep=",", collapse=NULL)),fileResult)
    # 
    # 

    #Monolith result calculation
    
    # monoPath =  paste('/home/vdledger/PycharmProjects/SingleModel/resultsBankMark/',ensNr,modeltype,'mono.csv', sep="", collapse=NULL)
    # print(monoPath)
    # MONOlist = fread(monoPath)
    # MONOValue = as.list(MONOlist[val])
    
    #weighted result calculation
    
    # EQll = meanlogloss(validation_df, validationLabels)
    # print(EQll)
    # RandomWLL = wmeanll(validation_df, validationLabels, rweights)
    # print(RandomWLL)
    # Wll = wmeanll(validation_df, validationLabels, wll)
    # print(Wll)
    INVWllShap = wmeanll(inversed_V_DF, validationLabels, ShapSignRemoved)
    #print(INVWllShap)
    ZeroShap = wmeanll(validation_df, validationLabels, ShapElementRemoved)
    write.csv(ShapElementRemoved, fileShap, row.names=TRUE)
    #print(ZeroShap)
    # 
    
    # Writing results to file
    # MonoName = paste("Mono",ensNr, sep="", collapse=NULL)
    # MonoLine = paste(MonoName,aval,MONOValue, sep=",", collapse=NULL)
    # EQName = paste("EQ",ensNr, sep="", collapse=NULL)
    # EQLine = paste(EQName,aval,EQll, sep=",", collapse=NULL)
    # LLName = paste("LL",ensNr, sep="", collapse=NULL)
    # LLLine = paste(LLName,aval,Wll, sep=",", collapse=NULL)
    # InvShapName = paste("posSHAP",ensNr, sep="", collapse=NULL)
    # InvShapLine = paste(InvShapName,aval,INVWllShap, sep=",", collapse=NULL)
    # ZeroShapName = paste("maxSHAP",ensNr, sep="", collapse=NULL)
    # ZeroShapLine = paste(ZeroShapName,aval,ZeroShap, sep=",", collapse=NULL)
    # RandName = paste("RAND",ensNr, sep="", collapse=NULL)
    # RandLine = paste(RandName,aval,RandomWLL, sep=",", collapse=NULL)
    
    
    #writeLines(c(MonoLine), fileEns)
    #writeLines(c(EQLine), fileEns)
    #writeLines(c(LLLine), fileEns)
    #writeLines(c(InvShapLine), fileEns)
    #writeLines(c(ZeroShapLine), fileEns)
    #writeLines(c(RandLine), fileEns)
    
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,MONO,Wll,INVWllShap,RandomWLL,EQll, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"Mono",MONOValue, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"Perf",Wll, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"posSHAP",INVWllShap, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"maxSHAP",ZeroShap, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"Rand",RandomWLL, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"Python",dataFile,"Equal",EQll, sep=",", collapse=NULL)),fileResult)
  }
  new <- Sys.time() - old # calculate difference
 # print(new)
}

# close(fileResult)



