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
paralelMeanLogloss<-function(full_df,comb,labels){
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
  
  meansDF$'1' <- round(meansDF$Means,digits=15) 
  meansDF$'0' <- with(meansDF, 1-round(Means,digits=15))
  
  meansDF = meansDF[c("1", "0")]
  
  ll = logloss(classes, as.matrix(meansDF, rownames.force = NA))
  return(ll)
}

f_logloss <-function(prediction){
  measure = msr("classif.logloss")
  loglossMono = prediction$score(measure)
  return(unname(loglossMono))
}

predict <-function(learner, dftest){
  dftest$ID <- seq.int(nrow(dftest))
  dftest$V1 <-factor(dftest$V1, levels=c(1, 0))
  btest = DataBackendDataTable$new(dftest, primary_key = "ID")
  test <- TaskClassif$new(id = "test", backend =  btest, target = "V1")
  
  prediction = learner$predict(test)
  return(prediction)
}

train_single_part <- function(dftrain, type)
{
  dftrain$ID <- seq.int(nrow(dftrain))
  dftrain$V1 <-factor(dftrain$V1, levels=c(1, 0))
  btrain = DataBackendDataTable$new(dftrain, primary_key = "ID")
  train <- TaskClassif$new(id = "train", backend = btrain, target = "V1")
  learner = lrn(type, predict_type = "prob")
  # we set learner parameters to match those in PySpark
  #if(type == "classif.rpart"){
  #learner$param_set$values <- list(minsplit=3, minbucket=1, maxdepth=10, cp=0.0001)
  #}
  learner = learner$train(train)
  return(learner)
}


train_mono <- function(dftrain, dftest, type) {

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
  learner = learner$train(train)
  prediction = learner$predict(test)

  return(prediction)
}

#number of ensambles to create
numberOfEnsambles <- c(2,3,5,8,13)

resultFilename= paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/RozResults/R/RlangRozResfullBNGDT.csv', sep="", collapse=NULL)
fileResult = file(resultFilename, 'a')
timefile = file("exactPerm.txt", 'w')

# writeLines(c("numberOfModels,modelType,prlang,datafile,fusionType,Result"), fileResult)

for(ensNr in numberOfEnsambles){
  writeLines(as.character(ensNr), timefile)
  fpath = "/home/vdledger/PycharmProjects/DataConverting/transformeddata/bankmark.csv"
  #fpath = "/home/vdledger/PycharmProjects/DataConverting/transformeddata/BNG_credit-a.csv"
  data = fread(fpath)
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
  dataFile = "BNG"
  #fileType = ""

  filename = paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/RozResults/R/',fileType,ensNr,'.csv', sep="", collapse=NULL)
  fileEns = file(filename, 'w')
  close(fileEns)
  fileEns = file(filename, 'a')
  
  set.seed(307)
  
  for(val in 1:100){
    
    predFiles= paste('./',fileType,ensNr,"Valid",val,'.csv', sep="", collapse=NULL)
    # we shuffle the data randomly
    drows <- sample(nrow(data))
    shufdata <- data[drows,]
    shufdata <- shufdata[0:nrow(data)]
    
    # create a test and train data set and leave some resrve data for testing on blockchain network.
   
    in_train <- createDataPartition(shufdata$V1, p = 0.8, list = FALSE)
    
    trainFull <- shufdata[in_train]
    
    trainFileName = paste('/home/vdledger/PycharmProjects/SingleModel/data/',ensNr,'train',val,'.csv', sep="", collapse=NULL)
   # write.csv(trainFull, trainFileName, row.names = FALSE)
   
    test_reserved <- shufdata[-in_train]
    
    testFileReservedName = paste('/home/vdledger/PycharmProjects/SingleModel/data/',ensNr,'test_reserved',val,'.csv', sep="", collapse=NULL)
    #write.csv(test_reserved, testFileReservedName, row.names = FALSE)
    
    in_test <- createDataPartition( trainFull$V1, p = 0.8, list = FALSE)
    
    test <- trainFull[-in_test]
    
    testFileName = paste('/home/vdledger/PycharmProjects/SingleModel/data/',ensNr,'test',val,'.csv', sep="", collapse=NULL)
    #write.csv(test, testFileName, row.names = FALSE)
  
    # distubute data based on zipf law
    zipfName = paste('/home/vdledger/PycharmProjects/SingleModel/data/',ensNr,'zipf','.csv', sep="", collapse=NULL)
    zipfDist = rzipf(nrow(trainFull), ensNr, 2)
    
    list =unlist(list(as.numeric(table(zipfDist))))
    #write.csv(list, zipfName, row.names=FALSE)
    
    # Mono <- train_mono(trainFull,test_reserved,type)
    
    #calculate random weights
    temprweigths <- 1
    for(num in list){
      temprweigths <- c(temprweigths, runif(1, min = 0, max = 1))
    }
    rweights <- temprweigths[-1] 
    
    
    N = length(list)
    #calculate all the possible combinations
    combinations = unlist(lapply(1:N, function(m) combn(1:N, m, simplify=F)), recursive=F)
    
    # we remove all the single digit combinations, as those are calculated differently
    combinations = combinations[-c(1:N)]
    
    labels = factor(test$V1, levels=c(1, 0))
    
    rgll = randomGuessingLogLoss(labels)
    
    ensll <- 1
    
    models <- c()
  
    combloglosses <-list(1/rgll)
  
    start_amount = 0
    end_amount = 0
    i = 0
    measure = msr("classif.logloss")
  
    for(amount in unlist(list)){
      end_amount = start_amount + amount
      tempdf = trainFull[start_amount:end_amount]
      s_model = train_single_part(tempdf,type)
      models <- append(models,s_model)
      predictions = predict(s_model, test)
      ll = predictions$score(measure)
      combloglosses <- append(combloglosses, list(1/unname(ll)))

      ensll <- c(ensll, f_logloss(predictions))

      if( i == 0){
        full_df = predictions$prob[,1]
      }else{
        full_df = cbind(full_df,predictions$prob[,1])
      }

      start_amount = end_amount
      i =  i+1
    }
    
    # write.csv(full_df, predFiles, row.names=FALSE)
    # 
    pbapply::pboptions(type = "txt", style = 3)

    #calculating Shapley values
    old <- Sys.time() # get start time
    paralelLogLosses <- pblapply(combinations,function(m) paralelMeanLogloss(full_df, m, labels))

    combloglosses <- append(combloglosses, paralelLogLosses)
    #print(combloglosses)
    mu <- set.func(as.numeric(combloglosses))
    ShapeyKl = kappalab::Shapley.value(mu)
    new <- Sys.time() - old # calculate difference
    writeLines(as.character(new), timefile)
    print(new)
    # ShapSignRemoved = c()
    # ShapElementRemoved = c()
    # ElementsToInvert = c()
    # i = 1
    # for(wght in ShapeyKl){
    #   if(wght < 0){
    #     ElementsToInvert= append(ElementsToInvert,FALSE)
    #     ShapSignRemoved= append(ShapSignRemoved,-1* wght)
    #     ShapElementRemoved= append(ShapElementRemoved, 0*wght)
    #   }else{
    #     ElementsToInvert= append(ElementsToInvert,TRUE)
    #     ShapSignRemoved= append(ShapSignRemoved,wght)
    #     ShapElementRemoved = append(ShapElementRemoved,wght)
    #   }
    #   i =  i+1
    # }

    i = 0
    for(amount in unlist(list)){
      s_model = models[[i+1]]
      val_predictions = predict(s_model, test_reserved)
      if( i == 0 ){
      validation_df = val_predictions$prob[,1]
      #   if(ElementsToInvert[i+1]){
      #     inversed_V_DF = val_predictions$prob[,1]
      #   }else{
      #     inversed_V_DF = val_predictions$prob[,2]
      #   }
      }else{
      validation_df = cbind(validation_df,  val_predictions$prob[,1])
      #   if(ElementsToInvert[i+1]){
      #     inversed_V_DF = cbind(inversed_V_DF,  val_predictions$prob[,1])
      #   }else{
      #     inversed_V_DF = cbind(inversed_V_DF, val_predictions$prob[,2])
      }
      i =  i+1
    }
   
    
    rozFileName = paste("/home/vdledger/PycharmProjects/AproxShapPy3/shapley-master/examples/HomoResults/Bank",modeltype,ensNr,'Shap',val,'.csv', sep="", collapse=NULL)
    RozWeights=read.csv(rozFileName,check.names=FALSE, header=FALSE)
    RzW = as.vector(as.numeric(RozWeights))
    validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    RzShap = wmeanll(validation_df, validationLabels, RzW)
    EQll = meanlogloss(validation_df, validationLabels)
    RozName = paste("Roz",ensNr, sep="", collapse=NULL)
    RozLine = paste(RozName,val,RzShap, sep=",", collapse=NULL)
    writeLines(c(RozLine), fileEns)
    writeLines(c(paste(ensNr,fileType,"R",dataFile,"Roz",RzShap, sep=",", collapse=NULL)),fileResult)
    
    #write.csv(validation_df, predFiles, row.names=FALSE)
  }
  print(ensNr)
  close(fileEns)


    #creating log loss weights

    # wll <- as.vector(ensll[-1])
    # wll <- 1/wll
    #print(wll)

    #Monolith result calculation

    # measure = msr("classif.logloss")
    # MONO = Mono$score(measure)

    #weighted result calculation

    #print(unname(MONO))

    validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    # 
    EQll = meanlogloss(validation_df, validationLabels)
    print(EQll)
    # RandomWLL = wmeanll(validation_df, validationLabels, rweights)
    # #print(RandomWLL)
    # Wll = wmeanll(validation_df, validationLabels, wll)
    # print(Wll)
    # INVWllShap = wmeanll(inversed_V_DF, validationLabels, ShapSignRemoved)
    # print(INVWllShap)
    # ZeroShap = wmeanll(validation_df, validationLabels, ShapElementRemoved)
    # print(ZeroShap)
    # RzShap = wmeanll(validation_df, validationLabels, RzW)
    # print(RzShap)


    # Writing results to file
    # MonoName = paste("Mono",ensNr, sep="", collapse=NULL)
    # MonoLine = paste(MonoName,val,MONO, sep=",", collapse=NULL)
    # EQName = paste("EQ",ensNr, sep="", collapse=NULL)
    # EQLine = paste(EQName,val,EQll, sep=",", collapse=NULL)
    # LLName = paste("LL",ensNr, sep="", collapse=NULL)
    # LLLine = paste(LLName,val,Wll, sep=",", collapse=NULL)
    # InvShapName = paste("INVSH",ensNr, sep="", collapse=NULL)
    # InvShapLine = paste(InvShapName,val,INVWllShap, sep=",", collapse=NULL)
    # ZeroShapName = paste("ZEROSH",ensNr, sep="", collapse=NULL)
    # ZeroShapLine = paste(ZeroShapName,val,ZeroShap, sep=",", collapse=NULL)
    # RandName = paste("RAND",ensNr, sep="", collapse=NULL)
    # RandLine = paste(RandName,val,RandomWLL, sep=",", collapse=NULL)
    # RozName = paste("ROZEMB",ensNr, sep="", collapse=NULL)
    # RozLine = paste(RozName,val,RzShap, sep=",", collapse=NULL)
    
   # writeLines(c(MonoLine), fileEns)
   # writeLines(c(EQLine), fileEns)
   # writeLines(c(LLLine), fileEns)
   # writeLines(c(RandLine), fileEns)
   # # writeLines(c(InvShapLine), fileEns)
   # writeLines(c(ZeroShapLine), fileEns)
   # writeLines(c(RozLine), fileEns)
   #  
    # writeLines(c(paste(ensNr,val,fileType,"R",dataFile,MONO,Wll,INVWllShap,RandomWLL,EQll, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,MONO,Wll,INVWllShap,RandomWLL,EQll, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Mono",MONO, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Perf",Wll, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"invShap",INVWllShap, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"zeroShap",ZeroShap, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Rand",RandomWLL, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Equal",EQll, sep=",", collapse=NULL)),fileResult)
  }
  # close(fileEns)
  new <- Sys.time() - old # calculate difference
  print(new)
close(timefile)
close(fileResult)

  
 
