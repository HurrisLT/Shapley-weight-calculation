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

#number of ensambles to create
numberOfEnsambles <- c(2,3,5,8)

resultFilename= paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/heteroPyRoz/resfullPyBankHetero.csv', sep="", collapse=NULL)
fileResult = file(resultFilename, 'a')

for(ensNr in numberOfEnsambles){
  
  old <- Sys.time() # get start time
  
  #type = "classif.rpart"
  # type = "classif.log_reg"
  dataFile = "BNG"
  # if(type == "classif.log_reg")
  # {
  #   fileType = "LRPy"
  #   modeltype = "LR"
  # }else{
  #   fileType = "DTPy"
  #   modeltype = "DT"
  # }
  
  filename = paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/heteroPyRoz/',fileType,ensNr,'.csv', sep="", collapse=NULL)
  fileEns = file(filename, 'w')
  close(fileEns)
  fileEns = file(filename, 'a')
  writeLines('classifier_name,dataset_name,accuracy', fileEns)
  
  set.seed(307)
  
  for(val in 1:99){
    aval = val-1
    
    predDTFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBNG/',ensNr,"DT",aval,'.csv', sep="", collapse=NULL)
    predictionsDT = fread(predDTFileName)
    
    predLRFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBNG/',ensNr,"LR",aval,'.csv', sep="", collapse=NULL)
    predictionsLR = fread(predLRFileName)
    
    
    for(pnum in 1:ensNr){
      pname = paste("pred",pnum, sep="", collapse=NULL)
      pNewName = paste("pred",pnum+ensNr, sep="", collapse=NULL)
      setnames(predictionsLR, old = pname, new = pNewName)
    }
   
    print(head(predictionsDT))
    print(head(predictionsLR))
    
    predictions = cbind(predictionsDT, predictionsLR)
    
    print(head(predictions))
    
    resPredFileName_lr = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBNG/',ensNr,"reserve",'LR',aval,'.csv', sep="", collapse=NULL)
    reserved_predictions_lr = fread(resPredFileName_lr)
    
    for(pnum in 1:ensNr){
      pname = paste("pred",pnum, sep="", collapse=NULL)
      pNewName = paste("pred",pnum+ensNr, sep="", collapse=NULL)
      setnames(reserved_predictions_lr, old = pname, new = pNewName)
    }
    
    resPredFileName_dt = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBNG/',ensNr,"reserve",'DT',aval,'.csv', sep="", collapse=NULL)
    reserved_predictions_dt = fread(resPredFileName_dt)
    reserved_predictions = cbind(reserved_predictions_dt, reserved_predictions_lr)
    
    testPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBNG/',ensNr,"test",val,'.csv', sep="", collapse=NULL)
    test = fread(testPath)
    
    testReservedPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBNG/',ensNr,"test_reserved",val,'.csv', sep="", collapse=NULL)
    test_reserved = fread(testReservedPath)
    
    labels = factor(test$V1, levels=c(1, 0))
    validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    
    zipfPath =  paste('/home/vdledger/PycharmProjects/SingleModel/dataBNG/',ensNr,"zipf",'.csv', sep="", collapse=NULL)
    zipf = fread(zipfPath)
    list = zipf
    
    #calculate random weights
    temprweigths <- 1
    N = ensNr + ensNr
    print(N)
    for(num in 1:N){
      temprweigths <- c(temprweigths, runif(1, min = 0, max = 1))
    }
    rweights <- temprweigths[-1] 
    
   
    #calculate all the possible combinations
    # combinations = unlist(lapply(1:N, function(m) combn(1:N, m, simplify=F)), recursive=F)
    
    # we remove all the single digit combinations, as those are calculated differently
    # combinations = combinations[-c(1:N)]
    
    
    rgll = randomGuessingLogLoss(labels)
    
    ensll <- 1
    
    combloglosses <-list(1/rgll)
    
    start_amount = 0
    end_amount = 0
    i = 1
    
    # singleLLDTFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBankMark/',ensNr,'DT','part',aval,'.csv', sep="", collapse=NULL)
    # singleLLDT = fread(singleLLDTFileName)
    # 
    # singleLLLRFileName = paste('/home/vdledger/PycharmProjects/SingleModel/resultsBankMark/',ensNr,'LR','part',aval,'.csv', sep="", collapse=NULL)
    # singleLLLR = fread(singleLLLRFileName)
    # 
    # for(amount in unlist(list)){
    #   ll = singleLLLR$V1[i]
    #   combloglosses <- append(combloglosses, list(1/unname(ll)))
    #   pname=paste("pred",i, sep="", collapse=NULL)
    #   
    #   if( i == 1){
    #     full_df = get(pname, predictions)
    #   }else{
    #     full_df = cbind(full_df,get(pname, predictions))
    #   }
    #   i = i+1
    # }
    # j = 1
    # for(amount in unlist(list)){
    #   ll = singleLLDT$V1[j]
    #   combloglosses <- append(combloglosses, list(1/unname(ll)))
    #   pname=paste("pred",i, sep="", collapse=NULL)
    #   full_df = cbind(full_df,get(pname, predictions))
    #   i = i+1
    #   j = j+1
    # }
    
    # print(head(full_df))
    # 
    # wll = fread(singleLLLRFileName)
    # wll = 1/wll$V1
    # print(wll)
    # wllDT = fread(singleLLDTFileName)
    # wllDT = 1/wllDT$V1
    # wll = append(wll, wllDT)
    # print(wll)
    # print(as.numeric(combloglosses))
    
    # pbapply::pboptions(type = "txt", style = 3)
    # res = paralelMeanLogloss(predictions, combinations[3], labels)
    
    #calculating Shapley values
    # paralelLogLosses <- pblapply(combinations,function(m) paralelMeanLogloss(full_df, m, labels))
    # 
    
    # combloglosses <- append(combloglosses, paralelLogLosses)
    # print(as.numeric(combloglosses))
    
    # mu <- set.func(as.numeric(combloglosses))
    # ShapeyKl = kappalab::Shapley.value(mu)
    # 
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
    
    
    i = 1
    for(amount in 1:N){
      pname=paste("pred",i, sep="", collapse=NULL)
      if( i == 1){
        validation_df = get(pname, reserved_predictions)
        # if(ElementsToInvert[i]){
        #   inversed_V_DF = get(pname, reserved_predictions)
        # }else{
        #   inversed_V_DF = 1-get(pname, reserved_predictions)
        # }
      }else{
        validation_df = cbind(validation_df,get(pname, reserved_predictions))
        # if(ElementsToInvert[i]){
        #   inversed_V_DF = cbind(inversed_V_DF, get(pname, reserved_predictions))
        # }else{
        #   inversed_V_DF = cbind(inversed_V_DF, 1-get(pname, reserved_predictions))
        # }
      }
      i = i+1
    }
    
    doubleEnsNr = ensNr + ensNr
    
    rozFileName = paste("/home/vdledger/Downloads/PyShapWeights/BNG",doubleEnsNr,'Shap',val,'.csv', sep="", collapse=NULL)
    RozWeights=read.csv(rozFileName,check.names=FALSE, header=FALSE)
    RzW = as.vector(as.numeric(RozWeights))
    validationLabels = factor(test_reserved$V1, levels=c(1, 0))
    RzShap = wmeanll(validation_df, validationLabels, RzW)
    EQll = meanlogloss(validation_df, validationLabels)
    RozName = paste("Roz",doubleEnsNr, sep="", collapse=NULL)
    RozLine = paste(RozName,val,RzShap, sep=",", collapse=NULL)
    print(RozLine)
    writeLines(c(RozLine), fileEns)
    writeLines(c(paste(ensNr,fileType,"Python",dataFile,"Roz",RzShap, sep=",", collapse=NULL)),fileResult)


    #Monolith result calculation
    
    # monoPath =  paste('/home/vdledger/PycharmProjects/SingleModel/resultsBankMark/',ensNr,modeltype,'mono.csv', sep="", collapse=NULL)
    # print(monoPath)
    # MONOlist = fread(monoPath)
    # MONO = as.list(MONOlist[val])
    
    #weighted result calculation
    
    # EQll = meanlogloss(validation_df, validationLabels)
    # #print(EQll)
    # RandomWLL = wmeanll(validation_df, validationLabels, rweights)
    # #print(RandomWLL)
    # Wll = wmeanll(validation_df, validationLabels, wll)
    # #print(Wll)
    # INVWllShap = wmeanll(inversed_V_DF, validationLabels, ShapSignRemoved)
    # #print(INVWllShap)
    # ZeroShap = wmeanll(validation_df, validationLabels, ShapElementRemoved)
    # #print(ZeroShap)
    # 
    
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
    
    
    # writeLines(c(MonoLine), fileEns)
    # writeLines(c(EQLine), fileEns)
    # writeLines(c(LLLine), fileEns)
    # writeLines(c(InvShapLine), fileEns)
    # writeLines(c(ZeroShapLine), fileEns)
    # writeLines(c(RandLine), fileEns)
    
    #writeLines(c(paste(ensNr,val,fileType,"R",dataFile,MONO,Wll,INVWllShap,RandomWLL,EQll, sep=",", collapse=NULL)),fileResult)
    #writeLines(c(paste(ensNr,fileType,"R",dataFile,MONO,Wll,INVWllShap,RandomWLL,EQll, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Mono",MONO, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Perf",Wll, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"invShap",INVWllShap, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"zeroShap",ZeroShap, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Rand",RandomWLL, sep=",", collapse=NULL)),fileResult)
    # writeLines(c(paste(ensNr,fileType,"R",dataFile,"Equal",EQll, sep=",", collapse=NULL)),fileResult)  }
 
  
  new <- Sys.time() - old # calculate difference
  print(new)
  }
  close(fileEns)
}

close(fileResult)



