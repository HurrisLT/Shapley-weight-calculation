library("mlr3")
library("mlr3measures")
library("mlr3learners")
library("data.table")
library("data.table")
library("random")
library("matrixStats")
library("cooptrees") 
smth = shapleyValue(5,v=c(3.61828,3.645632,3.462863,2.984388,3.309631,3.656636,3.675305,3.747792,3.584127,3.684355,3.751967,3.597933,3.687878,3.617144 ,3.632219 ,3.708707,3.781698 ,3.639838 ,3.776938 ,3.686563 ,3.735844 ,3.779732 ,3.694282 ,3.742071 ,3.740581 ,3.793972 ,3.708693 ,3.761261 ,3.775121 ,3.779562,3.785601
))
print(smth)


filename = paste('/home/vdledger/PycharmProjects/StatisticalAnalysis/res.csv', sep="", collapse=NULL)
filetemp = file(filename, 'w')
close(filetemp)


meanlogloss <- function(comb_df, labels) {
  meansDF = data.frame(Means=rowMeans(comb_df))
  
  meansDF$'1' <- meansDF$Means 
  meansDF$'0' <- with(meansDF, 1-Means)
  
  keeps <- c("1", "0")
  meansDF = meansDF[keeps]
  probMatrix = as.matrix(meansDF, rownames.force = NA)
  
  logloss = logloss(labels, probMatrix)
  return(logloss)
}

f_logloss <-function(prediction){
  measure = msr("classif.logloss")
  loglossMono = prediction$score(measure)
  return(unname(loglossMono))
}


weightmeanlogloss <- function(comb_df, predictions, weigths) {
  
  meansDF = data.frame(Means=rowWeightedMeans(as.matrix(comb_df), weigths))
  
  meansDF$'1' <- meansDF$Means 
  meansDF$'0' <- with(meansDF, 1-Means)
  
  keeps <- c("1", "0")
  meansDF = meansDF[keeps]
  
  probMatrix = as.matrix(meansDF, rownames.force = NA)
  logloss = logloss(predictions, probMatrix)
  
  return(logloss)
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
  learner$train(train)
  prediction = learner$predict(test)
  
  return(prediction)
}


#list <- c(24825,6349,2792,1563,985,691,513,427,282,229) Adult
#list <- c(485878,121521,53961,30290,19535,13657,9980,7663,6017,4885) CreditA
#list <- c(23331,5849,2483,1492,934) #bank 5 
#list <- c(22927,5555,2563,1420,926,673,470,372,286,230,187,149,141,131,105)#bank 15
list <- c(23220,5973,2534,1456,901,646,467,386,286,246) #bank

fileEns = file(filename, 'a')

listlen = length(list)

fpath = "/home/vdledger/PycharmProjects/DataConverting/transformeddata/bankmark.csv"
data = fread(fpath)
type = "classif.log_reg"
fileType = "DT"
path = "/home/vdledger/PycharmProjects/EEGDataManagement/ZipfData/Outputs/"

temprweigths <- 1
for(num in list){
  temprweigths <- c(temprweigths, runif(1, min = 0, max = 1))
}
rweights <- temprweigths[-1] 

for(val in 1:101){
  
  set.seed(val *54346)
  drows <- sample(nrow(data))
  shufdata <- data[drows,]
  shufdata <- shufdata[0:45210]
  
  splittt<- sample(c(rep(0, 0.8 * nrow(shufdata)), rep(1, 0.2 * nrow(shufdata))))
  trainFull <- shufdata[splittt == 0,] 
  testBlock <- shufdata[splittt == 1,] 
  split<- sample(c(rep(0, 0.8 * nrow(trainFull)), rep(1, 0.2 * nrow(trainFull))))
  trainFull <- trainFull[0:36167]
  test <- trainFull[split == 1,]
  
  Mono <- train_part(trainFull, testBlock,type)
  testpath = paste(path,"test",val,".csv",  sep="", collapse=NULL)
  write.table(testBlock,testpath, row.names = FALSE, col.names=FALSE, sep=",") 
  
  labels = factor(testBlock$V1, levels=c(1, 0))
  
  rows <- sample(nrow(trainFull))
  shufTrain <- trainFull[rows,]
  
  enslllr <- 1

  start_amount = 0
  end_amount = 0
  i = 0
  for(amount in list){
    end_amount = start_amount + amount
    tempdf = shufTrain[start_amount:end_amount]
    temppred = train_part(tempdf, test,type)
    enslllr <- c(enslllr, f_logloss(temppred))
    }
  
  for(amount in list){
    end_amount = start_amount + amount
    tempdf = shufTrain[start_amount:end_amount]
    temppred = train_part(tempdf, testBlock,type)
    if( i == 0){
      combined_df = temppred$prob[,1]
    }else{
      combined_df = cbind(combined_df,temppred$prob[,1])
    }
    start_amount = end_amount
    i =  i+1
  }
  
  wlllr <- as.vector(enslllr[-1])
  wlllr <- 1/ wlllr
  
  ShapWheightFile = paste("/home/vdledger/Desktop/BankShapley/resultsLR10/","ShapleyValues",val,"LL.csv",  sep="", collapse=NULL)
  Weights = fread(ShapWheightFile)
  ws = as.vector(Weights$V2)
  

  measure = msr("classif.logloss")
  MONO = Mono$score(measure)

  LREQll = meanlogloss(combined_df, labels)
  LRWll = weightmeanlogloss(combined_df, labels, wlllr)
  LRWllShap = weightmeanlogloss(combined_df, labels, ws)
  LRrand = weightmeanlogloss(combined_df, labels, rweights)
  esamblerRes = paste(toString(unname(MONO)),toString(unname(LREQll)),toString(unname(LRWll)),toString(unname(LRWllShap)),toString(unname(LRrand)), sep=",", collapse=NULL)
  print(esamblerRes)

  
  MonoName = paste("Mono",listlen, sep="", collapse=NULL)
  MonoLine = paste(MonoName,val,MONO, sep=",", collapse=NULL)
  EQName = paste("EQ",listlen, sep="", collapse=NULL)
  EQLine = paste(EQName,val,LREQll, sep=",", collapse=NULL)
  LLName = paste("LL",listlen, sep="", collapse=NULL)
  LLLine = paste(LLName,val,LRWll, sep=",", collapse=NULL)
  ShapName = paste("SH",listlen, sep="", collapse=NULL)
  ShapLine = paste(ShapName,val,LRWllShap, sep=",", collapse=NULL)
  RandName = paste("RAND",listlen, sep="", collapse=NULL)
  RandLine = paste(RandName,val,LRrand, sep=",", collapse=NULL)
  
  writeLines(c(MonoLine), fileEns)
  writeLines(c(EQLine), fileEns)
  writeLines(c(LLLine), fileEns)
  writeLines(c(ShapLine), fileEns)
  writeLines(c(RandLine), fileEns)

}
close(filetemp)





