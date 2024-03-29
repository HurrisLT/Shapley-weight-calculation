library("mlr3")
library("mlr3measures")
library("mlr3learners")
library("data.table")
library("data.table")
library("random")


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

f_auc <-function(prediction){
  auc = msr("classif.auc")
  aucP = prediction$score(auc)
  return(unname(aucP))
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
#list <- c(23220,5973,2534,1456,901,646,467,386,286,246) #bank 10
list <-c(22845, 5847, 2614, 1407, 891, 691, 465, 354, 294, 232, 208, 152, 142) #bank 13
#list <- c(22550, 5667, 2601, 1364, 859, 632, 500, 355, 279, 248, 198, 146, 140, 118, 79, 85, 76, 74, 47, 63, 47)#bank 21


fpath = "/home/vdledger/PycharmProjects/DataConverting/transformeddata/bankmark.csv"
data = fread(fpath)
type = "classif.rpart"
fileType = "DT"
path = "/home/vdledger/PycharmProjects/EEGDataManagement/ZipfData/Outputs/"

temprweigths <- 1
for(num in list){
  temprweigths <- c(temprweigths, runif(1, min = 0, max = 1))
}
rweights <- temprweigths[-1] 

for(val in 1:101){
print(val)

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

testpath = paste(path,"test",val,".csv",  sep="", collapse=NULL)
write.table(test,testpath, row.names = FALSE, col.names=FALSE, sep=",") 

labels = factor(test$V1, levels=c(1, 0))

rows <- sample(nrow(trainFull))
shufTrain <- trainFull[rows,]

enslllr <- 1
enslldt <- 1
ensauclr <- 1
ensaucdt <- 1

start_amount = 0
end_amount = 0
i = 0
measure = msr("classif.logloss")
for(amount in list){
  end_amount = start_amount + amount
  tempdf = shufTrain[start_amount:end_amount]
  #temppred = train_part(tempdf, test, "classif.rpart")
  temppred = train_part(tempdf, test,type)
  ll = temppred$score(measure)
  ensauclr <- c(ensauclr, f_auc(temppred))
  enslllr <- c(enslllr, f_logloss(temppred))
  if( i == 0){
    combined_df = temppred$prob[,1]
  }else{
    combined_df = cbind(combined_df,temppred$prob[,1])
  }
  start_amount = end_amount
  i =  i+1
  
  fullpath = paste(path,"output",fileType,val,".csv",  sep="", collapse=NULL)
  write.csv(combined_df,fullpath, row.names = TRUE)
}

wlllr <- as.vector(enslllr[-1])
wlllr <- 1/ wlllr

wauclr <- as.vector(ensauclr[-1])

#newauc <- 1
#for(auc in wauclr){
  #t = auc - 0.5
#  newauc <-c(newauc, t)
#}
#wauclr <- as.vector(wauclr[-1])

#  ShapWheightFile = paste("/home/vdledger/Desktop/BankShapley/resultsDT5/","ShapleyValues",val,"LL.csv",  sep="", collapse=NULL)
#  Weights = fread(ShapWheightFile)
#  ws = as.vector(Weights$V2)
# 
#  LREQll = meanlogloss(combined_df, labels)
#  LRWll = weightmeanlogloss(combined_df, labels, wlllr)
#  LRWllShap = weightmeanlogloss(combined_df, labels, ws)
#  LRrand = weightmeanlogloss(combined_df, labels, rweights)
# 
#   esamblerRes = paste(toString(unname(LREQll)),toString(unname(LRWll)),toString(unname(LRWllShap)),toString(unname(LRrand)), sep=",", collapse=NULL)
# 
# # #LRWAUC = weightmeanlogloss(combined_df, labels, wauclr)
#  print(esamblerRes)
#print(LRWll)
#print(LRWAUC)
}


