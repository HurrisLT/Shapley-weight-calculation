library("mlr3")
library("mlr3measures")
library("mlr3learners")
library("data.table")

gen_count = 1  # for
ext_count = 5  # split
int_count = 5  # split
peer_count = 21  # split
part_run = 21 # for

meanlogloss <- function(comb_df, predictions) {
  meansDF = data.frame(Means=rowMeans(comb_df))
  meansDF$'1' = meansDF$Means
  meansDF$'0' <- with(meansDF, 1-Means) 
  keeps <- c("1", "0")
  meansDF = meansDF[keeps]
  
  probMatrix = as.matrix(meansDF, rownames.force = NA)
  logloss = logloss(predictions, probMatrix)
  return(logloss)
}

for(i in 0:adjusted_gen_count){
  gen_path = paste("/","gen",toString(i),"/", sep="", collapse=NULL)
  for(j in 0:adjusted_ext_count){
    ext_path = paste("ext",toString(j),"/", sep="", collapse=NULL)
    base_path = "/home/vdledger/PycharmProjects/EEGDataManagement/Data"
    gen_ext_path = paste(base_path,gen_path,ext_path, sep="", collapse=NULL)
    
    for(k in 0:adjusted_int_count){
      int_path = paste(gen_ext_path,"int",toString(k),"/peer",toString(peer_count),"/", sep="", collapse=NULL)
      test_ens_full = paste(int_path,"/fulltestdata/csv/fulltest.csv", sep="", collapse=NULL)
      
      dtestFull <-fread(test_ens_full)
      labels <- subset(dtestFull , select=c(V1))
      labels <- factor(labels$V1, levels=c(1, 0))
      LRMLRpath = paste(int_path,"outputs/outputLRMLR.csv",  sep="", collapse=NULL)
      LRMLR <-fread(LRMLRpath)
      LRMLR = subset(LRMLR, select=-c(V1))
      DTMLRpath = paste(int_path,"outputs/outputDTMLR.csv",  sep="", collapse=NULL)
      DTMLR <-fread(DTMLRpath)
      DTMLR = subset(DTMLR, select=-c(V1))
      LRASpath = paste(int_path,"outputs/outputLRAS.csv",  sep="", collapse=NULL)
      LRAS <-fread(LRASpath)
      LRAS = subset(LRAS, select=-c(V1))
      DTASpath = paste(int_path,"outputs/outputDTAS.csv",  sep="", collapse=NULL)
      DTAS <-fread(DTASpath)
      DTAS  = subset(DTAS , select=-c(V1))
      fullDF = cbind(LRMLR, DTMLR,LRAS, DTAS)
      ll = meanlogloss(fullDF, labels)
      print(ll)
    
  }
  }
}






