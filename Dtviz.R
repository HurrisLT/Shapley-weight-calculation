library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)	

fpath = "/home/vdledger/PycharmProjects/DataConverting/transformeddata/bankmarklabels.csv"
data = fread(fpath)

# we shuffle the data randomly
drows <- sample(nrow(data))
shufdata <- data[drows,]
shufdata <- shufdata[0:nrow(data)]
form <- as.formula(Class~ .)
tree.2 <- rpart(form,data = shufdata, control=rpart.control(maxDepth = 5,cp = 0.01))			# A more reasonable tree
#prp(tree.2,)                                     # A fast plot													
fancyRpartPlot(tree.2, digits=-2)				# A fancy plot from rattle



