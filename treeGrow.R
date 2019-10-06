
##############################################################################################################################
######################################################Classification tree#####################################################
##############################################################################################################################
rm(list=ls())
#############
###Library###
#############
library(reshape2)
library(compare)
library(dplyr)
library(caret)
library(mlbench)
###Data###

###############
###Tree grow###
###############
# myEclipse <- read_delim("eclipse-metrics-packages-2.0.csv", ";", 
#                         escape_double = FALSE, trim_ws = TRUE)


diabetes <- read.csv("diabetes.csv")

x <- diabetes
y <- x[,c(9)]
x <- x[-c(9)]
nmin <- 20
minleaf <- 5
nfeat <- 8

###Short preprocessing###
# myEclipse <- as.data.frame(myEclipse)
# y <- myEclipse[, c(4)]
# x <- myEclipse[-c(1,2,4)]
# minleaf <- 5
# nmin <- 15
# nfeat <- 41
# y[y>0] <- 1

myData <- read.csv("credit.txt")
x <- myData[,c(1:ncol(myData)-1)]
y <-  myData[,ncol(myData)]
minleaf <- 1
nmin <- 2
nfeat <- 5

tree.grow <- function(x, y, nmin, minleaf, nfeat){
  
  ###If the length is 1, it means it is a pure node###
  if(length(unique(y))==1){
    #print("The node is pure, leaf node!")
    return(table(y))
  }
  ###If the dataSet is smaller than nmin, we skip that one###
  if(nrow(x)<nmin){
    #print("smaller than nmin, leaf Node!")
    return(table(y))
  }
  
  #################################################
  ###Getting the best split
  ###Returning the position of the best variable
  ###Returning the average value of the variable###
  #################################################
  splitValues <- bestSplit(x, y, nfeat, minleaf)
  
  ###If value is zero it means it can't be split, minleaf is higher than all the possible splits###
  if(splitValues[1]==0){
    return(table(y))
  }
  ###Using the split values we divide the dataset into left and right nodes###
  nodes <- nodeSplit(x, y, splitValues)
  
  ###Printing question for fanciness###
  #que <- question(x, splitValues)
  
  #################
  ###Left branch###
  #################
  
  ###The class of the left branch###
  y <- nodes[[1]][ncol(nodes[[1]])]
  y <- y[[1]]
  
  ###The dataset of the left branch###
  x <- nodes[[1]][1:(ncol(nodes[[1]])-1)] 

  ###Recursion through the left branch, until we hit a leaf###
  leftBranch <- tree.grow(x, y, nmin, minleaf, nfeat)
  
  ##################
  ###Right branch###
  ##################
  
  ###The class of the right branch###
  y <- nodes[[2]][ncol(nodes[[2]])]
  y <- y[[1]]
  
  ###The dataset of the right branch###
  x <- nodes[[2]][1:(ncol(nodes[[2]])-1)] 
  
  ###Recursion through the right branch, until we hit a leaf###
  rightBranch <- tree.grow(x, y, nmin, minleaf, nfeat)
  
  return(list(SPLIT = splitValues, "Left branch" = leftBranch, "Right branch" = rightBranch))
}


######################################

########################
###Optimal tree split###
########################

###Returns the subsets of node, left and right###
nodeSplit <- function(dataSet, y, splitValues){
  
  ###x[1] is the variable position to split
  ###x[2] is the value that splits the dataframe###
  
  dataSet$y <- y
  left <- dataSet[dataSet[splitValues[1]] >= splitValues[2],]
  right <- dataSet[dataSet[splitValues[1]] < splitValues[2],]
  
  return(list(left,right))
}



############################
###Optimal split function###
############################


###testing###
# x <- myData$income
# y <- myData$class

###Pick the best number based on impurity###
###Returns the following: 
###the position of the variable where it needs to be split
###The average number of the variable that is used to split###
bestSplit <- function(dataSet, y, nfeat, minleaf){
  t <- table(y)

  ###gini index###
  iT <- t[[1]]/length(y) * (1 - t[[1]]/length(y))
  
  ###The final quality value###
  checker <- 0
  
  ###The final optimal split average of the variable###
  optimalSplit <- 0
  
  ###Create a x with the atttributes###
  x <- c(1:ncol(dataSet))

  ###pick the best variable out of a few random picks###
  randomNodes <- sample(x, nfeat)
  
  varSplit <- 0
  for (i in 1:length(randomNodes)) {
    
    ###If there is only one value(aka pure node) we skip###
    if(length(unique(dataSet[[randomNodes[i]]]))>1){

      sortX <- sort(unique(dataSet[[randomNodes[i]]]))
      ###Sort the variable######Optimal value and position###
      for (j in 1:(length(sortX)-1)) {
        
        ###The number where we split the class###
        split <- (sortX[j] + sortX[j+1])/2
        
        ###Merge###
        merging <- data.frame(x = dataSet[[randomNodes[i]]], y = y)
        left <- merging[merging$x <split,]
        right <- merging[!(merging$x %in% left$x),]
        
        ###Get the quality value###
        quality <- iT - (nrow(left)/nrow(dataSet) * sum(left$y)/nrow(left) * (1 - sum(left$y)/nrow(left)) + 
                           nrow(right)/nrow(dataSet) * sum(right$y)/nrow(right) * (1 - sum(right$y)/nrow(right)))
        

        ###If the quality is higher than the previous one, replace it. If not, then we go to the next one.
        if (checker < quality) {
          if(nrow(left) >= minleaf && nrow(right)>=minleaf){
            checker <- quality
            optimalSplit <- split
            varSplit <- randomNodes[i]
            #print(dataSet[varSplit])
          }
          else{
            next
          }
        }
        else{
          next
        }
      }
    }
    
  }

  return(c(varSplit, optimalSplit))
}

###Just for clarity and more fanciiness, we change the values into a question###
question <- function(x, splitValues){
  print(paste("Is",colnames(x[splitValues[1]]), "smaller than", splitValues[2], "?"))
}



###Classifying the test data and returns a row of output###
tree.classify <- function(x, tr){
  indexing <- 0
  ###looping through the rows of the dataset###
  for (i in 1:nrow(x)) {
    
    ###Get the row###
    rowNumber = x[i,]
    
    ###Use the function to reach the leaf node and return the result (0 or 1)###
    result <- goTree(rowNumber, tr)
    indexing[i] <- result
    
  }
  return(indexing)
}

###If the node is a leaf node, we calculate whether it's a 1 or 0. If it's a pure node the value is obvious of course###
###Input: table###
leafNode <- function(t){
  
  ###If length is 1, it means it is a pure node###
  if(length(t)==1){
    if(names(t)[1]=="0"){
      return(0)
    }
    else{
      return(1)
    }
  }
  else{
    ###Calculate the probability of zero and one, then compare which one has a higher probability and return that value###
    zero <- t[[1]]/(t[[1]]+t[[2]])
    #print(zero)
    one <- 1 - zero
    if(zero<one){
      return(1)
    }
    else{
      return(0)
    }
  }
}


###GOGOGO! Here we go through the tree until we find a leaf, used in tree.classify function###
goTree <- function(rowNumber, tr){
  ###If it is a table, it is a leaf###
  if(class(tr)=="table"){
    return(leafNode(tr))
  }
  
  if(rowNumber[[tr[[1]][1]]] > tr[[1]][2]){
    return(goTree(rowNumber, tr[[2]]))
  }
  else{
    return(goTree(rowNumber, tr[[3]]))
  }
}

###Make the tree more fancy, god bless stack overflow###
nameTree <- function(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = ""){
  if( is.list(X) )
    for( i in seq_along(X) ) { 
      cat( if(i<length(X)) prefix1 else prefix3, names(X)[i], "\n", sep="" )
      prefix <- if( i<length(X) ) prefix2 else prefix4
      nameTree(
        X[[i]], 
        paste0(prefix, "├──"),
        paste0(prefix, "│  "),
        paste0(prefix, "└──"),
        paste0(prefix, "   ")
      )
    }
}


###Consufion matrix###
tr <- tree.grow(x, y, nmin, minleaf, nfeat)
prediction <- tree.classify(x, tr)
actual <- as.factor(y)
prediction <- as.factor(prediction)
confusionMatrix(actual, prediction)




###Bagging tree###
tree.grow.bag <- function(x, y, nmin, minleaf, nfeat = ncol(x)- 1, m){
  
  treeList <- c()
  x$y <- y
  for (i in 1:m) {
    print(i)
    ########################
    ###Processing dataset###
    ###Create a random sample###
    baggingData <- x[sample(nrow(x), nrow(x), replace = TRUE),]
    ###Get class and remove the class from the dataset###
    y <- baggingData$y 
    baggingData$y <- NULL
    ########################
    #print(baggingData)
    tree <- tree.grow(baggingData, y, nmin, minleaf, nfeat = ncol(x)-1)  
    treeList[[i]] <- tree
    
  }
  return(treeList)
  
}

tree.classify.bag <- function(x, treeList){
  indexing <- 0
  
  
  ###looping through the rows of the dataset###
  for (i in 1:nrow(x)) {

    ###Get the row###
    rowNumber = x[i,]
    
    ###Get all the outputs of the trees###
    recording <- c()
    
    ###Go through the trees###
    for (j in 1:length(treeList)) {
      result <- goTree(rowNumber, treeList[[j]])
      recording[[j]] <- result
      
    }
    
    ###Which output occurs the most###
    #output <- as.numeric(names(sort(table(recording),decreasing=TRUE))[1])
    output <- leafNode(table(recording))
    indexing[i] <- output
    
  }
  return(indexing)
}





trBag <- tree.grow.bag(x, y, nmin, minleaf, nfeat = ncol(x)-1, 100)
prediction <- tree.classify.bag(x, trBag)
actual <- as.factor(y)
prediction <- as.factor(prediction)
confusionMatrix(actual, prediction)
