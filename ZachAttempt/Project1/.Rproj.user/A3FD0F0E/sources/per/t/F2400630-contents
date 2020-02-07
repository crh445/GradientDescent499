gradDescMain <- function(maxIterations, stepSize)
{
  
    # library calls
    library(ggplot2)
  
    # source file compilations
    source("gradientDescent.R")
    source("fixData.R")
    source("calcError.R")
  
    #load data in from the file
    spam <- read.table("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", quote="\"", comment.char="")
    #####
    #set maxIterations
    
    #pass spam into the fixdata function to properly segment it
    #dataList indicies: [[1]] = test, [[2]] = train, [[3]] = validation
    dataList <- fixData(spam)
    
    testData <- dataList[[1]]
    testOutput <- dataList[[4]]
    
    trainData <- dataList[[2]]
    trainOutput <- dataList[[5]]
    
    validData <- dataList[[3]]
    validOutput <- dataList[[6]]
    
    #use gradient descent on train data first to get a weight matrix
    trainWeightMatrix <- gradientDescent(trainData, trainOutput, stepSize, maxIterations)
    
    # multiply train and validation inputs by weightmatrix to get predicitions
    
    # pred one is the prediction matrix based on train data
    pred1 <- trainData %*% trainWeightMatrix 
    
    # pred two is the prediction matrix based on validation data
    pred2 <- validData %*% trainWeightMatrix
    
    
    
    error1.data <- calcError(trainWeightMatrix, trainData, trainOutput, maxIterations)
    error2.data <- calcError(trainWeightMatrix, validData, validOutput, maxIterations)
    ggplot() + 
      geom_line(error2.data, colour = "red", mapping = aes(x = iterVector, y = errorVector)) +
      geom_line(error1.data, colour = "black", mapping = aes(x = iterVector, y = errorVector)) 
      
    
    #return(errorVector)
    
    #compute mean logistic loss
    logisticLossTrain <- logisticLossFunction(trainData, trainOutput, maxIterations, trainWeightMatrix)
    logisticLossValid <- logisticLossFunction(validData, validOutput, maxIterations, trainWeightMatrix)
    ggplot() +
     geom_line(logisticLossTrain, colour = "black", mapping = aes(x = returnVector2, y = returnVector)) +
    geom_line(logisticLossValid, colour = "red", mapping = aes(x = returnVector2, y = returnVector))
      
    
}


logisticLossFunction <- function(X, y, maxIterations, heavyRain)
{
  # this is equivalent to y~ in our notes
  yNew <- y
  
  # revector the retard wanted
  returnVector <- numeric(maxIterations)
  returnVector2 <- (1:maxIterations)

  # convert all 0's to -1's
  yNew[yNew==0]<-(-1)
  returnVal <- 0
  
  for(pp in 1:maxIterations)
  {
    theta <- heavyRain[,pp]

    for(iter in 1: nrow(X))
    {
      xTheta <-  t(X[iter,]) %*% theta

      returnVal <- returnVal + log(1 + exp(-yNew[iter] * xTheta[1,1]))
    } 
    returnVector[pp] <- (returnVal/nrow(X))
    returnVal <- 0
  }
  logloss.data <- data.frame(returnVector, returnVector2)
  return(logloss.data)
}

