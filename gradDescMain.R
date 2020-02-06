gradDescMain <- function(maxIterations)
{
  
    source("gradientDescent.R")
    source("fixData.R")
    #load data in from the file
    spam <- read.table("D:/_ClassesSpring2020/Deep Learning/code/project_1/spam.data", quote="\"", comment.char="")
  
    #set maxIterations
    
    #pass spam into the fixdata function to properly segment it
    #dataList indicies: [[1]] = test, [[2]] = train, [[3]] = validation
    dataList <- fixData(spam)
    
    trainData <- dataList[[2]]
    trainOutput <- dataList[[5]]
    
    validData <- dataList[[3]]
    
    #use gradient descent on train data first to get a weight matrix
    #step size = .00005
    #maxIterations = 200
    trainWeightMatrix <- gradientDescent(trainData, trainOutput, .00005, maxIterations)
    
    # multiply train and validation inputs by weightmatrix to get predicitions
    
    # pred one is the prediction matrix based on train data
    
    #pred1 <- matrix(nrow = nrow(trainData), ncol = maxIterations)
    pred1 <- trainData %*% trainWeightMatrix 
    
    # pred two is the prediction matrix based on validation data
    
    #pred2 <- matrix(nrow = nrow(validData), ncol = maxIterations)
    # print(dim(pred1))
    # print(pred1)
    
    errorVector <- numeric(maxIterations)
    
    numWrong <- 0
    
    
    for(i in 1: maxIterations)
    {
      for(j in 1:nrow(pred1))
      {
        if(pred1[j,i] < 0)
        {
          conversion <- 0
        }
        else
        {
          conversion <- 1
        }
        if(conversion != trainOutput[j])
        {
          numWrong <- numWrong + 1
        }
        
      }
      #print(numWrong)
      errorPerc <- numWrong / nrow(pred1)
      #print(errorPerc)
      errorVector[i] <- errorPerc
      numWrong <- 0
    }
    plot(1:length(errorVector), errorVector)
    return(errorVector)
    
}
