# X = a matrix of real numbers 
  # num of columns = num of features
  # num of rows = num of observations
# Y vector of real number outputs [0,1]
# stepSize = real number (likely between 0 and 1)
# maxIterations = how many iterations the function should run
gradientDescent <- function(X, y, stepSize, maxIterations)
{
  
  # initialize weight vector to vector of zeroes
  #weightVector <- numeric(ncol(X))
  weightVector <- matrix(data = 0, nrow = ncol(X), ncol = 1)
  
  # weight matrix declaration 
    # num of columns = 1
    # num of rows = num of features
  weightMatrix <- matrix(0, nrow = ncol(X), ncol = maxIterations)
  
  
  # start at iteration 2 as this is the first weight vector we calculate
  for(iter in 2:maxIterations)
  {
      # calculate the gradient with the previous weight vector
      currGradient <- calcGradient(X, y, weightMatrix[,iter - 1])
      
      # calculate new weight vector
      weightVector <- weightMatrix[,iter - 1] - (stepSize * currGradient)
      
      # set corresponding column of weightMatrix
      weightMatrix[,iter] <- weightVector
  }
  
  return(weightMatrix)
}


calcGradient <- function(X, y, theta)
{
  # this is equivalent to y~ in our notes
  yNew <- y
  
  # convert all 0's to -1's
  yNew[yNew==0]<-(-1)
  
  returnVal <- numeric(ncol(X))

  for(iter in 1: nrow(X))
  {
    # calculate theta^T * x^i
    xTheta <- t(theta) %*% X[iter,]
    
    #multiply first scalar
    scal1 <- (1 / ( 1 + exp(-yNew[iter] * xTheta[1,1])))
    #print(scal1)
    
    #multiply second scalar
    scal2 <- exp(-yNew[iter] * xTheta[1,1])
    #print(scal2)
    
    #multiply vector
    vec1 <- -yNew[iter] * X[iter,]

    returnVal <- returnVal + ((scal1 * scal2) * vec1)
  }          
  returnVal <- returnVal / (nrow(X))
  return(c(returnVal))
  
}