# X = a matrix of real numbers 
  # num of columns = num of features
  # num of rows = num of observations
# Y vector of real number outputs [0,1]
# stepSize = real number (likely between 0 and 1)
# maxIterations = how many iterations the function should run

gradientDescent <- function(X, y, stepSize, maxIterations)
{
  # initialize weight vector to vector of zeroes
  weightVector <- numeric(ncol(X))
  # temp test prints
  
  # weight matrix declaration 
    # num of columns = 1
    # num of rows = num of features
  weightMatrix <- matrix(0,nrow = ncol(X), ncol = maxIterations)
  
  
  
  iteration <- 2
  prevWeight <- weightMatrix[,1]
  #start at iteration 2 as this is the first weight vector we calculate
  while(iteration <= maxIterations)
  {
      #calculate the gradient with the previous weight vector
      currGradient <- calcGradient(X, y, weightMatrix[,iteration - 1])

      weightVector <- as.vector(weightMatrix[,iteration - 1]) - (stepSize * currGradient)
      
      #set corresponding column of weightMatrix
      weightMatrix[,iteration] <- c(weightVector)
      
      iteration <- iteration+1
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
  #print("ynew:")
  #print(yNew)
  #print("Theta:")
  for(iter in 1: nrow(X))
  {
    
    xTheta <- t(theta) %*% X[iter,]
    #print(xTheta)
    
    #multiply first scalar
    scal1 <- (1 / ( 1 + exp(-yNew[iter] * xTheta[1,1])))
    #print(scal1)
    
    #multiply second scalar
    scal2 <- exp(-yNew[iter] * xTheta[1,1])
    #print(scal2)
    
    #multiply vector
    vec1 <- -yNew[iter] * X[iter,]
    #print(vec1)
    
    
    returnVal <- returnVal + ((scal1 * scal2) * vec1)
    
  }          
  returnVal <- returnVal / (length(returnVal))
  return(c(returnVal))
  
}