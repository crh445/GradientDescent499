calcError <- function(weightMatrix, X, y, maxIterations)
{
  errorVector <- numeric(maxIterations)
  
  pred = X %*% weightMatrix
  print(dim(pred))
  
  numWrong <- 0
  
  for(i in 1: maxIterations)
  {
    for(j in 1:nrow(pred))
    {
      if(pred[j,i] < 0.5)
      {
        conversion <- 0
      }
      else
      {
        conversion <- 1
      }
      if(conversion != y[j])
      {
        numWrong <- numWrong + 1
      }
      
    }
    #print(numWrong)
    errorPerc <- numWrong / nrow(pred)
    #print(errorPerc)
    errorVector[i] <- errorPerc
    numWrong <- 0
  }
  
  iterVector <- (1:maxIterations)
  
  error.data <- data.frame(errorVector, iterVector)
  return(error.data)
}