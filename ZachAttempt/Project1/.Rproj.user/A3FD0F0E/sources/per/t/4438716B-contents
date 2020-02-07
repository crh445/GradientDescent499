fixData <- function(inputData){
  
  #inputData comes in as a list of vectors
  unscaledMatrix <- t(matrix(unlist(inputData), byrow = TRUE, nrow=length(inputData)))
  #print(unscaledMatrix[3,3])
  scaledMatrix <- scale(unscaledMatrix, TRUE, TRUE)
  #print(scaledMatrix)
  
  #set seed ()
  set.seed(6)
  
  
  #take 60% sample of scaled matrix
  trainRows <- sample.int(n = nrow(unscaledMatrix), size = floor(.6*nrow(unscaledMatrix)), replace = FALSE)
  trainMatrix <- unscaledMatrix[trainRows,]
  output <- unscaledMatrix[,ncol(scaledMatrix)]
  
  
  #declare matrix with the remaining data
  remMatrix <- unscaledMatrix[-trainRows,]
  
  #take 50% test sample of remaining matrix
  testRows <- sample.int(n = nrow(remMatrix), size = floor(.5*nrow(remMatrix)), replace = FALSE)
  testMatrix <- remMatrix[testRows,]
  
  #take 50% validation sample of remaining matrix
  validMatrix <- remMatrix[-testRows,]
  
  displayMatrix <- matrix(1:6, nrow = 3, ncol = 2)
  
  #get number of 1 occurrences for display matrix
  trainOnes <- sum(trainMatrix[,ncol(trainMatrix)])
  validOnes <- sum(validMatrix[,ncol(validMatrix)])
  testOnes <- sum(testMatrix[,ncol(testMatrix)])
  #print(testMatrix[,ncol(testMatrix)])
  
  rownames(displayMatrix) <- c("test", "train", "validation")
  colnames(displayMatrix) <- c(0, 1)
  
  displayMatrix[1,1] <- nrow(testMatrix) - testOnes
  displayMatrix[1,2] <- testOnes
  displayMatrix[2,1] <- nrow(trainMatrix) - trainOnes
  displayMatrix[2,2] <- trainOnes
  displayMatrix[3,1] <- nrow(validMatrix) - validOnes
  displayMatrix[3,2] <- validOnes
  
  print(displayMatrix)
  
  scaledValid <- scale(validMatrix, TRUE, TRUE)
  scaledTrain <- scale(trainMatrix, TRUE, TRUE)
  scaledTest <- scale(testMatrix, TRUE, TRUE)
  
  returnValid <- scaledValid[1:nrow(scaledValid), 1:ncol(scaledValid) - 1]
  returnTrain <- scaledTrain[1:nrow(scaledTrain), 1:ncol(scaledTrain) - 1]
  returnTest <- scaledTest[1:nrow(scaledTest), 1:ncol(scaledTest) - 1]
  validOutput <- validMatrix[,ncol(validMatrix)]
  testOutput <- testMatrix[,ncol(testMatrix)]
  trainOutput <- trainMatrix[,ncol(trainMatrix)]
  
  
  #don't @ me I'll clean it up later
  returnList <- list(returnTest, returnTrain, returnValid, testOutput, trainOutput, validOutput)
  return(returnList)
  
}