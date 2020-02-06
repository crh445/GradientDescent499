#install.packages("sigmoid")
#library(sigmoid)

###############################################################################
###############################################################################
#                              Global Constants                               #
###############################################################################
###############################################################################
maxIterations = 10000
stepSize = 0.0001

###############################################################################
###############################################################################
#                           Helper Functions                                  #
###############################################################################
###############################################################################

#Function Name: GradientDescent
#Algorithm: Use the gradient descent method to optimize weights for a set of
#           training data
#Input(s)
#X: data matrix, each row represents one obersvation, each column represents
#   a feature
#Y: data vector, each entry represents a 0 or 1 for binary classification
#stepSize: a positive real number that controls how far to step
#          in the negative gradient direction
#maxIterations: positive integer that controls how many steps to take
#Output(s): 
#weightMatrix: returns a matrix called weightMatrix of real numbers where
#        the number of rows is the number of input features
#        the number of columns is maxIterations
GradientDescent <- function( X, y, stepSize, maxIterations )
{
  #initialization
  weightMatrix <- matrix(data = 0, nrow = ncol(X), ncol = 1 )
  
  #descend maxIteration times
  for( currIter in 1:maxIterations )
  {
    #find and store current weight
    currWeight <- weightMatrix[,currIter]
    
    #given the curren weight, and the data, find the new weight after
    #one iteration of gradient descent
    newWeight <- findDescent( X, y, stepSize, currWeight )
    
    #bind the new weight vector to weightMatrix
    weightMatrix <- cbind( weightMatrix, newWeight )
  }
  
  #return weightMatrix
  weightMatrix
}

#Function Name; findDescent
#Algorithm: This will simulate and return a "single descent" in a given 
#           GradientDescent based algorithm
#Input(S):
#X: data matrix, each row represents one obersvation, each column represents
#   a feature
#Y: data vector, each entry represents a 0 or 1 for binary classification
#stepSize: a positive real number that controls how far to step
#          in the negative gradient direction
#currWeight: most updated vector of weights
#Output(s):
#newWeight: current weight vector plus a step in the direction of the
#           negative gradient
findDescent <- function( X, y, stepSize, currWeight )
{
  #calculate gradient
  gradient <- t(X) %*% ( (X %*% currWeight) - y)
  
  #find newWeight or the "descent"
  newWeight <- (-1 * gradient * stepSize) + currWeight
  
  #return newWeight
  newWeight
}

#FunctionName: findAccuracy
#Algorithm: Given test data that the machine has not seen before, and 
#           predicted weights, find the proportion of times the model
#           correctly predicts unseen output with the total number of
#           predictions
#Inputs:
#X: unseen data, each row represents an observation and each column
#   and each column represents a feature
#predWeight: predicted weight vector
#Y: Actual outputs for the unseen data, is compared to the product of
#   X and predicted weights
#Outputs: A number between 0 and 1 indicated the proportion of success
#         that our model has
#Note: This function only works for logistical regression because and
#      probably wont do much outside of the scope of this project

###############################################################################
###############################################################################
#                                   Testing                                   #
###############################################################################
###############################################################################

#I tried to webscrape the html online but I couldnt figure it out so 
#I made up some data
#url <- "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data"
#table <- readHTMLTable(url)
#table <- fread('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data')
#table <- fread('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.traintest')


#making up standardised "measured" data
data <- cbind(rnorm(10000, 0, 1), rnorm(10000, 0, 1), rnorm(10000, 0, 1))

#making up test weights and error
testWeight <- rnorm(3,0,3)
error <- rnorm(10000,0,0.1)

#calculator output
output <- data %*% testWeight + error

#Only use portions of our data for our X and y
X <- data[1:6000,]
y <- output[1:6000,]
#now make y binary
y <- round(sigmoid(y))



#run the program and plot/print results
weightMatrix <- GradientDescent(X,y,stepSize,maxIterations)

#plot change of individual weights over time
par(mfrow=c(2,2))
plot(1:(length(weightMatrix)/length(testWeight)),weightMatrix[1,])
plot(1:(length(weightMatrix)/length(testWeight)),weightMatrix[2,])
plot(1:(length(weightMatrix)/length(testWeight)),weightMatrix[3,])

#make sure these two print statements print something similar
print("testWeight(actual values):")
print(testWeight)
print("Last column of weightMatrix (predicted values):")
predictedWeight = weightMatrix[,length(weightMatrix)/length(testWeight)]
print(predictedWeight)

#Accuracy of the model assuming a p value of 0.5
#Basically this line of code takes rows 6001:8000 of our actual output
#(which the machine hasnt seen yet), runs out predicted weights with
#the data, and then compares predicted output with actual output.
#Then, it prints out the proportion of correct predictions with actual
#outputs. This line of code definitely isnt pep8 compliant because it
#has 90000000 characters lol
print("Accuracy:")
print(length(output[6001:8000][round(sigmoid(output[6001:8000])) == round(sigmoid(data[6001:8000,] %*% predictedWeight))]) / 2000)
