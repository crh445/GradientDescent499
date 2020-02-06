# library for making plots
library(ggplot2)
# library for reading data from site
library(data.table)

# constants for algorithm
maxIterations = 10000
stepSize = 0.0001

# read in the data
spamData <- fread('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data')





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
#GradientDescent <- function( X, Y, stepSize, maxIterations )
#{
  # declare a weight vector and initialize it to zero
  # same size as the number of features
#  weightVector = rep(0, ncol(X))
  
  # There should be a variable called weightMatrix of real numbers 
  # (number of rows = number of input features, number of columns = maxIterations)
#  weightMatrix = 0
  
  
#  return weightMatrix
#}





