## Put comments here that give an overall description of what your functions do.

# The functions are written using simple R function programming that allows it to setup the matrix, 
## get the matrix, and similarly its setsup an inverse and gets the inverse.


## Write a short comment describing this function
## A short comment describing this function is iven below.

##This function given name - Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 

#<<- operator is used to assign a value to an object in an environment that is different 
#from the current environment 

#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              #This statement gets the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #This statement sets the value of the invertible matrix
  getInverse <- function() invMatrix                     #This statement gets the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}



## The function cacheSolve written below takes the output of the previous matrix using makeCacheMatrix(matrix) as an 
# input and checks the inverse matrix from makeCacheMatrix(matrix) has any value in it or not i.e if it is NULL or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
    return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to compute the set that matrix needs to be inverted
  x$setInverse(invMatrix)                         #set the invertible matrix to variable x
  return(invMatrix)                               #return the inverted matrix
  ## Return a matrix that is the inverse of 'x'
}
