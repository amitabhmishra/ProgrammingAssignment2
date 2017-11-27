## Put comments here that give an overall description of what your functions do.

# The functions are written using simple R function programming that allows it to setup the matrix, 
## get the matrix, and similarly its setsup an inverse and gets the inverse.


## Write a short comment describing this function

## The makeCacheMatrix function constructs a "matrix" object that computes its inverse and cache if necessary. 

  
  
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(invers) inv <<- invers
    getinv <- function() inv
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)
  }
  
  ## Function cacheSolve computes the inverse of the "matrix" computed by the function makeCacheMatrix 
  ## which also returns it. Then this function checks for the existence of an inverse which has been 
  ## computed but did not change i.e. its elements are identical, then it will not recomupte the inverse but 
  ## will retrieve the inverse from the cache memory, to save computations.

  
  cacheSolve <- function(x, ..) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
  }

