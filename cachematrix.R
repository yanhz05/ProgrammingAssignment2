## These two functions are used for calculation of matrix inversion.
## We have makeCacheMatrix and cacheSolve here

## makeCacheMatrix is a function which are prepared for the inversion of matrix
## The functions defined in this makeCacheMatrix are as follows:
##   setMatrix      set the value of the matrix
##   getMatrix      get the value of the matrix
##   setInverse     set the value of inversion
##   getInverse     get the value of inversion
## Here the matrix should be provided

## The first step is to build a MATRIX 
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  # stored the matrix value 
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  setInverse <- function(solve) {
    # the matrixs are supposed to be invertable, otherwise use ginv
    m <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    m
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## The second step is to calculate the inversion of the matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # get the inversion value
  m <- x$getInverse()
  
  # if the inversion has been calculated, return the inversion value
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # else, get the matrix value and caclulate the inverse
  data <- x$getMatrix()
  m <- solve(data)
  x$setInverse(m)
  
  # return the inversion result
  m
}