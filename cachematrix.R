## This R file contains 2 functions, makeCacheMatrix and cacheSolve, which are
## used to cache the inverse of a matrix.
## 
## Assumption: the matrix supplied is always invertible

## makeCacheMatrix: This function creates a matrix object that can cache its 
## inverse. The output of this function will be used in the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  ## set and get value of the matrix -- this was taken from the example Caching 
  ## the Mean of a Vector  
  inversematrix <- NULL
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixinverse) inversematrix <<- matrixinverse
  getinverse <- function() inversematrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the matrix returned by the
## the other function, makeCacheMatrix. If the inverse is already calculated, 
## this function retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data) ## solve() returns inverse of the matrix
  x$setinverse(inversematrix)
  inversematrix
}
