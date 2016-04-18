## This file provides functions to do some matrix operations in a cached way. 
## For this, a cached version of a matrix is provided. The cached values are 
## stored at the cached objects.
##
## How to use:
##    1. Create a regular matrix => m=matrix(..)
##    2. Create an instance of the cached version of it 
##          => cached=makeCacheMatrix(m)
##    3. Call cacheSolve => cacheSolve(cached)
##
## How to test:
##    1. m=matrix(rnorm(810000), nrow=900, ncol=900)
##    2. cached=makeCacheMatrix(m)
##    3. solvedCached=cacheSolve(cached)
##    4. solvedOrig=solve(m)
##    5. sum((solvedOrig-solvedCached)>0) => This should be 0
##    6. system.time(cacheSolve(cached)) => Should be something small
##    7. system.time(solve(m)) => Should be something bigger
##    8. m=matrix(rnorm(810000), nrow=900, ncol=900)
##    9. cached$set(m)
##   10. cacheSolve(cached)

## Wraps a matrix in an object, which can store cached values for some matrix 
## operations.
## 
## Usage:
##    makeCacheMatrix(m)
## Args:
##    m : The original matrix, for which the values should be cached.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the given matrix. It uses solve in the back.
## 
## Usage:
##    cacheSolve(x, ...)
## Args:
##    x: The wrapped matrix. See makeCacheMatrix for details on how to create a
##        cached version of an R matrix.
##    ...: Further arguments passed to solve.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  
  # If the determinat is 0, there is no solution for this matrix. Stop and show
  # a propert error message.
  if(det(data)==0) {
    stop("The determinant of the matrix is 0. Matrix is not solvable.")
  }
  
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
