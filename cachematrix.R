## This script has the R-code for two functions: one which creates
## a special matrix from the given input matrix X. The code has been
## written to account for both square and non-square matrices. But it has
## been assumed that the matrix is invertible. The second function computes
## the inverse of the special matrix obtained from the first function and also
## checks if the inverse is already present in the cache, to improve the speed of computation.

## The function makeCacheMatrix accepts a matrix X as input and returns a list
## of four elements each of which is a matrix- set, get, setinverse and getinverse
## all of which are self-explanatory from the context of the example provided

makeCacheMatrix <- function(x = matrix()) {
  inverse_value <- NULL
  set <- function(y) {
    x <<- y                                 ##redefining the scope
    inverse_value <<- NULL                  ##redefining the scope
  }
  get <- function() x                       ##assigning x to get
  setinverse <- function(inverse_val) inverse_value <<- inverse_val 
  getinverse <- function() inverse_value
  list(set = set, get = get,                ##creating the list and return it
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve takes in the special matrix computed in the previous
## function as input and checks if the inverse for that same matrix already
## exists in the cache. If so, it returns that value. Else, it re-computes the
## inverse, assigns it to the cache and return the new value

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inverse_value <- x$getinverse()        ##getting the inverse from cache
  if(!is.null(inverse_value)) {          ##checking if the inverse is null
    message("getting cached data")
    return(inverse_value)
  }
  new_data <- x$get()                    ##if null then get the new matrix
  library(MASS)
  inverse_value <- ginv(new_data, ...)   ##computing the inverse freshly
  x$setinverse(inverse_value)            ##setting the inverse back to cache and return
  inverse_value
}
