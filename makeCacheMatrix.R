## Put comments here that give an overall description of what your
## functions do
## Write two functions: makeCacheMatrix, cacheSolve. These two functions
## can store the matrix into cache and calculate the inverse matrix when
## calling them.
## The efficiency of the calculation for the inverset matrix is increased
## through this way.


## Write a short comment describing this function
## This function returns a list with four functions
## set: store the matrix
## get: get the matrix
## setInverse: store the inverse matrix
## getInverse: get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  set <- function(m) {
    x <<- m
    y <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMat) y<<-inverseMat
  getInverse <- function() y
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function can calculate the inverse matrix of the input matrix
## from the makeCacheMatrix function
## If the inverse matrix has been stored in the cache before with the
## with the same input matrix, the return value will be retrieved from 
## the cache directly
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
