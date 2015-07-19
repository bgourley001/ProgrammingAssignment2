## Put comments here that give an overall description of what your
## functions do
##
## Overview :
##  The included functions, makeCacheMatrix() and cacheSolve() allow the creation of the inverse of
##  a matrix returning a cached copy of the inverse if it exists. This would reduce unnecessary
##  computation time if the inverse was required to be created multiple times.
##
##  Operation is a 2-step operation, see Usage note below.
##  
## Usage :
##  1. create a matrix cache object : 
##      > myMatrixCache <- makeCacheMatrix(myMatrix)
##  where myMatrix is an invertible matrix e.g > myMatrix <- matrix(rnorm(1:9), nrow=3, ncol = 3))
##
##  2. get the inverse of myMatrix : 
##      > myMatrixInverse <- cacheSolve(myMatrixCache)

## Write a short comment describing this function
##
## The makeCacheMatrix function when called with a matrix will create an object which stores the
## matrix and creates the following internal functions :
##      set : stores the passed matrix
##      get : returns the stored matrix
##      setInverse : creates the inverse of the stored matrix via the R solve function
##      getInverse : returns the inversed matrix
##
## Note : The function could be extended to add other matrix operations if desired.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##
## The cacheSolve function when passed a makeCacheMatrix object will return
## the inverse of the matrix stored in the makeCacheMatrix object.
## If the inverse already exists in the makeCacheMatrix object, then the
## existing inverse will be returned without recreating it. In this case, a message
## will be output to the console "getting cached matrix inverse". Otherwise the
## inverse will be created and returned.
##
## The function calls the makeCacheMatrix operations : get, getInverse and setInverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data,...)
    x$setInverse(inverse)
    inverse
}
