## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 

## the following functions:
  
### 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
### 2. cacheSolve: This function computes the inverse of the special "matrix" 
###  returned by akeCacheMatrix.
###    If the inverse has already been calculated (and the matrix has not changed), 
###    then cacheSolve should retrieve the inverse from the cache.

  ## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) invMat <<- inverseMatrix
  getInverse <- function() invMat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         ## Return a matrix that is the inverse of 'x'
  matrixValue <- x$getinverse()
  if (!is.null( matrixValue)) {
    message("getting cached data")
    return (matrixValue)
  }
  data <- x$get()
  matrixValue <- solve (data, ...)
  x$setinverse(matrixValue)
  matrixValue
 }
