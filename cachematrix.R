## Matrix inversion
##
## These functions are part of Programming Assignment 2 for R Programming Course.
## For this assignment, assume that the matrix supplied is always invertible.
##
## There are two functions here that together aims to address the costly nature of 
## matrix inversion by caching the inverse and only calculating if the result
## wasn't previously calculated.  

## This first function is to create a matrix object and cache the inverse.
## The result is a list containing 4 functions that: set the matrix, gets the matrix,
## sets the inverse and gets the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from the cache.

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
