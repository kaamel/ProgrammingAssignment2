## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## This is the inverse
  m <- NULL
  ## set the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the vector
  get <- function() {
    x
  }
  ## set inverse of vector
  setmatrix <- function(matrix) {
    m <<- matrix
  }
  
  ## get inverse of vector
  getmatrix <- function() {
    m
  }
  
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Write a short comment describing this function
## This function returns the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse was previously calculated it is returned, otherwise
## it is calculated, saved for future use, and returned
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## Calculates and saves the first time
  ## No need to recalcuate after the next time
  
  saved_inverse <- x$getmatrix()
  if(!is.null(saved_inverse)){
    ## It is in cache so we are done!
    message("it is in the cache!")
    return(saved_inverse)
  }
  message("will calculate it since not in cache!")  
  data <- x$get()
  ## calculate and save in cache
  saved_inverse <- solve(data, ...)
  ## save it
  x$setmatrix(saved_inverse)
  saved_inverse
}
