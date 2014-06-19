## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cached_matrix <- NULL
  set <- function(key) {
    x <<- key
    cached_matrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setmatrix <- function(matrix) {
    cached_matrix <<- matrix
  }
  
  getmatrix <- function() {
    cached_matrix
  }
  
  list(set=set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Write a short comment describing this function
## This function returns the inverse of the special "matrix" returned by
## makeCacheMatrix from cache, if previously calculated, otherwise
## calculates and saves into cache for future use, and returns it
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## Calculates and cache it the first time
  ## No calcuations the next time, if in the cache
  
  cache_matrix <- x$getmatrix()
  if(!is.null(cache_matrix)){
    ## It is in cache so we are done!
    message("it is in the cache!")
    return(cache_matrix)
  }
  message("will calculate since not in cache!")  
  data <- x$get()
  cahce_matrix <- solve(data, ...)
  x$setmatrix(cahce_matrix)
  cahce_matrix
}
