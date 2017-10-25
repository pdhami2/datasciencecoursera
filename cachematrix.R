## Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly

#Clear up the environment
rm(list=ls())

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
# its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse_matrix <<- solve
  getInverse <- function() inverse_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated, then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


