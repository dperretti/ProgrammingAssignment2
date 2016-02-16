## The function is caching the inverse of a previously generated matrix
## rather than repeatedly calculating it.
## This is done by the two functions below which 1) create an object to
## store the matrix, and 2) compute it's inverse.

## this function generates a matrix and saves its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix generated above by
## makeCacheMatrix. In case the inverse was already calculated previously
## (and stored) then the function retrieves the inverse from the cache.
## Otherwise it generates the inverse from scratch.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinverse(m)
  m
}