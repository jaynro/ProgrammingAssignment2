##  The purpose of below functions is  to minimize computing time 
## when calculating the inverse of a matrix  by creating  a special object that ## stores a numeric matrix  and caches its inversion. In case it´s requested 
## again, the result is cached and not computed.



##This function creates a special "matrix" object and caches its inverse
## using solve() function. It`s  always assumed that the matrix its reversible.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special object returned 
## by above functiion. If the inverse has already been calculated, then
##`cacheSolve`  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
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
