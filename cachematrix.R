## Caching the Inverse of a Matrix:

## Matrix inversion is costly from a computational perspective. There may be 
## benefits to caching the inverse of a matrix rather than compute it
##multiple times. The following two functions will create a special object 
##that stores a matrix and caches its inverse. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## the function makeCacheMatrix above. If the inverse was already calculated,
## it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Compute a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
