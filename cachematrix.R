## To save time calculating the inverse of a matrix multiple times, 
## a special "matrix" is created using the function `makeCacheMatrix`. This 
## special "matrix" can cache its calculated inverse.
## To calculate the inverse using the caching functionality,
## the function `cacheSolve` is used.

## This function creates a special "matrix" that is capable of storing
## its calculated inverse. 
## Therefore it returns a list of functions to get or set the actual matrix
## and to get or set the inverse of this matrix.
##
## Args:
##   x: An arbitrary matrix that is invertible.
##
## Returns:
##   A special "matrix" capable of caching its inverse. 
##   To calculate its inverse, the following function `cacheSolve` 
##   should be used.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" `x`.
## For the calculation it first tries to get the cached inverse of `x`.
## If no cached version exists, it is calculated and stored in the cache.
##
## Args:
##   x: Must be a special "matrix" created via function `makeCacheMatrix`
##
## Returns:
##   Inverse of special "matrix" `x`.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
