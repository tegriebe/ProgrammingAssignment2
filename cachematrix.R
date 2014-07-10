## To save time calculating the inverse of a matrix multiple times, 
## a special "matrix" is created which is capable of caching its calculated
## inverse.
## To accomplish this, the special "matrix" must be created using 
## the function `makeCacheMatrix`. The function `cacheSolve` is responsible
## for calculating and caching the inverse. If the inverse was already cached,
## it just returns the inverse of the matrix using the cache.


## Creates a special "matrix" capable of caching its inverse.
##
## Args:
##   x: An arbitrary invertible matrix.
##
## Returns:
##   List of functions for accessing the special "matrix" and 
##   its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of a special "matrix" taking account for the 
## potentially existing cached inverse of the matrix.
##
## Args:
##   x: Must be a special "matrix" created via function `makeCacheMatrix`
##
## Returns:
##   Inverse of special "matrix" `x`.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
