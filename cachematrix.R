## This file contains methods for a special matrix implementation that
## caches the inverted matrix, so it doesn't have to be recomputed even
## if it's called for frequently.

## create a special "matrix", which is really a list containing
## functions to set & get the matrix and to set & get it's inverse.
## this assumes the matrix is invertible.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## this function gets the invers of the special matrix, using cache if
## possible, or computing and adding to cache if it's not there
cacheSolve <- function(x, ...) {
    # look for the inverse in cache first
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return (i)
    }

    # it wasn't in cache.  compute the inverse and stick it in cache for
    # next time
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
