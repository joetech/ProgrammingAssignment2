## Functions that can create, cache, and return the inverse of a matrix

## This function creates special "matrix" object that can cache its inverse matrix
## Returns a list with functions for getting and setting the cached matrix
## Keeps cached values in the "inverse" variable.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() {
        x
    }

    setInverse <- function(inv) {
        inverse <<- inv
    }
    getInverse <- function() {
        inverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## If the cached inverse variable is not null, return it, otherwise, set the inverse of the matrix x passed in as
## an argument and store it in the cache variable.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()

    if(!is.null(inverse)) {
        return(inverse)
    }

    mtrx <- x$get()
    inverse <- solve(mtrx, ...)
    x$setInverse(inverse)

    inverse
}
