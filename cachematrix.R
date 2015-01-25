## Function makeCacheMatrix creates a matrix object. Function cacheSolve returns an inverse of a matrix object created by makeCacheMatrix.

## Create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Compute the inverse of a matrix object. If the inverse has already been calculated, return the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("retrieving cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data ...)
    x$set_inverse(inverse)
    inverse
}
