## The following two functions can be used to create special matrix
## objects that have the abiblity to cache their inverses, and then
## find the inverses using the cache, thus only recalculating the
## inverse when necessary.

## Create a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute inverse of special 'matrix' object returned by makeCacheMatrix.
## If already calculated, return cached value instead.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return (i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setInverse(i)
    i
}
