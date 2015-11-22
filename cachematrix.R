## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly. These two functions below creates a special matrix
## object and cache the inverse matrix for latter use.

## 'makeCacheMatrix' function creates a special 'matrix' object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    ## sets the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## gets the matrix
    get <- function() x

    ## sets the inverse matrix
    setmatrix <- function(matrix) {
        m <<- matrix
    }

    ## gets the inverse matrix of 'x'
    getmatrix <- function() m

    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## 'cacheSolve' function computes the inverse of the special 'matrix'
## returned by 'makeCacheMatrix'. If the inverse has already been
## calculated (and the matrix has not changed), then the 'cacheSolve'
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## get the inverse matrix
    m <- x$getmatrix()

    ## check to see if the inverse matrix has already been computed
    ## and cached. If it is, return the cached inverse matrix
    if (!is.null(m)) {
        message("getting cached inverse matrix data")
        return(m)
    }

    ## If the execution gets this far, it means that the inverse
    ## matrix needs to be computed and cached.
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)

    m
}
