## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix extends a matrix with four functions. x$set(y) will change the identity of the
## matrix to y. x$get() returns the original matrix. x$setinv() saves a second matrix, which will be
## the cached, inverted matrix. x$getinv() returns the cached matrix.

makeCacheMatrix <- function(x = matrix()) {
    matr <- NULL
    set <- function(y) {
        x <<- y
        matr <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        matr <<- inv
    }
    getinv <- function() {
        matr
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function will first check for the value returned by the getinv() method to see if
## it is an actual value (or NULL). If it isn't NULL, then the function will return the cached value.
## Otherwise, the value will be calculated normally, then saved into the cache. 

cacheSolve <- function(x, ...) {
    matr <- x$getinv()
    if(!is.null(matr)) {
        #message("Caching worked") ## used to check that this was being used, and it was.
        return(matr)
    }
    data <- x$get()
    matr <- solve(data, ...)
    x$setinv(matr)
    matr
}
