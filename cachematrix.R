## This file defines 2 functions aimed to be used together in order
## to store a matrix and compute the corresponding inverse of this
## matrix only once (the computed inverse is stored in memory for
## quick further accesses).

## Return a CacheMatrix list composed with the following functions:
## - set: set a new matrix
## - get: reurn the matrix
## - setinv: set the inverse of the matrix
## - getinv: return the inverse of the matrix or NULL if the inverse
## has not been set yet.
##
## Parameters:
## - x: a matrix
##
## Note: This function does not compute the inverse of the passed
## matrix, to do so, please use the cacheSolve function along with
## a CacheMatrix list returned by this function.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    getinv <- function() inv
    setinv <- function(i) inv <<- i
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}

## Return the inverse of the passed matrix CacheMatrix list.
## This function assumes that the passed CacheMatrix is invertible.
## The computed inverse will be stored in the environment of x, thus
## the inverse is only compute once.
##
## Parameters:
## - x: a CacheMatrix list containing an invertible matrix
cacheSolve <- function(x, ...) {
    i = x$getinv()
    if (is.null(i)){
        message("Computing the inverse of the matrix...")
        i = solve(x$get())
        x$setinv(i)
        message("done")
    }
    x$getinv()
}
