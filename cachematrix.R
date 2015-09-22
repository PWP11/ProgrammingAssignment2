## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.  This pair
## of functions compute and cache the inverse of a matrix so that it can be
## retrieved again from the cache without re-calculating.

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## input is the matrix which we assume is invertible
## output is a list of 4 objects which are the input to the cacheSolve function
##    set: set the input matrix
##    get: get the input matrix
##    setinverse: set the inverse
##    getinverse: get the inverse
        m <- NULL
        set <- function(y) {
                x <<- y       ## assign a value to x and m in an environment different from 
                m <<- NULL    ## the current environment
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache

cacheSolve <- function(x, ...) {
## The input is the output from makeCacheMatrix
## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## if the inverse has already been calculated then get it from the
        ## cache and stop the calculations
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise, calculate the inverse using "solve"
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)  ## set the value of the inverse in the cache
        m
}
