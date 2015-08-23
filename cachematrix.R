## Assignment 2
## Sandro Jean-Mairet

## The following functions create an invertible matrix and make it available in 
## the cache

## This function creates a special "matrix" object that can cache its inverse
#1 set matrix
#2 get matrix
#3 set inverse
#4 get inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize matrix
        cache_value <- NULL
        set <- function(y) {
                x <<- y
                cache_value <<- NULL
        }
        
        # get matrix
        get <- function() x
        
        # set inverse
        setinv <- function(solve) cache_value <<- solve
        
        # get inverse
        getinv <- function() cache_value
        list(set = set, get = get, setinv = setinv, getinv = getinv)


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        
        # get cached inverse of matrix if available
        cache_value<- x$getinv()
        if(!is.null(cache_value)) {
                message("getting cached data")
                return(cache_value)
        }
        # calculate inverse if not available in cache
        data <- x$get()
        cache_value<- solve(data, ...)
        x$setinv(cache_value)
        cache_value
        
}
