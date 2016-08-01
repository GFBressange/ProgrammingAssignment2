## This pair of functions cache the inverse of a matrix
## These functions have to be used separately:
## Example:
## source("cachematrix.R")
## m <- replicate(7,rnorm(7)) ## creates a random 7x7 matrix
## x <- makeCacheMatrix(m)
## cacheSolve(x) ## first time computation of inverse
## cacheSolve(x) ## second time: returns the value in cache and displays a message "getting cached data".


## This function makeCacheMatrix creates a special "matrix" which is really a list containing a function to
## 1. set the value of a matrix                 ## 2. get the value of a matrix
## 3. set the value of the inverse of a matrix  ## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of a matrix only if the value of its inverse is not in cache.
## If the inverse is already in cache, it returns the cache value to avoid time-consumming computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {       ## checks if inverse is already in cache
                message("getting cached data")  ## if yes : gives a message
                return(m)       ## and returns the value already calculated
        }
        data <- x$get()         ## if valvue of the inverse is not in cache
        m <- solve(data, ...)   ## calculates the inverse
        x$setinverse(m)         ## puts the value to cache
        m                       ## returns the inverse
}