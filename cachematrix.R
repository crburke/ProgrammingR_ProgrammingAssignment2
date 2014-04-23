## Functions that provide a method of caching the inverse of a matrix
##   Suggested usage: cacheSolve(makeCacheMatrix(<your-matrix>),...)

## Create a list of functions to assist in calculation of the inverse 
## of the specified square matrix. The returned list contains functions to:
##  1. set - set the matrix to invert
##  2. get - get the matrix to invert
##  3. setinverse - set the inverse of the matrix
##  4. getinverse - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    if (nrow(x) != ncol(x)) {
        stop("cannot compute inverse of non-square matrix")
    }
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Calcuate the inverse of the given cacheMatrix x
## ... arguments are supplied to the call to solve function to solve the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
