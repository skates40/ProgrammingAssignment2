## The goal is to create a pair of functions, makeCacheMatrix, which creates a special
## "matrix" object that can cache its inverse and cacheSolve, which computes the 
## inverse of the special "matrix" returned by makeCacheMatrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrse <- NULL
        set <- function(y) {
                x <<- y
                invrse <<- NULL
        }
        get <- function() x
        setInvrse <- function(inverse) invrse <<- inverse
        getInverse <- function() invrse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrse <- x$getinvrse()
        if (!is.null(invrse)) {
                message ("getting cache result")
                return(invrse)
        }
        data <- x$get()
        invrse <- solve(data,...)
        x$setinvrse(inv)
        invrse
        }
