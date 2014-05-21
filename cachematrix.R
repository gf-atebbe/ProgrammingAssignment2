## These functions provide a cache in memory of the inverse of a matrix.
## If the matrix hasn't been seen before, 
## the inverse is computed and added to the cache.

## Example Usage:
## b <- makeCacheMatrix()
## b$set(matrix(c(2, 4, 3, 1, 5, 7, 1, 5, 3), nrow=3, ncol=3))
## cacheSolve(b)
## cacheSolve(b)
## - the first call to cacheSolve will return the calculted inverse 
##   and store it in the cache
## - the second call will return the precomputed inverse from the cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         getinv = getinv, setinv = setinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

