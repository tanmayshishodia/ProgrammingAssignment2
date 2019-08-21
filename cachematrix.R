##A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
          x <<- y
          invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMat <<- inverse
    getInverse <- function() invMat
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
     invMat <- x$getInverse()
     
     #check if cache data exists or not. If it exists return cached data
     if (!is.null(invMat)) {
          message("getting cached data")
          return(invMat)
     }
     
     #if cached data doesn't exist, get data and compute its inverse
     data <- x$get()
     invMat <- solve(data,...)
     x$setInverse(invMat)
     invMat
}
