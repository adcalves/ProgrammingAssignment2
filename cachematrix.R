## cachematrix allows the user to cache the results of matrix operations.
## Initially, only the inverse of a matrix is supported.

## makeCacheMatrix creates a matrix that caches the result of its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a cached matrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached inverse.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

## Tests
# m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# cm1 = makeCacheMatrix(m1)
# cs1 = cacheSolve(cm1)
# m2 = matrix(c(1,3,2,4), nrow=2,ncol=2)
# cm1$set(m2)
# cacheSolve(cm1)

