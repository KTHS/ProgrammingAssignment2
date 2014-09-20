## Creating a special "matrix" object that can cache its inverse and computing the inverse of the special "matrix". 

makeCacheMatrix <- function(x = matrix()) {
## Creates a special "matrix" object that can cache its inverse.
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(invmatrix) inv <<- invmatrix
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {    
## Computes the inverse or retrieve the inverse from the cache.
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
