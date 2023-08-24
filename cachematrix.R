## The functions below calculate and return the inverse of a matrix
## using a cache to speed up repeated lookups.

## Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #variable to store inverse
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
