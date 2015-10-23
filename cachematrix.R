## makeCacheMatrix creates a special matrix object that can cache its inverse.
## An assumption is that the matrix supplied is a square matrix and is always
## invertible.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix created with the 
## makeCacheMatrix function written above.  It first checks to see whether
## the inverse has already been calculated.  If yes, it skips the computation
## and uses the cached inverse value. If no, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache using the setinverse
## function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
