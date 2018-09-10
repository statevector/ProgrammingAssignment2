
## The function makeCacheMatrix() creates an R object that stores a 
## matrix and its inverse. This is accomplished with the setters set() 
## and setinv() and the getters get() and getinv(). Crucially, 
## makeCacheMatrix() returns a list of its functions to the parent 
## environment, which allows for direct access of its internal state.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
        setinv	= setinv,
        getinv = getinv)
}

## The function cachesolve() computes the inverse of the matrix stored 
## in the function makeCashMatrix(). If the inverse has already been 
## calculated, then the cached value is returned and no computation of 
## the inverse is performed.

cacheSolve <- function(x, ...) {
            inv <- x$getinv()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv		
}
