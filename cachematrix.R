## Assignment: Caching the Inverse of a Matrix

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Init inverse matrix to NULL
    inverse <- NULL
    # Define functions
    get <- function() { x }
    set <- function(d) {
        x <<- d
        inverse <<- NULL
    }
    getinv <- function() { inverse }
    setinv <- function(inv) { inverse <<- inv }
    # Put functions into a list
    list(get = get,
         set = set,
         getinv = getinv,
         setinv = setinv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Get cached inverse matrix
    inv <- x$getinv()
    if (!is.null(inv)) {
        # If one exists, use it
        message("Using cached matrix")
    } else {
        # Otherwise calculate the inverse and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
    }

    inv
}
