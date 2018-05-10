## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    # if the inverse has already been calculated above.
    if (!is.null(m)){
        # get the inverse from the cache and skips the computation.
        message("getting cached data")
        return(m)
    }
    # if the inverse has not been calculated, it calculates the inverse
    data <- x$get()
    m <- solve(data, ...)
    # Here, sets the value of the inverse in the cache via the setinv function.
    x$setinv(m)
    m
}
