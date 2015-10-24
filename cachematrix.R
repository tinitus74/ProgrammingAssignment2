# This two functions allow to cache the inverse of a matrix
# The first, "makeCacheMatrix" creates a special matrix object that can cache its inverse
# The second, "cacheSolve" computes the inverse of the special matrix returned by makeCacheMatrix.
#
# If the inverse has already been calculated "cachesolve" retrieve the inverse from the cache. 
# This is useful for avoiding computing the same inverse several time. 

# We assume the input matrix is invertible. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
