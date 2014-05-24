## Because matrix inversion is usually a costly computation we created this code that uses caching to save 
## the calculation rather than computing repeatedly.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## This function assumes that the matrix is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) m <<- inver
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of a makeCacheMatrix object. If the inverse
## has been cached it will return that calculation, if not, it will calcuate the
## inverse and cache results.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
