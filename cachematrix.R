## The following pair of functions implement a result-caching system for
## matrix inverse computation.  makeCacheMatrix returns an R object that will
## store a square matrix and a cache of its inverse.  cacheSolve operates on
## the R objects returned by makeCacheMatrix; it returns the cached inverse if
## it exists and computes/caches the inverse if it does not.

## makeCacheMatrix returns an R object that will store a matrix and a cache
## of that matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    # The cached inverse starts out as NULL.
    Minv <- NULL
    
    # We return a list object containing four functions that perform the
    # required operations.
    list(  ### START RETURN VALUE ###
        # The function to set the stored matrix.
        # Note that the cache is cleared (to NULL) whenever the matrix changes.
        set = function(y) {
            M <<- y
            Minv <<- NULL
        },
        
        # The function to get the stored matrix.
        get = function() {
            M
        },
        
        # The function to cache the inverse of the matrix.
        setinverse = function(inverse) {
            Minv <<- inverse
        },
        
        # The function to get the cached inverse of the matrix.
        getinverse = function() {
            Minv 
        }
    ) ### END RETURN VALUE ### 
}

## cacheSolve returns the cached inverse from a makeCacheMatrix-created R
## object if it exists, and computes/caches/returns it if not.

cacheSolve <- function(x, ...) {
    # check the cache
    Minv <- x$getinverse()
    
    if(!is.null(Minv)) {
        # the cache is full!  use it!
        message("getting cached data")
    } else {
        # the cache is bare!  get the matrix...
        M <- x$get()
        
        # ... invert it ...
        Minv <- solve(M)
        
        # ... and fill the cache.
        x$setinverse(Minv)
    }
    
    # return the inverse, regardless of how we got it
    Minv
}
