## The module defines a matrix object in which its inverse is cached using R lexycal scoping.

## Create a cached object passing matrix x. The inverese matrix value is intially set to NULL. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(invmat) inv <<- invmat
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv, getinv = getinv)
        
}


## Returns the inverse of the given CacheMatrix. If the object already contains the inverse
## no calculation is done and the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
