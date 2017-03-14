
## This program caches inverse of matrix and creates inverse if absent

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        mx.data <- x$get()
        m <- solve(mx.data, ...)
        x$setinv(m)
        
        return(m)
}