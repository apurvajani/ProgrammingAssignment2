## makeCacheMatrix - This function creates a special matrix that can cache its inverse
## cacheSolve - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

## The below function will create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the cached inverse of the special matrix created 
## by the above function. If the inverse is not cached or the matrix has
## changed, the inverse will be calculated again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m1 <- x$get()
        m <- solve(m1, ...)
        x$setinv(m)
        m
}
