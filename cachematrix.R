## makeCachMatrix - creates list of functions that can be used to cache the
##                  inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(solve) inv <<- solve
        getMatrixInverse <- function() inv
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## cachesolve : inputs a matrix and first checks to see if the inverse exists
##              if so then it will use the cached value, otherwise call the makeCacheMatrix
##              function and create the inverse directly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getMatrixInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        myMatrix <- x$get()
        inv <- solve(myMatrix, ...)
        x$setMatrixInverse(inv)
        inv
}
