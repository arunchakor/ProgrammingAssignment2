## There are two function written 
## 1. makeCacheMatrix - Creates a list that contains four functions - get, set, getMatInverse and setMatInverse
## 2. cacheSolve - Computes Inverse of the matrix created with above function. However, before it would check in cache if
##                 inverse is already available. Only if inverse is missing in cache, it would do the computation.

## Creates a list that contains four functions - get, set, getMatInverse and setMatInverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setMatInverse <- function(inverse) i <<- inverse
        getMatInverse <- function() i
        list(set = set,
             get = get,
             setMatInverse = setMatInverse,
             getMatInverse = getMatInverse)
}


## Computes inverse of a matrix if not available in cache

cacheSolve <- function(x, ...) {
        i <- x$getMatInverse()
        if (!is.null(i)) {
                message("Retrieving cached matrix inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setMatInverse(i)
        i
}
