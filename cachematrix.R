## Caching the Inverse of a Vector
## set() - set the value of the vector
## get() - get the value of the vector
## setInverse() - set the value of the Inverse
## getInverse() - get the value of the Inverse

makeCacheMatrix <- function(x = numeric()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) invrs <<- Inverse
        getInverse <- function() invrs
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function  cacheSolve() calculates the Inverse Matrix of the special "vector"

cacheSolve <- function(x, ...) {
        invrs <- x$getInverse()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...) 
        x$setInverse(invrs)
        invrs
}
