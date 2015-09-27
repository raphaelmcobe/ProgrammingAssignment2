## This program implements a cached version of the solve operation. The idea 
## here is to try to save processor time caching the result of the solve 
## operation on a given matrix.

## This function is responsible for storing the matrix and its solved version.

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set <- function(y) {
        x <<- y
        solved <<- NULL
    }
    get <- function() x
    setsolved <- function(solved) solved <<- solved
    getsolved <- function() solved
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
    
}


## This function solves a matrix. Fist it checks if the matrix has already been
## solved. If that is the case, returns the cached version of the result.
## If not, it uses the solve operation and updates the cache for this matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    solved <- x$getsolved()
    if(!is.null(solved)) {
        message("getting cached data")
        return(solved)
    }
    data <- x$get()
    solved <- solve(data, ...)
    x$setsolved(solved)
    solved
}
