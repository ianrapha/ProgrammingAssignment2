## The following functions are used for caching the inverse of a matrix as it
## seems to be an expensive task.

## Returns a list containing some functions responsible for creating and 
## retrieving original and cached versions of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Returns the inverse matrix from the cache (if defined) or the computed 
## version.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    
    if(!is.null(inverseMatrix)) {
        message("Getting cached matrix.")
        return(inverseMatrix)
    }
    
    originalMatrix <- x$get()
    inverseMatrix <- solve(originalMatrix)
    x$setInverse(inverseMatrix)
    
    inverseMatrix
}

## Testing case
##regularMatrix <- matrix(rnorm(36), nrow = 6, ncol = 6)
##cache <- makeCacheMatrix(regularMatrix)

##cache$get()
##cache$getInverse()
##cacheSolve(cache)

##cache$getInverse()
##cacheSolve(cache)