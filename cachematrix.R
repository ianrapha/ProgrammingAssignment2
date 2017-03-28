## The functions below are used for calculate and cache the inverse of a matrix 
## as it seems to be an expensive task.

## This function receives one argument that is a conventional and invertable
## matrix (x) and returns a list containing functions responsible for 
## creating and retrieving original and cached versions of the matrix.
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


## This function receives one argument that is the object created by the 
## makeCacheMatrix function and extra arguments that can be used in solve 
## function and returns the inverse matrix from the cache (if defined) or 
## computes, caches and returns the computated version.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    
    if(!is.null(inverseMatrix)) {
        message("Getting cached matrix.")
        return(inverseMatrix)
    }
    
    originalMatrix <- x$get()
    inverseMatrix <- solve(originalMatrix, ...)
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