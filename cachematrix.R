## This function is used to hold a matrix and cache some operations (in this case, only the inverse of the matrix).
## It returns a list of methods that will be used by cache supported functions.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        ## when a new matrix is defined, we have to clear the precalculated operations (in this case, only the inverse)
        inv <<- NULL
    }
    get <- function () x
    
    setinverse <- function (inver) inv <<- inver
    getinverse <- function () inv
    
    ## sets the matrix passed as parameter as the current matrix
    set(x)
    
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}

## This function returns the cached inverse matrix of the cached matrix. If there is no cached inverse, 
## it will be calculated then.
cacheSolve <- function(x, ...) {
    ## It tries to get the cached inverse
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## There is no cached inverse, then it will be calculated and returned
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}