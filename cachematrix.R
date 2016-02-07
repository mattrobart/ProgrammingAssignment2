## Put comments here that give an overall description of what your
## functions do

## create a special "matrix" that is really just a list of containing functions
## to set the value of the matrix, get the value of the matrix, set the value of 
## the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { ## the input matrix must be square
        inverse <- NULL
        set <- function(y) {
                x <<- y  ## assign y to x, which is in the cache 
                inverse <<- NULL  ## set "inverse" as NULL
        }
        get <- function() x  ## get the matrix
        setinverse <- function(inv) inverse <<- inv  ## set the inverse
        getinverse <- function() inverse  ## get the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This will calculate the inverse of the original input matrix (using the solve
## function) and store it in the cache.  If the inverse is already in the cache,
## it will retrieve it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {  ## if the inverse is in the cache
                message("inverse is in the cache")  # tell the user 
                return(inverse)  ## show the inverse
        }
        data <- x$get()  ## assign the matrix to "data"
        inverse <- solve(data, ...)  ## calculate the inverse
        x$setinverse(inverse)  ## save the inverse to the cache
        inverse  ## show the inverse
}
