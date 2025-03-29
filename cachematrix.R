## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## Initialize the inverse as NULL
    
        set <- function(y) {
        x <<- y  ## Assign new matrix
        inv <<- NULL  ## Reset the cached inverse        
        }
    
        get <- function() x  ## Return the matrix
    
        setInverse <- function(inverse) inv <<- inverse  ## Store the inverse
    
        getInverse <- function() inv  ## Retrieve the cached inverse
    
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()  ## Retrieve cached inverse
    
        if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)  ## Return cached inverse if available
        }
    
        mat <- x$get()  ## Get the matrix
        inv <- solve(mat, ...)  ## Compute the inverse
        x$setInverse(inv)  ## Cache the computed inverse
    
        return(inv)
}
