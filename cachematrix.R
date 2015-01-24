## This file incudes functionality to inverse 2x2 matrix and use caching 
## mechanizem
 

## makeCacheMatrix function include function templates for inversing matrix and
## storing the original and inversed matrix in cach. The Caching mechanizem is 
## actually the environment created for each matrix instance.

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(xIn) inv <<- xIn
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
}
