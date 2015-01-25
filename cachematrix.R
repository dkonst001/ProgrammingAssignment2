## This file incudes functionality to inverse square matrix and use caching 
## mechanizem in order to avoid inverting again if no change to the matrix took place.

## The caching mechanizem is based on lexical scoping principals. The matrix and 
## inverted matrix are store by the <<- operator which stores them in the global
## environment.

## makeCacheMatrix: This function is used to declare the subfunctions needed for 
## caching and store the matrix as well as the inverted matrix in cached memeory 
## i.e. the global environment. 
## Input - square matrix
## Output - list with sub functions
## 

## cacheSolve: This function is used to perform the caching is needed or use the 
## cached inverted matrix.

## Assumptions:
## 1) makeCacheMatrix(matrix) is called once with the initial value of the matrix.
## 2) Every change of the matrix must be followed by call to x$set(matrix) to make 
## the change effective (i.e. noticed by cacheSolve(list))
## 3) If x$set(m) was executed but the matrix was not changed then cached 
## information is not flushed, i.e. cacheSolve(List) will return the cached 
## inverted matrix. 
## Only if the matrix was changed followed by x$set(matrix), then cache is flushed.

makeCacheMatrix <- function(x = matrix()) {
        ## The marix is cached (i.e. x when used as parameter) and the inversed 
        ## matrix (i.e. inv) is initialized
        inv <<- NULL
        ## Set subfunction used to update changes in the matrix only in case 
        ## there is a difference between the cached (i.e. x) matrix and the updated matrix (i.e. y)
        set <- function(y) {
                if (matequal(x,y) == FALSE) { 
                        x <<- y
                        inv <<- NULL
                }
        }
        # Get subfunction retrieves the cached matrix
        get <- function() x
        # setInverse subfunction saves the inverted matrix in the cache
        setInverse <- function(xIn) inv <<- xIn
        # getInverse subfunction retrieves the cached inverted matrix
        getInverse <- function() inv
        # matequal subfunction comapres 2 matrixs and returns a booleen
        matequal <- function(x, y)
                is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## This functions is used to invert a square matrix. If the matrix was allready 
## inverted, the functions takes the inverted matrix from the cached 
## information and does not perform the matrix inversion again.                                                                                      

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()
        # If the cached inverted matrix has a value, display a proper message
        # and return with the cached value
        if(!is.null(inv)) {
                message("getting cached data")
        } else {
                data <- x$get()
                inv <- solve(data, ...)
                x$setInverse(inv)
        }
        inv
}