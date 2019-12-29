##------------------------------------------------------------------------------------------------------------------
## Assignment 2: Caching the Inverse of a Matrix
##    Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
##    matrix rather than compute it repeatedly The current program contains a pair of functions that 
##    cache the inverse of a matrix.
## Author: Debjit Ray
## Date: Dec 29, 2019
##------------------------------------------------------------------------------------------------------------------


##------------------------------------------------------------------------------------------------------------------
## The below function creates a matrix object that can cache its invers
##------------------------------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inversed_matrix) {i <<- inversed_matrix}
        getinverse <- function() {i}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }

##------------------------------------------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.
##------------------------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()

    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }    
    
    data <- x$get()
    i <- matlib::inv(data, ...)
    x$setinverse(i)
    i    
}

