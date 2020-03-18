## Sascha Gutmann
## R programming on Coursera by R.D. Peng, Johns Hopkins University
## Programming Assignment 2, https://github.com/rdpeng/ProgrammingAssignment2
##
## Two functions are defined below (1) a "matrix" object is created that can cache 
## its inverse (makeCacheMatrix) and (2) calculate the inverse of a matrix returned 
## by makeCacheMatrix (cacheSolve).
##
## Assumption: Supplied matrix is always invertible

## The makeCacheMatrix creates a special "matrix" object (list). It returns a list of functions to
## (1) set the matrix, (2) get the matrix, (3) set the inverse, and
## to (4) get the inverse of the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv, 
             getInv = getInv)
}


## The casheSolve takes the special "matrix" object created by makeCacheMatrix. It calls x$getInv and checks if the 
## inverse exists, i.e. is already cached (!is.null). If so it will be returned as inv. If no inverse matrix 
## is stored it will get the matrix data <- x$get(), calculte its inverse inv <- solve(data, ...) and r
## return the inverse (inv).

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
