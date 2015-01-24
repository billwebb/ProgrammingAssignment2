## R Programming - Cousera
## January 2015
## Programming Assignment #2

## This file contains functions to store a matrix and cache the calculation of
## the matrix inverse.  The matrix is created first with the makeCacheMatrix()
## function.  For calculating the inverse, use the cacheSolve() funciton, which
## will calculate the inverse, if needed, and cache it, then return the
## matrix inverse.

## makeCacheMatrix() - used to create a new matrix cache  
## Parameters -
##   x - matrix that will be cached.
## Returns list containing -
##   set(y) - function to set the value of the matrix
##   get() - return the matrix
##   setsolve(inv) - function to set the cached inverse matrix
##   getsolve() - return the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## This is the cached inverse matrix, initially NULL
    i <- NULL
    ## Set function stores the updated matrix in x and clears
    ## any previously cached inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## Get function simply returns the cached matrix
    get <- function() x
    ## Update the cached inverse matrix
    setsolve <- function(inv) i <<- inv
    ## Get the chanced inverse matrix.  Returns NULL if this has not
    ## yet been cached.
    getsolve <- function() i
    ## Return a list containing the 4 functions
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve() - calculate inverse of a matrix.  Return a cached value,
## if it exists.  Otherwise, calculate, cache, and return the inverse.
## Parameters -
##   x - matrix that was created using makeCacheMatrix()
## Returns -
##   matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Get the current value of the inverse from the matrix cache.  This 
    ## could be NULL, if the inverse has not yet been calculated
    i <- x$getsolve()
    ## If the cached inverse is valid, just return it!
    if (!is.null(i) ) {
        message("getting cached data")
        return(i)
    }
    ## Otherwise, we need to calculate and store the matrix inverse.  First,
    ## let's get the cached value of the matrix itself.
    data <- x$get()
    ## Then, calculate the inverse of the matrix.
    i <- solve(data, ...)
    ## Cache the inverse matrix we just calculated
    x$setsolve(i)
    ## And return the inverse matrix
    i
}
