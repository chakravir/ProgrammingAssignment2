# R Programming - 2/2015 - Programming Assignment 2
# Submission by Ravi Ramkissoon

## This file implements a special "matrix" object that can cache the value of its own inverse
## After the first time the inverse is calculated, subsequent requests for the matrix inverse
## will return the cached value - until the matrix changes and the cache s cleared
## There are 2 functions : 
## makeCacheMatrix creates a new instance of the special "matrix" object
## cacheSolve calculates the inverse of the matrix, using the cached value if present

## makeCacheMatrix creates a new instance of the special "matrix" object
## This is basically a list containing the underlying matrix and its currently cached inverse
## as well as methods to set and get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize cached inverse value to null
    i <- NULL

    # functions to get and set the underlying matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x

    # functions to get and set the cached inverse value
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i

    # return the "matrix" object which is a list of members and get/set methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the matrix, using the cached value if present
## The only required parameter is an instance of the special "matrix" object
## created by the makeCacheMatrix function. Other parameters will be passed to the 
## solve function if we are calculating the inverse instead of returning a cached value
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("returning cached inverse matrix")
        i
    }
    
    # there is no cached value. Calculate the inverse and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

# Testing the functions

# a simple matrix
# mo<-makeCacheMatrix(matrix(4:1,nrow<-2,ncol<-2))
# cacheSolve(mo)
# cacheSolve(mo)

# a larger matrix
# mo<-makeCacheMatrix(matrix(sample(1:1000000,1000000),nrow<-1000,ncol<-1000))
# mi<-cacheSolve(mo)
# this call should be much faster
# mi<-cacheSolve(mo)
