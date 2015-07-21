# Week 3 - Assignment 2
# R Programming
# By: Erick Rojas
#
# Given that matrix inversion is a computationally
# intenstive operation, the following two functions aim
# to get the inverse of a matrix and cache the result
# so that subsequent calls don't have to be that
# intensive.
#
# EXAMPLE USE:
# x <- matrix(1:4, 2, 2)
# m <- makeCacheMatrix(x)
# s <- cacheSolve(m)
# m1 <- makeCacheMatrix(s)
# s1 <- cacheSolve(m1)
# x == s1

# makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.
#
# Value
# Returns a list with 4 function in it: 
# 1. set: sets the matrix
# 2. get: gets the matrix
# 3. setinverse: sets the inverse of the matrix
# 4. getinverse: gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        inv <<- NULL
        x <<- y
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }

    getinverse <- function() {
        inv
    }

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}

# cacheSolve:
# receives a makeCacheMatrix list as argument and
# checks if the inverse has been calculated via geinverse().
# If inv !is.null then it prints a message saying the
# cached result is returned
# if inv is null then it calculates the inverse of
# the matrix and stores the result using setinverse()
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("Returning cached inverse matrix")
        return(inv)
    }
    # this is only executed if inv IS null
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}