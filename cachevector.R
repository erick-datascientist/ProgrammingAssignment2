## Week 3 - Assignment 2
## R Programming
## By: Erick Rojas
##
## The goal of this file is to understand the principles
## involving caching so that I can work on the matrix
## assignment

## makeVector:
## Function copied/pasted from week3 example
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## makeVector:
## Function copied/pasted from week3 example
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}