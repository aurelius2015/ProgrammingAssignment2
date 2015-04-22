## Programming Assignment 2: Lexical Scoping - Caching the Inverse of a Matrix

##This second programming assignment will require you to write an R function is 
##able to cache potentially time-consuming computations. For example, taking the 
##mean of a numeric vector is typically a fast operation. However, for a very 
##long vector, it may take too long to compute the mean, especially if it has to
##be computed repeatedly (e.g. in a loop). If the contents of a vector are not 
##changing, it may make sense to cache the value of the mean so that when we 
##need it again, it can be looked up in the cache rather than recomputed. In 
##this Programming Assignment will take advantage of the scoping rules of the R 
##language and how they can be manipulated to preserve state inside of an R 
##object.


##Matrix inversion is usually a costly computation and there may be some benefit
##to caching the inverse of a matrix rather than compute it repeatedly (there 
##are also alternatives to matrix inversion that we will not discuss here). Your
##assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}
