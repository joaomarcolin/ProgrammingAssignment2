## This first function creates a special "matrix"
## object that can cache its inverse
##
## the output of this function is a list of functions
## that work on a given matrix called x

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## this function takes as argument a list in the format
## of the output of the function makeCacheMatrix
##
## in the first time you call the function cacheSolve,
## it will complete the list object with the inverse of
## matrix x
##
## if you call this function again, it will not calculate
## the inverse again

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}