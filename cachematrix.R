# Matrix inversion is a costly computation.
# Caching the inverse of a matrix helps to 
# avoid repeated computing. The two following 
# functions cache the inverse of a matrix.

# makeCacheMatrix returns a list containing 
# a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve is a function that returns the inverse
# of a matrix. First, it checks whether the inverse
# has already been computed. If yes, it skips the 
# computation and returns it. Else, it computes the
# inverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}