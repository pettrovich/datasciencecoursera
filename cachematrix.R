## These functions allow to cache the inverse of a matrix
## by storing it, along with the matrix data, in a new
## "CacheMatrix" object, so it doesn't have to be
## computed more than once

## This function creates a list of 4 functions to
## interact with the CacheMatrix object, creating it
## recovering the matrix data, setting the inverse,
## and recovering it
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}

## This functions computes the inverse of a cacheMatrix
## object using the solve() function, but only if it hasn't
## been previously computed and stored in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}