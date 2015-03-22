## This script contains two functions makeCacheMatrix() and 
## cacheSolve() that will take a square matrix as an input and process the
## corresponding inverse of that matrix.

## makeCacheMatrix() will accept a square matrix as an input and 
## create a list that contains the following functions: 
## set(), get(), setinv() and getinv().
## This function uses the super assignment operator <<- .

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() will return the inverse of square matrix x.
## It checks the environment for any cached iinverse matrix.
## If the matrix has changed, then it creates a new inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
