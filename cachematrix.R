# These functions are used in conjunction to store the values of frequently
# referenced variables in cache to avoid recalculation and therefore save time
# if they have already been calculated. If the arguments have been changed then 
# the calculation takes place.
# ASSUMPTION: The matrix supplied is always invertible therefore there are no
# checks.


# First function creates a list of functions to set and get the matrix and the 
# inverse of it. The variables are stored at a different environment from the
# current one. The inverse of matrix is cached. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(invrs) inv <<- invrs
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}

# The second function calculates the inverse of the matrix using the functions
# created by the first function. However, if the inverse has already been
# calculated then the function retrieves it from cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinverse(inv)
        inv
}
