# 2014/04/20
# fladei

# Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <-function(x = matrix())
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL
    }
    # Returns the matrix, not this object
    get <- function() x
    
    # Calls the solve function and sets the cached variable to the solution
    setInverse <- function(solve) m <<- solve
    
    # Returns the cached matrix 
    getInverse <- function() m
    
    list(set=set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then this function retrieves the inverse from the cache.
cacheSolve <- function(x,...)
{
    # Get the cached inverse matrix
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    
    # If the cached inverse matrix is null, get the actual data and
    # compute the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}