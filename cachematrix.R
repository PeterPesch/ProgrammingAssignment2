##  cachematrix
##  This file contains 2 functions to cache the inverse of a matrix:
##
##  makeCacheMatrix(x)              Creates a cache for a matrix and its inverse
##  cacheSolve <- function(x, ...)  Returns the inverse of the cached matrix
##
##  Peter Pesch, 3 nov 2016, for a programming assignment by Roger Peng


##  makeCacheMatrix(x)
##      Creates a cache which can hold a matrix and (optionaly) its reverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x'  (Optional) Matrix which will be stored initially in the cache.
    
    inv <- NULL
    
    # Define setter and getter for the matrix;
    # Please note that the setter also clears 'inv'
    set <- function(m) {
        inv <<- NULL
        x <<- m
    }
    get <- function() {return(x)}

    # Define setter and getter for the inverse.
    setinverse <- function(inverse) {inv <<- inverse}
    getinverse <- function() {return(inv)}
    
    # Return the setter and getter for the matrix and its inverse:
    return(list(set=set, 
                get=get,
                setinverse=setinverse,
                getinverse=getinverse))
}


##  cacheSolve <- function(x, ...)
##      Returns the the inverse of the cached matrix.
##      Tries to use the cached inverse.

cacheSolve <- function(x, ...) {
    ## 'x'  CacheMatrix which contains the matrix
    ## ...  Rest of the arguments are forwarded to solve()
    ##
    ## Returns the inverse of the matrix
    
    # Try to retrieve the inverse from the cache
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute the inverse of the matrix
    mat <- x$get()          # Retrieve the matrix from the cache
    inv <- solve(mat, ...)  # Compute the inverse of the matrix
    x$setinverse(inv)       # Store the inverse in the cache
    return(inv)             # Return the inverse
}
