# The Function has been taken from makeVector and adapted for this assignment

makeCacheMatrix <- function(x = matrix()) {
    # m (the later inverse of x) is default to NULL
    m <- NULL
    
    # define "set" as a function and assign x (scope of makeCacheMatrix) the value of y. Everytime you call this function m (scope of MakeCacheMatrix) will be set to NULL again
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get just returns the value of x
    get <- function() x
    
    # This will calculate the inverse of the matrix and store it in m (scope of MakeCacheMatrix)
    setinverse <- function(solve) m <<- solve
    
    # Return the value of m (scope of makeCacheMatrix)
    getinverse <- function() m
    
    # This is the returnvalue, a list containing functions to get or set the values
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The Function has been taken from cachemean and adapted for this assignment

cacheSolve <- function(x, ...) {
    # Try to load the Inverse of x
    m <- x$getinverse()
    
    # When m is NULL, the inverse has not been calculated, if !NULL just return m (the inverse of x) and finish
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If m is NULL get the value of x (the one who needs to be inversed) and store it in data
    data <- x$get()
    
    #following message should be uncommented for debugging
    # message("not cached")
    
    # m will be calculated as the inverse of x
    m <- solve(data, ...)
    
    # Cache the inverse for further access
    x$setinverse(m)
    
    # return the inverse of x
    m
}

# a few commands to test the functions
# z <- matrix(1:4,2,2)
# a <- makeCacheMatrix(z)
# cacheSolve(a)
# cacheSolve(a)