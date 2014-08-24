## Matrix inversion is usually a costly computation and 
## their may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly 
## The following functions (makeCacheMatrix, and cacheSolve)
## are designed to cache the inverse of a matrix.


# makeCacheMatrix creates list containing a function to
# set the value of the matrix
# get the value of the matrix
# setinverse the value of the inverse
# getinverse the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    #Initialize the final output
    result <- NULL
    
    #Set the value of the matrix
    set <- function(y) {
        x <<- y
        result <<- NULL
    }
    #Get the value of the matrice
    get <- function() x
    
    #Set the value of the matrix inverse
    setinverse <- function(inverse) result <<- inverse
    
    #Get the value of the matrix inverse
    getinverse <- function() result
    
    #Show the available methods when called without argument
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

# Hypothesis: the input matrix is always inversible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    #Get the availble output from getmean
    result <- x$getinverse()
    
    #If the result is not NULL, return the result (thus exiting the function)
    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }
    #Get the matrix to inverse
    data <- x$get()
    
    #Calculate the inverse of this matrix
    result <- solve(data)
    
    #Then set the result
    x$setinverse(result)
    
    #And return it ! Voilà!! :)
    result
}



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
