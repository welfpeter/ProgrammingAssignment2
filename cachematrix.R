## This file contains two functions. 
## The calling argument is a matrix for which the inverse should be calculated.
## 

## makeCacheMatrix stores a list of functions

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL ##when the main function is called, set the inverse to Null 
        
        ##set changes the Matrix stored in the main function and sets the inverse to NULL
        set <- function(y) {
                x <<- y          ##changes the matrix in the main function
                inverse <<- NULL ##restores the inverse to NULL
        }
        
        ## function that returns the Matrix stored in the main function
        get <- function() x
        
        ## setinverse and getinverse are similar to set and get function
        ## they store and return the inverse value
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        
        ## the four functions are being stored in a list so that when makeCacheMatrix
        ## is assigned to an object, the 4 functions are available
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the Matrix and stores it to the cache using
## setinverse. If the Matrix has not been changed and the inverse has already been
## calculated, the inverse is called from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        ## if inverse is not Null a inverse of the matrix already exists and will be plotted
        if (!is.null(inverse)){
                message("getting cache data")
                return(inverse)
        }
        ## if there is no inverse of the matrix is in the cache it is beeing claculated
        data <- x$get()               ## get the matrix from makeCacheMatrix function
        inverse <- solve(data,...)    ## inverts the matrix
        x$setinverse(inverse)         ## stores the inverse to the makeCacheMatrix function
        inverse                       ## plot the inverse
}
