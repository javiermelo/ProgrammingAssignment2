#################################################################################
##
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##
## Methods: 
##       set: sets the original matrix
##       get: access the variable that stores the original matrix
##       setinverse:  sets the inverted matrix in  the parent contents variable 
##       getinverse:  access the variable that stores the inverted matrix
##
#################################################################################
#################################################################################
##
## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix function. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
##
## Args: x is an instance of makeCacheMatrix
##
#################################################################################

## Creates special matrix object to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Initialize the variable for the inverse matrix
        set <- function(y) {
                x <<- y   ## x Stores the original matrix in the parent environment variable 
                i <<- NULL ## i Stores the inverse matrix in the parent environment variable
        }
        get <- function() x  ## returns the original matrix
        setinverse <- function(inverse) i <<- inverse  ## assigns the inverse matrix to parent 
        getinverse <- function() i  ## returns the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  ## returns the list of methods
}


## Returns a the inverse matrix of x, being x an instance of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()  ## gets the inverse matrix if it was calculated, otherwise NULL
        if(!is.null(i)) {
                message("getting cached data")
                return(i)  ## returns the cached inverse matrix without recalculating
        }
        data <- x$get()  ## gets the original matrix into data variable
        i <- solve(data, ...) ## calculates the inverse of matrix data into i
        x$setinverse(i) ## calls setinverse
        i ## returns the inverse matrix calculated
}
