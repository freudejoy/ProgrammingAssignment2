## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The followings are a pair of functions will first create a cache matrix object, then calculate its inverstion with cacheing ability.

## Name: makeCacheMatrix
## Argument: x an matrix object
## Description: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  ##initiate a vector 's' for storing cahced inverted matrix
        
        ## An internal function (method) to assign different matrix value to the "makeCacheMatrix" object 
        ##  that already being created
        set <- function(y) { 
                x <<- y
                s <<- NULL
        }
        get <- function() x ##to get the matrix value of the object created by calling "makeCacheMatrix"
        setsolve <- function(solve) s <<- solve  ##Invert the matrix in the argument (x)
        getsolve <- function() s  ##Return an inverted matrix value of x 
        
        ##accessed every time makeCacheMatrix object is created, so it can access the internal methods
        list(set = set, get = get,      
             setsolve = setsolve,                             
             getsolve = getsolve)

}


## Name: cacheSolve
## Arguments: x an makeCacheMatrix object
##            ... further arguments passed to or from other methods.
## Description: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##              If the inverse has already been calculated (and the matrix has not changed), 
##              then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
         s <- x$getsolve()   ## accesses the object 'x' and gets inverted matrix
        if(!is.null(s)) {     ## if the inverted matrix was already cached (not NULL) ...
                message("getting cached data")
                return(s)
        }
        data <- x$get()  ## we reach this code only if x$getsolve() returned NULL
        s <- solve(data, ...)  ## if m was NULL then we have to calculate thr inverted matrix
        x$setsolve(s)  # store the calculated inverted matrix in x 
        s   # return the inverted matrix to the caller
}
