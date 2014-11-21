## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  ##initiate a vector 's' for storing cahced inverted matrix
        
        ## a function to assign different matrix value to the matrix object that already created
        set <- function(y) { 
                x <<- y
                s <<- NULL
        }
        get <- function() x ##to get the matrix value of the object created by calling "makeCacheMatrix"
        setsolve <- function(solve) s <<- solve  ##invert the matrix in the argument (x)
        getsolve <- function() s  ##get inverted matrix value of x the object created by calling "makeCacheMatrix
        
        ##accessed every time makeCacheMatrix object is created, so it can access the internal methods
        list(set = set, get = get,      
             setsolve = setsolve,                             
             getsolve = getsolve)

}


## Write a short comment describing this function

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
