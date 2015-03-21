## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function sets up the initial matrix and the methods for 
## computing , fetching , and setting the inverse of the matrix
## the code is copied from the example for the cached 'mean' of 
## a numerical vector
## if the inverse matrix is cached then that value is used.
## Per the instuctions and information in the coursera forum:
## calls to the mean function are replaced with calls to the solve function
## the dummy variable m is initialized to NULL so we know there is no cached value now 
## the <<- stores the result to the parent environment
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                x <<- y 	
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)


}


## Write a short comment describing this function
## this function uses the information created in the call to makeCacheMatrix 
## if m is Null then the inverse matrix is no yet known; it forces the calculation of the inverse
## if m is NOT NULL then it returns the value already cached in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
