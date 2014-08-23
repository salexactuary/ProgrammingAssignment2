##The following functions calculate and cache the inverse of a matrix
##such that the inverse need only be calculated one time. Following is a 
##simple example:
##a <- matrix(c(1,5,6,3,4,1,5,7,2),3,3)
##b <- makeCacheMatrix(a)
##cacheSolve(b)
##     [,1]  [,2] [,3]
##[1,]  0.5  -0.5  0.5
##[2,] 16.0 -14.0  9.0
##[3,] -9.5   8.5 -5.5


##This function creates a special "matrix", 
##which is really a list containing a function to:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix
##     get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) v <<- solve
        getsolve <- function() v
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##This function calculates the inverse of the special "matrix" 
##created with the above function. If the inverse has already 
##been calculated (and the matrix has not changed), 
##then cacheSolve retrieves the inverse from the cache.
##Otherwise, it calculates the inverse of the matrix, 
##which must be square and invertible, and sets the value of the 
##inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        v <- x$getsolve()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setsolve(v)
        v
}

