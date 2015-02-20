## Helper function to cache a inverse of a matrix

## Create a function to control a matrix x.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s<<- solve
        getsolve <- function() s
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function return a inverse of matrix X. 

cacheSolve <- function(x, ...) {
        
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
