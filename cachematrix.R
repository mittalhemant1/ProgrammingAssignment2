## Firsr Function creates a structure to set a matrix and store its inverse locally
## while second function access the cached inverse if available, otherwise calculates the inverse and store the same in the cache.

## makeCacheMatrix  creates a special matrix , which is really a list that contains a function to
## set the value of matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix
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


## cacheSolve calculates the inverse of the matrix. if already been calculated it gets the same and skips the calculation
## Otherwise it calculates the inverse, store the value in cache and returns the same.
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
