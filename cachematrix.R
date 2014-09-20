## This code is a minor modification of the example code from the course.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If inverse already exists, cacheSolve can retrieve the
## inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        ## makeCacheMatrix returns a list containing a function to
        ## set the value of the matrix
        ## get the value of the matrix
        ## set the inverse of the matrix
        ## get the inverse of the matrix
        
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function (solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix 's' that is the inverse of 'x'
        ## If the inverse already exists,it gets the inverse from the cache 
        ## and skips the inversion. Otherwise, it solves the inverse of the data
        ## and sets  the inverse in the cache via the setSolve function.
        
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
}

## Example usage

x1 <- matrix(c(1:4),2,2)
m1 <- makeCacheMatrix(x1)
cachesolve(m1)
x2 <- matrix(c(2:5),2,2)
m2 <- makeCacheMatrix(x2)
cachesolve(m2)

cachesolve(m1)
cachesolve(m2)
