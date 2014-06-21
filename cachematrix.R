## R programming assignment #2: lexical scoping by jsy1979

## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. 
## The following functions will cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    dim_x <- dim(x$get())
    if (dim_x[1] != dim_x[2]) {
        message("Row and column of your input matrix is different!")
        return()
    }
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m        
}
