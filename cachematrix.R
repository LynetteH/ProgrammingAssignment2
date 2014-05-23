## These functions below create a special matrix (makeCacheMatrix) which takes
## a matrix as an argument and can cache the inverse of the matrix
## The function cacheSolve takes as its argument the special matrix produced by 
## makeCacheMatrix and checks to see if the inverse has been cached (in which case
## it returns the cached value; otherwise it calculates the inverse and returns it
## and also caches it.


## makeCacheMatrix creates a list consisting of a matrix plus functions that can
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the matrix inverse
##      4. get the value of the matrix inverse

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

## cacheSolve first checks to see if the inverse of the input matrix has been 
## calculated; if yes, it returns that value; otherwise it calculates
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Check to see if inverse has been cached
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise load the matrix and invert it
        data <- x$get()
        m <- solve(data,...)
        message("solving for m")
        ## call setinv to cache the results
        x$setinv(m)
        ## Return the inverted matrix
        m
}
