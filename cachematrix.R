## Below are two functions that are used to create a special object 
## that stores a numeric matrix and cache's its inverse matrix

## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse matrix
##      get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv1) i <<- inv1
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve  retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
B <- makeCacheMatrix(A)
cacheSolve(B)

