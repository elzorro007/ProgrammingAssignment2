## This package defines 2 functions to handle the inversion of a matrix
## If the inverse has been already stored, the system will retrieve it from the cache


## Example of an invertible matrix to test: matrix(c(c(1,2,1),c(0,-1,4),c(1,3,2)),3,3)

## makeCacheMatrix is defining a list containing a function to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve is calling the subfunction $getinv
##   if the result is null, then it calculate the inverse of the matrix (solve())
##   otherwise it retrieve it from the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv     
}
