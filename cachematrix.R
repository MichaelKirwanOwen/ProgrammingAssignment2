## Put comments here that give an overall description of what your
## functions do

## This function takes an input matrix and stores it in cache

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)

}


## This function solves the inverse of the matrix and also stores
## the inverse in cache if the matrix is not new

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

