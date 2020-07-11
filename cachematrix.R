## These functions together can calculate the inverse of a matrix
## and cache its value instead of repeating the calculation 

## This function caches the value of the inverse matrix 
## obtained from the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list( set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## This function solves for the inverse of a matrix 
##but it first checks for a cached value. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}
