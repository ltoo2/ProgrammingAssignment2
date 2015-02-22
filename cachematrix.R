## This script provides two functions that work in tandem to invert a matrix in
## a computationally efficient way, if the inversion is likely to be repeated

## The makeCacheMatrix() function creates a list of 4 functions to set and get
## the matrix and set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve() function uses the list produced by the makeCacheMatrix()
## function to determine if the inverse has been already calculated (and then
## it returns it) or ot calculate it, if it is the first time

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
