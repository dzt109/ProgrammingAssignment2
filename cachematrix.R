## Solution to Programming Assignment 2

## Make a Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinv <- function(solve) INV <<- solve
  getinv <- function() INV
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Invert a cache matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getinv()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setinv(INV)
  INV
}
