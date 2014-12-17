## These functions allow a user to solve for the inverse of a matrix and cache 
## the result, to be retrieved as needed, so that it doesn't need to be calculated
## anew every time it is needed.

## 'makeCacheMatrix' saves a matrix as a list of functions that allows for
## calculating the inverse of the matrix and caching this result to be retrieved
## as needed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of the matrix stored by `makeCacheMatrix`, caches
## that inverse, retrieves it if called again

cacheSolve <- function(x, ...) {
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
