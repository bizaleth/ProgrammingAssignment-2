## These functions create and store a matrix into the cache.
## This program also calculates the inverse of a square matrix.
## Since sometimes a matrix can be large and take a long time to solve, the program checks to see if the matrix has already
## been solved so that it doesn't have to do it twice.

## The makeCacheMatrix function creates a matrix which is stored to cache.  Once the matrix is solved, the solved matrix is also stored
## in the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## This function checks to see if the matrix has been stored to the cache.  If it has not been, it then solves for the matrix.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m

}
