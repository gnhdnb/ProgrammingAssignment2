## This set of functions enables an option to inverse matrices with caching.

## Converts matrix to a custom object with an option to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(
    set = set, 
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
    )
}


## Inverses the matrix in the custom object from the function above. Returns cached result if possible.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
