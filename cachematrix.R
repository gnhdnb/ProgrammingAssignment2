## This set of functions enables an option to inverse matrices with caching.

## Converts matrix to a custom object with an option to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
    )
}


## Inverses the matrix in the custom object from the function above. Returns cached result if possible.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
