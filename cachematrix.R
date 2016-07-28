## `makeCacheMatrix` creates a special matrix that can be cached using
## the function `cacheSolve`. `cacheSolve` returns the inverse of the
## matrix. If the inverse already has been cached, it won't be 
## calculated again.


## Cache and fetch cached matrixes
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of a matrix from the cache 
## otherwise calculate the inverse and cache it.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
