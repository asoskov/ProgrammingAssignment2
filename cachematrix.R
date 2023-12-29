## Both functions together provide a caching mechanism for the inverse matrix,
## which can be useful when you are working with the same matrix multiple times
## and you want to avoid computing the inverse matrix multiple times.


## This function creates and returns a special "matrix" object that is designed
## to cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse matrix for the "matrix" object created by
## the "makeCacheMatrix" function. If the inverse matrix has already been
## calculated and cached, the function retrieves it from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

