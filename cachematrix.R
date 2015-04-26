## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if(!identical(x, y)) {
        x <<- y
        m <<- NULL
    }
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function()  m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv  <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  data  <- x$get()
  m  <- solve(data, ...)
  x$setInverse(m)
  m
}
