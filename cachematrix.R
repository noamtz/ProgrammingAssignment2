## The goal of this script is to demonstrate caching optimization.
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
##
## Usage:
## 1.create special "matrix" object with the function makeCacheMatrix
##    x  <- makeCacheMatrix(matrix(rnorm(1:16), 4, 4))
## 2.from here use the 'x' variable to compute inverse using the cacheSolve function
##    cacheSolve(x)
## 3.to update the matrix in variable x use the set method
##    x$set(matrix(rnorm(1:100), 10, 10))


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    ## Check if the new matrix is equal to the cached matrix
    ## if not cache the new matrix
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
  ## Return a matrix that is the inverse of matrix 'x'
  inv  <- x$getInverse()
  ## checking if the inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  data  <- x$get()
  ## calculate the inverse of the matrix
  m  <- solve(data, ...)
  ## cache the inverse of the matrix
  x$setInverse(m)
  ## return the inverse matrix
  m
}
