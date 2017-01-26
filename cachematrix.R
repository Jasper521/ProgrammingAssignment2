## These functions can used to input a matrix and display the matrix itself and its inverse
## makeCacheMatrix generates a list of functions to set the values and display the values
## CacheSolve is used to solve the inverse of the input matrix x

## This generates the matrix x and can be used to set x, set the inverse of x, display x and inverse of x

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function is used to test the elements from m and calculate the inverse of matrix x

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
