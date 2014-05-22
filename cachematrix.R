## The following functions are used to cache a matrix and
## solve a matrix. The solve matrix function first
## identifies if the matrix has been cached before solving


## This function will create a special "matrix" object that can
## cache it's inverse
## This function generates a list that has functions for
## getting and setting both the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    y <<- x
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



## This function computes the inverse of the matrix from makeCacheMatrix
## If the matrix has been calculated already, this function 
## retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <-- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
