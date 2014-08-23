##  A pair of functions that calculate and cache the inverse of a matrix.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # clear cache
  m <- NULL
  # function to set the matrix and erase the cache 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # function to get the matrix 
  get <- function() x  
  # function to set the inverse of a matrix in the cache 
  setinv <- function(inv) m <<- inv
  # function to get the inverse of a matrix from the cache 
  getinv <- function() m
  # return a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get de inverse of the matrix if exists on cache
  m <- x$getinv()
  ## If exists, return it from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Else, get the matrix
  data <- x$get()
  ## Calculate the inverse
  m <- solve(data, ...)
  ## Put the result in the cache
  x$setinv(m)
  ## return the inverse matrix
  m
}
