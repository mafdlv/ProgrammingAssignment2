## Matrix inversion is usually a costly computation, and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatr <- function(inversematr) m <<- inversematr
  getInvMatr <- function() m
  list(set = set, get = get,
       setInvMatr = setInvMatr,
       getInvMatr = getInvMatr)
}
  else{
  return(message("The matrix cannot be inverted."))
  }
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, given the matrix unchanged, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatr()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatr(m)
  m
}
