## Below are two functions.  The first function creates a matrix that 
## has the ability to cache its inverse.  
## The second function first checks to see whether or not the inverse 
## has already been calculated before proceeding.
## If the inverse has not been calculated, it calculates the 
## inverse, and then caches that calculation.  

## This function creates a matrix that can has the ability to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachemat <- NULL
  set <- function(tee) {
    x <<- tee
    cachemat <<- NULL
  }
  get <- function() x
  setmatrixval <- function(solve) cachemat <<- solve
  getmatrixval <- function() cachemat
  list(set = set, get = get, setmatrixval = setmatrixval, getmatrixval = getmatrixval)
}


## This function first checks to see whether or not the inverse has already been calculated before proceeding.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachemat <- x$getmatrixval()
  if (!is.null(cachemat)) {
    message("getting cached data")
    return(cachemat)
  }
  data <- x$get()
  cachemat <- solve(data, ...)
  x$setmatrixval(cachemat)
  cachemat
}
