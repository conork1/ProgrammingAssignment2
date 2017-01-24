## The following 2 functions provide a means for caching 
## the result of a matrix inversion. The first time a 
## matrix inversion is evaluated it is cached so that 
## the next time the same matrix inversion is evaluated
## it returns the value from the cache instead of having
## to perform the computation again. This is achieved 
## using 2 functions. The first function makeCacheMatrix
## creates a special version of the matrix you want to
## invert. It returns a list of functions and effectively
## caches the results in the parent Environment. The
## other function is called cacheSolve and it is passed
## the list returned by makeCacheMatrix and uses the 
## functions in it to check the cache to see if the 
## result is already there or else it performs the
## matrix inversion using the solve function and 
## stores the result in the cache.


## creates a special "matrix" which contains functions
## that can be used to get and set the value of the 
## matrix and get and set the inverted value of the 
## matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinversion <- function(solve) m <<- solve
  getmatrixinversion <- function() m
  list(set = set, get = get,
       setmatrixinversion = setmatrixinversion,
       getmatrixinversion = getmatrixinversion)
}


## returns the result of performing a matrix inversion
## on the special "matrix" object passed as an 
## argument. The function makeCacheMatrix should be 
## used to create this object. If the result of the
## matrix inversion is already in the cache then it
## returns it from the cache however if it is not in
## the cache it calls the solve function on it to get
## the matrix inversion and then puts the result into 
## the cache before returning it. The result returned 
## is a matrix itself.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinversion(m)
  m
}
