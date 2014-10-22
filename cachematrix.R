## These functions permit you to create a matrix object, compute its inverse,
## and store (cache) the inverse in order to avoid having to do a potentially
## costly matrix inversion computation each time.
## 
## Assumptions for usage: matrices are square and invertible
##

## 'makeCacheMatrix' creates a special matrix object using the superassignment
## operator such that it can cache its inverse (using the second function
## 'cacheSolve' below)

makeCacheMatrix <- function(x = matrix()) {
 
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(myMatrix) m <<- myMatrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  
}


## 'cacheSolve' computes the inverse of the matrix object created by
## 'makeCacheMatrix' and checks to see if inverse was already cached before
## doing so.  If so, it retrieves the cached values instead of computing the
## inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m  
  
}
