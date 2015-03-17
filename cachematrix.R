## Creates a special matrix type that can cache the inverse 
## computation of matrix and can return the cached value instead
## of computing again

## Creates a special "matrix" that can cache inverse matrix.
## It contains list of functions to get and set the matrix and 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## This function calculates the inverse of the special "matrix".
## it uses the cached matrix if it has already been calculated.
## Otherwise, it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
