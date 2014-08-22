## The functions in this file enable storage of (invertible) matrices and
## computation of their inverses.  Once computed, an inverse is cached for
## its next use.

## create a matrix and associated access functions
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   #initialize inverse value to NULL
  
  # store matrix value and initialize inverse value to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get matrix value
  get <- function() x
  
  # store inverse value
  setinv <- function(inverse_value) inv <<- inverse_value
  
  # get inverse value
  getinv <- function() inv
  
  # return list of access functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## return inverse of a matrix, from stored (cached) value if possible,
## otherwise compute inverse and cache value for next access
cacheSolve <- function(x, ...) {
  inv <- x$getinv()   ## get saved inverse value
  if (!is.null(inv)) return(inv)  ## done if inv != NULL
  
  data <- x$get()   ## retrieve matrix x
  inv <- solve(data, ...)  ## compute inverse
  x$setinv(inv)   ## save inv in cache
  inv   ## return inv
}
