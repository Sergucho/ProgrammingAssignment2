## Below are two functions that are used to create a special object that stores a numeric matrix and cache's inversion matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set(matrix) sets the value of the matrix
## get() gets the value of the matrix
## setinvmtrx(matrix) sets the value of the inversion matrix
## getinvmtrx() gets the value of the inversion matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmtrx <- function(invmtrx) m <<- invmtrx
  getinvmtrx <- function() m
  list(set = set, get = get,
       setinvmtrx = setinvmtrx,
       getinvmtrx = getinvmtrx)
}


# The following function calculates the inversion matrix of the special "matrix" created with the above function. However, it first checks to see if the inversion matrix has already been calculated. If so, it gets the inversion matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmtrx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmtrx(m)
  m
}
