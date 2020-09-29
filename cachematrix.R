## makeCacheMatrix creates an invertable matrix
## and sets up a persistent environment for
## the variable values.
## cacheSolve creates an inverted matrix.
## Use:
##  Create an invertable matrix:
##    a <- matrix(c(1,2,3,4),2,2)
##  Instantiate the function:
##    a1 <- makeCacheMatrix(a)
##  Inspect matrix a
##    a
##  Invert matrix a
##    cacheSolve(a1)
##  Run cacheSolve(a1) a second time
##  to retrieve to get the cached value.
##
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


