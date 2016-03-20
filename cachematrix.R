## The following two functions are used to cache a matrix's inverse
## for better performance.

## makeCacheMatrix holds a matrix in an encapsulated environment (the function
## scope). It offers functions to get and set both the matrix itself and it's
## inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## If a new matrix is set, always reset the inverse.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function takes a makeCacheMatrix list object.
## It first checks if the inverse is already set on the makeCacheMatrix object.
## If it is, it returns the inverse matrix and skips the rest of the function.
## If it's not yet calculated (and therefore NULL), the inverse gets calculated
## by taking the original matrix and running solve() on it. The inverse is then
## set on the makeCacheMatrix object for future get() calls.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("returning cached matrix inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
