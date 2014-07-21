# Functions to speed up repeated inversions of a given matrix.
#
# makeCacheMatrix: create a special matrix that can cache its inverse
# cacheSolve: returns inverse of a CacheMatrix (using cache if possible)

makeCacheMatrix <- function(x = matrix()) {
  # Returns a matrix object that caches its inverse.
  #
  # Args:
  #   x: The source matrix
  #
  # Returns:
  #    A list of 4 functions:
  #      set(y): sets the original matrix
  #      get():  gets the original matrix
  #      setInverse(inverse): sets the inverse of the matrix.
  #      getInverse(): gets the inverse of the matrix.
  #
  # The returned list operates as a matrix with cached inverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  #
  # Args:
  #   x: a CacheMatrix list
  #
  # Returns:
  #   The matrix inverse of x
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}