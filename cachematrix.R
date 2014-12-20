## 
# Combination of functions that provide caching strategy for obtaining
# the results of 'solve' to a specified Vector.
#
# Usage:
# makeCacheMatrix - use this function to create a special object
#                   that encapsulates a specified matrix and a cached
#                   result.
# cacheSolve -      this provides the 'solve' restuls for the specified
#                   matrix - providing the cached value if executed twice.

# Creates an object that encapsulates a cached result for
#  calculating the inverse of a matrix.
#
# Args:
#   x: Specified matrix that the inverse function will be applied to.
#
# Returns:
#   An object encapsulating the cached resulting value that has the
#   following functions:
#     result$get - Get the specified matrix value
#     result$set - Set the specified amatrix value
#     result$getinverse - The cached value or NULL
#     result$setinverse - Set the cached value
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       getsolve = getsolve,
       setsolve = setsolve)
}


# Calculates the mean value using the 'mean' value of the value specified
# in the specified 'special vector'.  Returns the cache value if there is
# one.
#
# Args:
#   x - 'special object' returned by the 'makeCacheMatrix'.
#
# Returns:
#   An object encapsulating the cached resulting value that has the
#   following functions:
#     result$get - Get the specified matrix value
#     result$set - Set the specified amatrix value
#     result$getinverse - The cached value or NULL
#     result$setinverse - Set the cached value
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
