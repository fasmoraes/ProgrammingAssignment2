## Motivation
## ==========
##   Matrix inversion is usually a costly computation and their may be some 
##   benefit to caching the inverse of a matrix rather than compute it 
##   repeatedly. This pair of functions can calculate and cache the 
##   inverse of a matrix.

## Testing:
## ========
##   ## Create a 3x3 matrix with random numbers 
##   origMatrix <- matrix(rnorm(9), nrow = 3, ncol = 3)
##
##   ## Create a special "matrix" object that can cache its inverse
##   cMatrix <- makeCacheMatrix(origMatrix)
##
##   ## Calculate its inverse
##   cacheSolve(cMatrix)
##
##   ## Again, calculate its inverse (should get from cache)
##   cacheSolve(cMatrix)


makeCacheMatrix <- function(x = matrix()) {
# Creates a special "matrix" object that can cache its inverse.
#
# Args:
#   x: An invertible matrix
#
# Returns:
#   A list of functions that may be applied to 'x'.

  # Initialize the inverse matrix
  inverseMatrix = NULL  
  
  Get <- function() {
    # Returns:
    #   The original matrix
    x
  }
  
  GetInverse <- function() {    
    # Returns:
    #   The inverse matrix
    inverseMatrix
  }
  
  Set <- function(y) {    
    # Sets the matrix and initialize the inverse matrix
    #
    # Args:
    #   y: A matrix to replace the original
    x <<- y
    inverseMatrix <<- NULL
  }
  
  SetInverse <- function(newInverse) {
    # Sets the inverse matrix.
    #
    # Args:
    #   newInverse: The new inverse matrix    
    inverseMatrix <<- newInverse    
  }
    
  # Function returns a set of operations
  list(Set = Set, 
       Get = Get, 
       SetInverse = SetInverse, 
       GetInverse = GetInverse)  
}


cacheSolve <- function(x, ...) {
# Check if the inverse of x is in cache. If so, return it.
#   Otherwise, calculate the inverse and update the cache
#
# Args:
#   x: An invertible matrix
#
# Returns:
#   A matrix that is the inverse of 'x' 
  
  # Try to get the cached inverse of 'x'
  iMatrix <- x$GetInverse()
  
  # If found, return it
  if(!is.null(iMatrix)) {
    message("Getting cached data...")
    return(iMatrix)
  }
  
  # Otherwise, caculate the inverse
  iMatrix <- solve(x$Get(), ...)
  
  # Set the inverse in cache
  x$SetInverse(iMatrix)
  
  # Return the inverse
  iMatrix    
}
