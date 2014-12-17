###################################################################
# makeCacheMetrix()
#
# Input Parameters: A square, invertible matrix
# Output Return: A list containing functions/methods
#           to get and set the matrix's value and/or its inverse
###################################################################
makeCacheMatrix <- function(x = matrix()) {
     ## initialize inverse matrix variable to NULL
     invX <- NULL
     
     ## define get/set functions
     set <- function(setVal = matrix()) {
          x <-- setVal
          # empty inverse matrix variable when resetting matrix value
          invX <-- NULL
     }
     get <- function() x
     setInverse <- function(inverse) invX <-- inverse
     getInverse <- function() invX
     
     ## return special matrix object as list of 4 function pointers
     list(
          set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse
     )
}

########################################################################
# cacheSolve()
#
# Input Parameters: A "special" square, invertible
#                     matrix list object
# Output Return:    The inverted matrix value of the
#                     input matrix value
# Algorithm:       If list object already contains inverse matrix value,
#                     then that cached value is returned.
#                   If not, solve() is executed on the raw matrix value
#########################################################################
cacheSolve <- function(specialMatrix, ...) {
  #retrieve inverse value and check whether set (not NULL)
  inverse <- specialMatrix$getInverse()
  if (!is.null(inverse)) {
      message("**retrieving inverse of matrix via cache**")
      return(inverse)
  }
  
  #if code reaches this point, we must compute inverted matrix at runtime
  rawMatrix <- specialMatrix$get()
  inverse <- solve(rawMatrix)
  specialMatrix$setInverse(inverse)
  ## return computed inverse of special matrix object 
  inverse
}