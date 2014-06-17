## Calculating the inverse could be a very time-consuming computation.
## These two functions work together in order to cache the inverse matrix
## and re-use the cache in subsequent calls instead of recalculating
## it each and every time

## See bottom of code for an example on how to use the functions...

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the inverse matrix
# 4.  get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                         ## Clears the matrix inv
      set <- function(y) {
            x <<- y
            inv <<- NULL                  ## when a new matrix is set it clears the cached inverse matrix
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix or if the inverse 
## has been calculated before, will skip the calculation and just return the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("retrieving inverse from cache")
            return(inv)                         ## Returns the cached inverse matrix and exits the function
      }
      data <- x$get()
      inv <- solve(data, ...)  ## Calculates the inverse matrix
      x$setinv(inv)  ## stores the inverse matrix in the cache
      inv
}


## Example showing how to use the two functions: 

# Everything on ident is code that you can run in the console

##----------------------------------------------

#Call  the makeCacheMatrix() function and assign it's
#  return value ( a list of four functions) to a variable, z
#  z is now a list of four functions

#           z <- makeCacheMatrix()

#use z's set function to create a matrix 
#  containing the numbers 1 through to 4 in a 2x2 matrix
#           z$set(matrix(1:4,2,2))

#use z's get function to retrieve the vector created 
#           z$get()

#pass the list z to the cacheSolve() function
#   the inverse of the matrix should be returned
#           cacheSolve(z)

#pass the list z to the cacheSolve() function a second time
#  the inverse of the matrix should be returned
#  also a message "retrieving inverse from cache" indicating that the inverse
#  is not being calculated this time but is being retrieved from the cached
#  value
#           cacheSolve(z)

