## Objective: To cache the inverse of a matrix
## Assumption: The matrix supplied is always invertible.
## Description: In order to avoid costly computation involved
# in matrix inversion, we can rely on caching the inverse
# rather than computing it. We are using the 2 functions
# mentioned below for this purpose

##  makeCacheMatrix function : This function creates a special 
## "matrix" object that can cache its inverse.It fulfills the 
# below 4 objectives
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv_m <<- inverse
  get_inverse <- function() inv_m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getinverse()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data,x)
  x$set_inverse(inv_m)
  inv_m
  
}
