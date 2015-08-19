## Objective: To cache the inverse of a matrix
## Assumption: The matrix supplied is always invertible.

##  makeCacheMatrix function : This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) m <<- solve()
  get_inverse <- function() m
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
  m <- solve(x,m)
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get_inverse()
  m <- solve(data,x)
  x$set_inverse(m)
  m
  
}
