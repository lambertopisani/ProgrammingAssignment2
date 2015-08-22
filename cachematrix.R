## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, is grounded on the same logic as the Mean Cache creation for a vector to which Mean will be calculated
## the input will change to a MAtrix type of variable, and I renamed the generated functions to align with the inverse exercise 
##
##

## For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inver) m <<- inver
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function, is grounded on the same logic as the Mean Cache calculation of the example.
## the input will still be a "special" vector (list of four items), and I renamed the generated functions to align with the inverse exercise 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data for inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
