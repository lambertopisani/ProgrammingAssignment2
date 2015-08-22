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
  ## the following lines define the functions to be used by cacheSolve to either set or get either the (original) matrix or its inverse.
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
  #CHECKS IF THE INVERSE HAS ALREADY BEEN CALCULATED IN THE PAST (I.E, PRESENT IN THE CACHE)
  m <- x$getinv()
  if(!is.null(m)) { ## IF THE MATRIX IS PRESENT THEN IT TAKES THE INFO FROM TEH CACHE AND EXIT THE FUNCTION RETURNING THE VALUE IT FOUND
    message("getting cached data for inverse matrix")
    return(m)
  }
  ## IF THE DATA IS NOT IN TE CACHE, IT GETS THE ORIGINAL MATRIX'S VALUE AND THEN CALCULATES THE INVERSE OF IT.
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m) # WRITES BACK THE RESULT IN THE SPECIAL VECTOR FROM MAKEVECTOR SO IT CAN BE USED LATER ONE AS A CACHE
  m
}
