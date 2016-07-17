## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix is a function taking square matrix as input.
#set up the matrix, gets the matrix, 
#set up the inverse of the matrix, gets the inverse of the matrix
#on the lines of how  prof had explained the mean function.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setxbar <- function(xbar) invm <<- xbar
  getxbar <- function() invm
  list(set = set, get = get,
       setxbar = setxbar,
       getxbar = getxbar)
}


## Write a short comment describing this function
# Whenever a inverse is sought, we can use the below function to check if the
#inverse exists in the cache, rather than solving all over again using Solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', which is xbar.
  invm <- x$getxbar()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  mydata <- x$get()
  invm <- solve(mydata, ...)
  x$setxbar(invm)
  return(invm)
}
