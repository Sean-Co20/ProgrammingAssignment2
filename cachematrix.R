## Put comments here that give an overall description of what your
## functions do

##Function 1: makeCacheMatrix(x)
##This function sets the value of the matrix using the double arrows to modify parent levels
##This function gets the value of the matrix
##This function sets the value of the inverse (uses double arrows again)
##This function sets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverseValue <- NULL
  set <- function(y) { 
    x <<- y
    inverseValue <<- NULL
  }
  get <- function() {x} 
  setInver <- function(inverse) {inverseValue <<- inverse}
  getInver <- function() {inverseValue}
  list(set = set, get = get, setInver = setInver, getInver = getInver)
}


##cacheSolve(x,...)
##This function will be evaluate the inverse of the matrix dveloped previously

cacheSolve <- function(x, ...) {
  inverseValue <- x$getinv()
  if(!is.null(inverseValue)) { 
    message("getting cached data")
    return(inverseValue)
  }
  outcome <- x$get()
  inverseValue <- solve(outcome, ...)
  x$setinv(inverseValue)
  inverseValue
        ## Return a matrix that is the inverse of 'x'
}
