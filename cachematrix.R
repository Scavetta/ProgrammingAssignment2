## These are two function which:
## (1) caches the inverse of a matrix and 
## (2) returns the cached inverse if it available, or else calculates and caches new inverse.

## This funtion takes a matrix as input and makes an object which is a list of four functions:
# set: Sets the value of a matrix
# get: Gets the value of a matrix
# setinverse: Calculates the inverse of a matrix and saves it.
# getinverse: Gets the saved inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## When the cacheSolve function is called, it computes the inverse of the 
## special "matrix" returned by makeCacheMatrix if the inverse has 
## already been calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {          
  m <- x$getinverse()       #Check to see if the inverse for this matrix has already been calculated.
  if(!is.null(m)) {         # If it has been calculated..
    message("getting cached inverse of the matrix")
    return(m)               # It provides this as output
  }
  data <- x$get()           # or else it gets the matrix from the makeCacheMatrix object
  m <- solve(data, ...)     # and calcuates the invers itself...
  x$setinverse(m)           # ...sets the function in the the object for future use, and
  m                         # returns the invers
}
